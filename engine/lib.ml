open Astring
module R = Rresult.R

type run_parameters =
  { repo_dir : string
  ; nocache : bool
  ; deploy : bool
  ; target_nodes : string list
  ; dont_delete : string list
  ; aws_profile : string option
  ; guid : Uuidm.t }

let make_params ?aws_profile ~repo_dir ~nocache ~deploy ~target_nodes ~dont_delete () =
  let guid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  { repo_dir; nocache; deploy; target_nodes; guid; dont_delete; aws_profile }

let sprintf = Printf.sprintf

let get_assoc_list x =
  match x with
  | `O y ->
      R.ok y
  | _ ->
      R.error (R.msg "node must have list of attributes below it.")

let get_array x =
  match snd x with
  | `A y ->
      R.ok y
  | _ ->
      R.error (R.msg "tried to access an array that wasn't actually an array.")

let get_value assoc_list key =
  let results = List.filter (fun x -> fst x = key) assoc_list in
  match results with
  | hd :: _ ->
      R.ok hd
  | _ ->
      R.error (R.msg ("key " ^ key ^ " isn't found in the assoc list"))

let get_string str =
  match str with
  | `String x ->
      R.ok (String.trim x)
  | _ ->
      R.error (R.msg "not a string")

let get_string_from_attrib_list assoc_list key =
  let bind = R.bind in
  let%bind pair = get_value assoc_list key in
  get_string (snd pair)

let result_fold fn acc input_list =
  List.fold_left
    (fun a n ->
      match a with
      | Error _ as e ->
          e
      | Ok l -> (
        match fn n with Error _ as e -> e | Ok item -> Ok (item :: l) ))
    (Ok acc) input_list

let is_dir path =
  let%lwt stats = Lwt_unix.lstat path in
  Lwt.return (stats.st_kind = Lwt_unix.S_DIR)

(* grabbed from SO *)
let get_or_timeout ?(max_request_duration = max_float) uri =
  let open Lwt.Infix in
  let timeout =
    Lwt_unix.sleep max_request_duration
    >>= fun () -> Lwt.return_error `Timed_out
  in
  let get =
    Cohttp_lwt_unix.Client.get uri >>= fun response -> Lwt.return_ok response
  in
  Lwt.pick [timeout; get]

let rec repeat_until_some f c =
  match c with
  | 0 ->
      Lwt.return
        (R.error_msg "Couldn't get a successful call for repeat_until_some")
  | _ -> (
      match%lwt f () with
      | Some x ->
          Lwt.return (Ok x)
      | None ->
          let%lwt _test = Lwt_unix.sleep 5.0 in
          repeat_until_some f (c - 1) )

let rec repeat_until_presult f c =
  match c with
  | 0 ->
      f ()
  | _ -> (
      match%lwt f () with
      | `Ok x ->
          Lwt.return (`Ok x)
      | `Error _ ->
          let%lwt _test = Lwt_unix.sleep 5.0 in
          repeat_until_presult f (c - 1) )

let rec repeat_until_ok f c =
  match c with
  (*We can't seem to successfully return the call so return the error for analysis.*)
  | 0 ->
      f ()
  | _ -> (
      match%lwt f () with
      | Ok x ->
          Lwt.return (Ok x)
      | Error _ ->
          let%lwt _test = Lwt_unix.sleep 5.0 in
          repeat_until_ok f (c - 1) )

let ok_or_raise (x : (_, exn) result) =
  match x with Ok y -> y | Error e -> raise e

(* stolen from extractor *)
let run_command ~timeout ~command =
  Lwt_process.with_process_full ~timeout command (fun p ->
      let%lwt () = Lwt_io.close p#stdin in
      let%lwt status = p#status
      and stdout = Lwt_io.read p#stdout
      and stderr = Lwt_io.read p#stderr in
      Lwt.return (status, stdout, stderr))

let key_check () =
  match Sys.getenv_opt "MC_KEY" with
  | Some ""
  | None ->
      failwith "Error: The ENV variable MC_KEY isn't set and must be set."
  | Some s ->
    ()

(*TODO These don't need to be polymophic*)
type verb = [`Get | `Put]
