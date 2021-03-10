open Cohttp_lwt_unix
module R = Rresult.R
let sprintf = Printf.sprintf

let env = ref [||]

let cmds = ref []

let outputs = ref []

let next_id = ref 0

let win_env =
  [ "ALLUSERSPROFILE"
  ; "APPDATA"
  ; "CommonProgramFiles"
  ; "CommonProgramFiles(x86)"
  ; "CommonProgramW6432"
  ; "COMPUTERNAME"
  ; "HOMEDRIVE"
  ; "HOMEPATH"
  ; "LOCALAPPDATA"
  ; "LOGONSERVER"
  ; "PATH"
  ; "ProgramData"
  ; "ProgramFiles"
  ; "ProgramFiles(x86)"
  ; "ProgramW6432"
  ; "PUBLIC"
  ; "SESSIONNAME"
  ; "SystemDrive"
  ; "SystemRoot"
  ; "USERDOMAIN"
  ; "USERDOMAIN_ROAMINGPROFILE"
  ; "USERPROFILE" ]

let option_map fx x = match x with Some y -> Some (fx y) | None -> None

(* stolen from extractor *)
let run_command ~command =
  let additional_env =
    match Sys.os_type with
    | "Win32" | "Cygwin" ->
        List.fold_left
          (fun acc x ->
            match (x, Sys.getenv_opt x) with
            | _, None ->
                acc
            | z, Some y ->
                (z ^ "=" ^ y) :: acc)
          [] win_env
        |> Array.of_list
    | _ ->
        [||]
  in
  let lwt_ofd, ofd = Lwt_unix.pipe_in () in
  let out = Lwt_io.(read_lines (of_fd ~mode:Input lwt_ofd)) in
  let lwt_efd, efd = Lwt_unix.pipe_in () in
  let err = Lwt_io.(read_lines (of_fd ~mode:Input lwt_efd)) in
  let status = 
    let result = Lwt_process.exec ~env:(Array.append !env additional_env) ~stdout:(`FD_copy ofd) ~stderr:(`FD_copy efd) command in
    let parse_result = function
      | Unix.WEXITED 0 ->
        Lwt.return true 
      | Unix.WEXITED _ | WSIGNALED _ | WSTOPPED _ ->
        Lwt.return false 
    in
    Lwt.bind result parse_result
  in 
  Lwt.return (status, out, err)

let authentication key req =
  let req_key = Cohttp.Header.get (Request.headers req) "ApiKey" in
  match option_map (String.equal key) req_key with
  | Some true ->
      true
  | _ ->
      false

module Upload = struct
  type t = { 
    src_file : string;
    dst_url : string; 
  } [@@deriving of_yojson] 

  let process_body body = 
    let json = Yojson.Safe.from_string body in
    of_yojson json |> R.reword_error R.msg

  let process_upload body =
    let _cmd = process_body body in
    let out_stream, out_push = Lwt_stream.create () in
    let err_stream, err_push = Lwt_stream.create () in
    let promise = 
      let%lwt f = Lwt_io.open_file ~mode:Input "/tmp/test" in
      let s = Lwt_io.read_lines f in
      let _body = Cohttp_lwt.Body.of_stream s in
      let () = out_push (Some "Upload successfully") in
      let () = out_push (None) in
      let () = err_push (None) in
      Lwt.return true
    in
    Lwt.return (promise, out_stream, err_stream)
end

let http_command _req body =
  let%lwt command = Cohttp_lwt.Body.to_string body in
  let promise =
    run_command ~command:(Lwt_process.shell command)
  in
  let id = !next_id in
  next_id := id + 1 ;
  cmds := (id, promise) :: !cmds ;
  Server.respond_string ~status:`Accepted ~body:(string_of_int id) ()

let http_upload _req body =
  let%lwt raw_body = Cohttp_lwt.Body.to_string body in
  let promise =
    Upload.process_upload raw_body
  in
  let id = !next_id in
  next_id := id + 1 ;
  cmds := (id, promise) :: !cmds ;
  Server.respond_string ~status:`Accepted ~body:(string_of_int id) ()

(* TOOD: Figure out how to handle this better. Probably should use norest once that gets open sourced.*)
let get_command req =
  let uri = Cohttp_lwt_unix.Request.uri req in
  let bind = R.bind in
  let%bind str_id =
    match Uri.get_query_param uri "id" with
    | None ->
        R.error (`Bad_request, "must supply an id parameter that is a number.")
    | Some i ->
        R.ok i
  in
  let%bind id =
    match int_of_string_opt str_id with
    | None ->
        R.error (`Bad_request, "Your id parameter must be a number.")
    | Some x ->
        R.ok x
  in
  let%bind promise =
    match List.assoc_opt id !cmds with
    | None ->
        R.error (`Not_found, "No such command.")
    | Some x ->
        R.ok x
  in
  R.ok (id, promise)

let handle_output job_id out err =
  let new_out_lines = Lwt_stream.get_available out in
  let new_err_lines = Lwt_stream.get_available err in
  let all_lines = new_out_lines @ new_err_lines in
  match List.assoc_opt job_id !outputs with
  | None ->
    outputs := (job_id, all_lines) :: !outputs;
    Lwt.return (String.concat "\n" all_lines)
  | Some old_lines ->
    let untouched = List.remove_assoc job_id !outputs in
    let lines = old_lines @ all_lines in
    outputs := (job_id, lines) :: untouched;
    Lwt.return (String.concat "\n" (lines))

let http_check_command req body =
  let%lwt () = Cohttp_lwt.Body.drain_body body in
  match get_command req with
  | Error (code, msg) ->
      Server.respond_string ~status:code ~body:msg ()
  | Ok (job_id, p) -> (
    let%lwt status, out, err = p in
    match Lwt.state status with
    | Lwt.Sleep ->
        let%lwt current_logs = handle_output job_id out err in
        Server.respond_string ~status:`Accepted
          ~body:current_logs ()
    | Lwt.Fail e ->
        Server.respond_string ~status:`Internal_server_error
          ~body:(sprintf "Something went very wrong will running command: %s." (Printexc.to_string e)) ()
    | Lwt.Return (s) -> (
      let%lwt current_logs = handle_output job_id out err in
      match s with
      | true ->
          Server.respond_string ~status:`OK ~body:current_logs ()
      | false ->
          Server.respond_string ~status:`Unprocessable_entity ~body:(Printf.sprintf "job failed error, logs:\n%s\n" current_logs) ()))

let http_set_env _req body =
  let%lwt json_body = Cohttp_lwt.Body.to_string body in
  let json = Yojson.Basic.from_string json_body in
  (* TODO: if this isn't an assoc, this will crash. *)
  let dirty_pairs = Yojson.Basic.Util.to_assoc json in
  let pairs =
    List.map
      (fun (k, v) -> (k, Yojson.Basic.Util.to_string_option v))
      dirty_pairs
  in
  let process_pairs acc (k, v) =
    match acc with
    | None ->
        None
    | Some r -> (
      match v with None -> None | Some item -> Some ((k, item) :: r) )
  in
  let env_list = List.fold_left process_pairs (Some []) pairs in
  match env_list with
  | None ->
      Server.respond_string ~status:`Bad_request
        ~body:"body must be a json object with key,value pairs in it." ()
  | Some p ->
      let weird_fmt = List.map (fun (k, v) -> Printf.sprintf "%s=%s" k v) p in
      env := Array.of_list weird_fmt ;
      Server.respond_string ~status:`OK
        ~body:"Finished successfully setting env variables!" ()

let http_show_env _req _body =
  let results = String.concat "\n" (Array.to_list !env) in
  Server.respond_string ~status:`OK ~body:results ()

let router req body =
  let uri = Cohttp.Request.uri req in
  match Uri.path uri with
  | "/command" ->
      http_command req body
  | "/upload" ->
      http_upload req body
  | "/check_command" ->
      http_check_command req body
  | "/set_env" ->
      http_set_env req body
  | "/show_env" ->
      http_show_env req body
  | _ ->
      Server.respond_string ~status:`Not_found ~body:"Route not found" ()

let server key =
  let callback _conn req body =
    match authentication key req with
    | true ->
        router req body
    | false ->
        Server.respond_string ~status:`Unauthorized ~body:"incorrect api key\n"
          ()
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let main key = Lwt_main.run (server key)

open Cmdliner

let key =
  let doc = "Key used to authenticate requests." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"KEY" ~doc)

let main_t =
  let open Term in
  const main $ key

let info =
  let doc = "the agent for makecloud." in
  let man =
    [ `S Manpage.s_bugs
    ; `P "Please report bugs to the project's github issue tracker." ]
  in
  Term.info "makecloud" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits
    ~man

let () = Term.exit @@ Term.eval (main_t, Term.info Sys.executable_name)
