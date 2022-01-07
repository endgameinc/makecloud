let sprintf = Printf.sprintf

type repository = { full_name : string; master_branch : string }
[@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

type head_commit = { id : string }
[@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

type github_response = {
  ref : string;
  repository : repository;
  head_commit : head_commit;
}
[@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

type status = Pending | Success | Failed | Error
type msg = { state : status; target_url : Uri.t; description : string }

let status_to_string x =
  match x with
  | Pending -> "pending"
  | Success -> "success"
  | Failed -> "failed"
  | Error -> "error"

let send_status (token : string) full sha (status : status) =
  let path = sprintf "/repos/%s/statuses/%s" full sha in
  let user = String.split_on_char '/' full |> List.hd in
  let credentials = `Basic (user, token) in
  let uri = Uri.make ~scheme:"https" ~host:"api.github.com" ~path () in
  let headers = Cohttp.Header.(add_authorization (init ()) credentials) in
  let body =
    sprintf {|{ "state": "%s" }|} (status_to_string status)
    |> Cohttp_lwt.Body.of_string
  in
  let%lwt _req, body = Cohttp_lwt_unix.Client.post ~headers ~body uri in
  let%lwt () = Cohttp_lwt.Body.drain_body body in
  (*TODO Handle Failure*)
  Lwt.return_unit

let get_archive token full sha =
  (*TODO Handle Failure*)
  let path = sprintf "/repos/%s/tarball/%s" full sha in
  let user = String.split_on_char '/' full |> List.hd in
  let credentials = `Basic (user, token) in
  let uri = Uri.make ~scheme:"https" ~host:"api.github.com" ~path () in
  let headers = Cohttp.Header.(add_authorization (init ()) credentials) in
  let%lwt resp, body = Cohttp_lwt_unix.Client.get ~headers uri in
  let%lwt () = Cohttp_lwt.Body.drain_body body in
  let download_loc =
    Cohttp_lwt_unix.Response.headers resp |> Cohttp.Header.get_location
  in
  let download_loc =
    match download_loc with
    | Some x -> x
    | None ->
        failwith
          "No redirect in response from Github when asking for a download, \
           this probably means you don't have permissions."
  in
  let%lwt _resp, body = Cohttp_lwt_unix.Client.get download_loc in
  let temp_file = Filename.temp_file "makecloud_gh_download" ".tar.gz" in
  let%lwt () = Lwt_io.printl temp_file in
  let%lwt fp = Lwt_io.open_file ~mode:Output temp_file in
  let%lwt () = Lwt_io.write_lines fp (Cohttp_lwt.Body.to_stream body) in
  let%lwt () = Lwt_io.close fp in
  Lwt.return temp_file

let parse_github _req body =
  let%lwt body = Cohttp_lwt.Body.to_string body in
  let gh_resp = Yojson.Safe.from_string body |> github_response_of_json in
  let%lwt gh_resp =
    match gh_resp with
    | Ok x -> Lwt.return x
    | Error _ ->
        failwith
          "JSON sent to handler was improperly formatted, this really \
           shouldn't happen?"
  in
  Lwt.return gh_resp

let process aws_profile req body =
  let gh_token = Sys.getenv "GITHUB_TOKEN" in
  let%lwt gh_info = parse_github req body in
  let%lwt tmp_file =
    get_archive gh_token gh_info.repository.full_name gh_info.head_commit.id
  in
  let repo_dir = Filename.chop_suffix tmp_file ".tar.gz" in
  (*TOOD: This is hacky as hell but can wait til replaced with pure OCaml tar.*)
  let cmd =
    sprintf "mkdir %s; tar zxf %s -C %s --strip-components=1" repo_dir tmp_file
      repo_dir
  in
  let%lwt _out = Lwt_process.pread (Lwt_process.shell cmd) in
  let deploy =
    String.equal gh_info.ref
      (sprintf "refs/heads/%s" gh_info.repository.master_branch)
  in
  Lwt.async (fun () ->
      let params =
        Engine.Lib.make_params ~repo_dir ~nocache:false ~deploy ~target_nodes:[]
          ~dont_delete:[] ?aws_profile ()
      in
      Engine.Runner.main params);
  Lwt.return ()

let handler profile req body =
  let%lwt () = process profile req body in
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"sent" ()
