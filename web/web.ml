module R = Rresult.R
open Jingoo
open Cohttp_lwt_unix

type state = (float * string) list

module GuidState = Map.Make (String)

let runs = ref GuidState.empty

let option_map fx x = match x with Some y -> Some (fx y) | None -> None

module Templates = struct
  let base = [%blob "templates/base.html"]

  let index = [%blob "templates/index.html"]

  let show_runs = [%blob "templates/show_runs.html"]
end

let authentication api_key req =
  let key = Cohttp.Header.get (Request.headers req) "ApiKey" in
  (* TODO Make this a real auth string taken from the commandline. *)
  match option_map (String.equal api_key) key with
  | Some true ->
      true
  | _ ->
      false

let show_with_base (body : string) =
  Jg_template.from_string Templates.base
    ~models:[("content", Jg_types.Tstr body)]

let show_index _req body =
  let%lwt () = Cohttp_lwt.Body.drain_body body in
  let content = GuidState.bindings !runs in
  let runs = List.map Run.to_jingoo content in
  let data = Jg_types.Tlist runs in
  let page_content =
    Jg_template.from_string Templates.index ~models:[("links", data)]
  in
  let body = show_with_base page_content in
  Server.respond_string ~status:`OK ~body ()

let report_http_check req =
  let uri = Request.uri req in
  let bind = R.bind in
  let%bind guid =
    match Uri.get_query_param uri "guid" with
    | None ->
        R.error (`Bad_request, "must supply a guid parameter for the run.")
    | Some i ->
        R.ok i
  in
  let%bind node_name =
    match Uri.get_query_param uri "node_name" with
    | None ->
        R.error (`Bad_request, "must supply a node_name parameter for the run.")
    | Some i ->
        R.ok i
  in
  let%bind state =
    match Uri.get_query_param uri "state" with
    | None ->
        R.error (`Bad_request, "must supply a state parameter for the node.")
    | Some i ->
        R.ok i
  in
  let%bind state =
    match Engine.Notify.state_of_string state with
    | None ->
        R.error (`Bad_request, "must supply a valid state for the node.")
    | Some i ->
        R.ok i
  in
  R.ok (guid, node_name, state)

let add_event guid new_entry =
  let r = !runs in
  match GuidState.find_opt guid r with
  | Some x ->
      GuidState.add guid (Run.add_stage x new_entry) r
  | None ->
      GuidState.add guid (Run.create_run new_entry) r

let report_http req body =
  let%lwt () = Cohttp_lwt.Body.drain_body body in
  match report_http_check req with
  | Ok (guid, node_name, state) ->
      let%lwt () = Engine.Notify.print_state guid node_name state in
      let new_event = Run.create_stage node_name state in
      runs := add_event guid new_event ;
      Server.respond_string ~status:`OK ~body:"Accepted." ()
  | Error (t, msg) ->
      Server.respond_string ~status:t ~body:msg ()

let run_report_http_check req =
  let uri = Request.uri req in
  let bind = R.bind in
  let%bind guid =
    match Uri.get_query_param uri "guid" with
    | None ->
        R.error (`Bad_request, "must supply a guid parameter for the run.")
    | Some i ->
        R.ok i
  in
  let name = Uri.get_query_param uri "name" in
  let%bind state =
    match Uri.get_query_param uri "state" with
    | None ->
        R.error (`Bad_request, "must supply a state parameter for the node.")
    | Some i ->
        R.ok i
  in
  let%bind state =
    match Engine.Notify.run_state_of_string state with
    | None ->
        R.error (`Bad_request, "must supply a valid state for the node.")
    | Some i ->
        R.ok i
  in
  R.ok (guid, name, state)

let change_run_state guid (state : Engine.Notify.run_state) =
  let r = !runs in
  match GuidState.find_opt guid r with
  | Some x ->
      GuidState.add guid (Run.change_status x state) r
  | None ->
      GuidState.add guid (Run.create_run_init ()) r

let run_report_http req body =
  let%lwt () = Cohttp_lwt.Body.drain_body body in
  match run_report_http_check req with
  | Ok (guid, _name, state) ->
      let%lwt () = Engine.Notify.print_run_state guid state in
      runs := change_run_state guid state ;
      Server.respond_string ~status:`OK ~body:"Accepted." ()
  | Error (t, msg) ->
      Server.respond_string ~status:t ~body:msg ()

let ret_404_http () =
  Server.respond_string ~status:`Not_found ~body:"404 - Not Found." ()

let show_run_http_check req : (string, [> `Bad_request] * string) result =
  let uri = Request.uri req in
  let bind = R.bind in
  let%bind guid =
    match Uri.get_query_param uri "guid" with
    | None ->
        R.error
          (`Bad_request, "must supply a valid guid parameter for the run.")
    | Some i ->
        R.ok i
  in
  R.ok guid

let show_run_http req body =
  let%lwt () = Cohttp_lwt.Body.drain_body body in
  match show_run_http_check req with
  | Ok guid -> (
      let content = GuidState.find_opt guid !runs in
      match content with
      | None ->
          ret_404_http ()
      | Some r ->
          let stages = Run.stages_to_jingoo r in
          let page_content =
            Jg_template.from_string Templates.show_runs
              ~models:[("stages", stages); ("guid", Jg_types.Tstr guid)]
          in
          let body = show_with_base page_content in
          Server.respond_string ~status:`OK ~body () )
  | Error (t, msg) ->
      Server.respond_string ~status:t ~body:msg ()

let serve_timeago _req body =
  let%lwt () = Cohttp_lwt.Body.drain_body body in
  let body = [%blob "assets/timeago.min.js"] in
  Server.respond_string ~status:`OK ~body ()

let http_auth api_key callback req body =
  match authentication api_key req with
  | true ->
      callback req body
  | false ->
      Server.respond_string ~status:`Unauthorized ~body:"incorrect api key\n"
        ()

let router api_key _conn req body =
  let uri = Cohttp.Request.uri req in
  let auth = http_auth api_key in
  match Uri.path uri with
  | "/" ->
      show_index req body
  | "/show_run" ->
      show_run_http req body
  | "/report" ->
      auth report_http req body
  | "/run_report" ->
      auth run_report_http req body
  | "/github_handler" ->
      Github.handler req body
  | "/assets/timeago.min.js" ->
      serve_timeago req body
  | _ ->
      ret_404_http ()

let server api_key =
  let () = key_check () in
  Server.create ~mode:(`TCP (`Port 9000)) (Server.make ~callback:(router api_key) ())

let main api_key = Lwt_main.run (server api_key)

open Cmdliner

let auth_cli p =
  let doc = "Path to the repository to process." in
  Arg.(value & pos p string "." & info [] ~docv:"MC_KEY" ~doc)

let main_t = Term.(const main $ auth_cli 0)

let info =
  let doc = "the web server for makecloud." in
  let man = [`S Manpage.s_bugs; `P "Please report bugs to the project's github issue tracker."] in
  Term.info "makecloud" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits
    ~man

let () = Term.exit @@ Term.eval (main_t, Term.info Sys.executable_name)
