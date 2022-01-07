module R = Rresult.R
open Jingoo
open Cohttp_lwt_unix
module Run_store = Irmin_unix.Git.FS.KV (Run.Run)

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
  | Some true -> Lwt.return true
  | _ -> Lwt.return false

let show_with_base (body : string) =
  Jg_template.from_string Templates.base
    ~models:[ ("content", Jg_types.Tstr body) ]

let add_run store guid run =
  runs := GuidState.add guid run !runs;
  let info = Irmin_unix.info "added %s" guid in
  Run_store.set_exn store [ guid ] run ~info

let show_index _req body =
  let%lwt () = Cohttp_lwt.Body.drain_body body in
  let content = GuidState.bindings !runs in
  let runs = List.map Run.to_jingoo content in
  let data = Jg_types.Tlist runs in
  let page_content =
    Jg_template.from_string Templates.index ~models:[ ("links", data) ]
  in
  let body = show_with_base page_content in
  Server.respond_string ~status:`OK ~body ()

let report_http_check req body =
  let uri = Request.uri req in
  let bind = R.bind in
  let%bind guid =
    match Uri.get_query_param uri "guid" with
    | None -> R.error (`Bad_request, "must supply a guid parameter for the run.")
    | Some i -> R.ok i
  in
  let%bind node_name =
    match Uri.get_query_param uri "node_name" with
    | None ->
        R.error (`Bad_request, "must supply a node_name parameter for the run.")
    | Some i -> R.ok i
  in
  let clean_json = Yojson.Safe.from_string body in
  let%bind state =
    match Engine.Notify.state_of_json clean_json with
    | Error _ ->
        R.error (`Bad_request, "must supply a valid state for the node.")
    | Ok i -> R.ok i
  in
  R.ok (guid, node_name, state)

let add_event t guid (new_entry : Run.stage) =
  let r = !runs in
  match GuidState.find_opt guid r with
  | Some x ->
      let new_run = Run.add_stage x new_entry in
      add_run t guid new_run
  | None ->
      let new_run =
        Run.create_run new_entry (Engine.Notify.make_run_start [] [])
      in
      add_run t guid new_run

let report_http t req body =
  let%lwt body = Cohttp_lwt.Body.to_string body in
  match report_http_check req body with
  | Ok (guid, node_name, state) ->
      (*let%lwt () = Engine.Notify.print_state guid node_name state in*)
      let new_event = Run.create_stage node_name state in
      let%lwt () = add_event t guid new_event in
      Server.respond_string ~status:`OK ~body:"Accepted." ()
  | Error (t, msg) -> Server.respond_string ~status:t ~body:msg ()

let run_report_http_check req body =
  let uri = Request.uri req in
  let bind = R.bind in
  let%bind guid =
    match Uri.get_query_param uri "guid" with
    | None -> R.error (`Bad_request, "must supply a guid parameter for the run.")
    | Some i -> R.ok i
  in
  let name = Uri.get_query_param uri "name" in
  let body_json = Yojson.Safe.from_string body in
  let%bind state =
    Engine.Notify.run_state_of_json body_json
    |> R.reword_error (fun _ -> (`Bad_request, "Error decoding run state"))
  in
  R.ok (guid, name, state)

let change_run_state t guid (state : Engine.Notify.run_state) =
  match GuidState.find_opt guid !runs with
  | Some x ->
      let new_run = Run.change_status x state in
      add_run t guid new_run
  | None ->
      let new_run = Run.create_run_init state () in
      add_run t guid new_run

let run_report_http t req body =
  let%lwt body = Cohttp_lwt.Body.to_string body in
  match run_report_http_check req body with
  | Ok (guid, _name, state) ->
      let%lwt () = Engine.Notify.print_run_state guid state in
      let%lwt () = change_run_state t guid state in
      Server.respond_string ~status:`OK ~body:"Accepted." ()
  | Error (t, msg) -> Server.respond_string ~status:t ~body:msg ()

let ret_404_http () =
  Server.respond_string ~status:`Not_found ~body:"404 - Not Found." ()

let show_run_http_check req : (string, [> `Bad_request ] * string) result =
  let uri = Request.uri req in
  let bind = R.bind in
  let%bind guid =
    match Uri.get_query_param uri "guid" with
    | None ->
        R.error (`Bad_request, "must supply a valid guid parameter for the run.")
    | Some i -> R.ok i
  in
  R.ok guid

let show_run_http req body =
  let%lwt () = Cohttp_lwt.Body.drain_body body in
  match show_run_http_check req with
  | Ok guid -> (
      let content = GuidState.find_opt guid !runs in
      match content with
      | None -> ret_404_http ()
      | Some r ->
          let stages = Run.stages_to_jingoo r in
          let nodes, edges = Run.make_graph r in
          let page_content =
            Jg_template.from_string Templates.show_runs
              ~models:
                [
                  ("stages", stages);
                  ("guid", Jg_types.Tstr guid);
                  ("nodes", nodes);
                  ("edges", edges);
                ]
          in
          let body = show_with_base page_content in
          Server.respond_string ~status:`OK ~body ())
  | Error (t, msg) -> Server.respond_string ~status:t ~body:msg ()

type asset = Timeago | Fullrender | Vizjs

let serve_asset (asset : asset) _req body =
  let%lwt () = Cohttp_lwt.Body.drain_body body in
  let body =
    match asset with
    | Timeago -> [%blob "assets/timeago.min.js"]
    | Fullrender -> [%blob "assets/full.render.js"]
    | Vizjs -> [%blob "assets/viz.js"]
  in
  Server.respond_string ~status:`OK ~body ()

let http_auth api_key callback t req body =
  match%lwt authentication api_key req with
  | true -> callback t req body
  | false ->
      let%lwt () =
        Lwt_io.printl "Error: Request rejected due to incorrect api key."
      in
      Server.respond_string ~status:`Unauthorized ~body:"incorrect api key\n" ()

let logs_request req =
  let time = Ptime_clock.now () in
  let time_pp =
    match Ptime_clock.current_tz_offset_s () with
    | Some tz_offset_s -> Ptime.pp_human ~tz_offset_s ()
    | None -> Ptime.pp
  in
  Format.asprintf "[%a] %a" time_pp time Request.pp_hum req

let router t api_key profile _conn req body =
  let uri = Cohttp.Request.uri req in
  let auth = http_auth api_key in
  (*let%lwt () = Lwt_io.printl (logs_request req) in*)
  match Uri.path uri with
  | "/" -> show_index req body
  | "/show_run" -> show_run_http req body
  | "/report" -> auth report_http t req body
  | "/run_report" -> auth run_report_http t req body
  | "/github_handler" -> Github.handler profile req body
  | "/assets/timeago.min.js" -> serve_asset Timeago req body
  | "/assets/full.render.js" -> serve_asset Fullrender req body
  | "/assets/viz.js" -> serve_asset Vizjs req body
  | _ -> ret_404_http ()

let load_data t =
  let%lwt keys = Run_store.list t [] in
  Lwt_list.iter_s
    (fun (step, _data) ->
      match%lwt Run_store.find t [ step ] with
      | Some run ->
          runs := GuidState.add step run !runs;
          Lwt_io.printl (Printf.sprintf "Found item for %s" step)
      | None ->
          Lwt_io.printl (Printf.sprintf "Error: No item found for %s" step))
    keys

let server api_key profile =
  let () = Engine.Lib.key_check () in
  let config = Irmin_fs.config "mc_datastore" in
  let%lwt repo = Run_store.Repo.v config in
  let%lwt tree = Run_store.master repo in
  let%lwt () = load_data tree in
  Server.create
    ~mode:(`TCP (`Port 9000))
    (Server.make ~callback:(router tree api_key profile) ())

let main api_key profile = Lwt_main.run (server api_key profile)

open Cmdliner

let auth_cli p =
  let doc = "key for authentication." in
  Arg.(value & pos p string "." & info [] ~docv:"MC_KEY" ~doc)

let aws_profile =
  let doc = "Specific AWS profile to run under." in
  Arg.(
    value & opt (some string) None & info [ "profile" ] ~docv:"AWS_PROFILE" ~doc)

let main_t = Term.(const main $ auth_cli 0 $ aws_profile)

let info =
  let doc = "the web server for makecloud." in
  let man =
    [
      `S Manpage.s_bugs;
      `P "Please report bugs to the project's github issue tracker.";
    ]
  in
  Term.info "makecloud" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits
    ~man

let () = Term.exit @@ Term.eval (main_t, Term.info Sys.executable_name)
