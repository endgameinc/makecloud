let sprintf = Printf.sprintf

type state = StartBox | StartCommands | EndBoxSuccessful | EndBoxFailed

type run_state = RunStart | RunSuccess | RunFail | RunException

let state_to_string = function
  | StartBox ->
      "StartBox"
  | StartCommands ->
      "StartCommands"
  | EndBoxSuccessful ->
      "EndBoxSuccessful"
  | EndBoxFailed ->
      "EndBoxFailed"

let state_to_json x =
  `String (state_to_string x)

let run_state_to_string = function
  | RunStart ->
      "RunStart"
  | RunSuccess ->
      "RunSuccess"
  | RunFail ->
      "RunFail"
  | RunException ->
      "RunException"

let run_state_to_json x =
  `String (run_state_to_string x)

let state_of_string = function
  | "StartBox" ->
      Some StartBox
  | "StartCommands" ->
      Some StartCommands
  | "EndBoxSuccessful" ->
      Some EndBoxSuccessful
  | "EndBoxFailed" ->
      Some EndBoxFailed
  | _ ->
      None

let state_of_json_exn = function
  | `String v -> (match state_of_string v with
    | Some x -> x
    | None -> raise (Failure (sprintf "Failed to parse %s into a valid state." v)))
  | _ -> raise (Failure "wrong type of value to parse into state.")

let run_state_of_string = function
  | "RunStart" ->
      Some RunStart
  | "RunSuccess" ->
      Some RunSuccess
  | "RunFail" ->
      Some RunFail
  | "RunException" ->
      Some RunException
  | _ ->
      None

let run_state_of_json_exn = function
  | `String v -> (match run_state_of_string v with
    | Some x -> x
    | None -> raise (Failure (sprintf "Failed to parse %s into a valid run state." v)))
  | _ -> raise (Failure "wrong type of value to parse into run state.")

let send_state ~(settings : Settings.t) ~guid ~node (state : state) ~key =
  match settings.notify_url with
  | None ->
      Lwt.return ()
  | Some uri ->
      let uri = Uri.with_path uri "/report" in
      let node_name = Node.node_to_string node in
      let q_params =
        [ ("guid", guid)
        ; ("node_name", node_name)
        ; ("state", state_to_string state) ]
      in
      let uri = Uri.with_query' uri q_params in
      let headers = Cohttp.Header.init_with "ApiKey" key in
      let body = Cohttp_lwt.Body.empty in
      (*TODO cohttp_retry. *)
      let%lwt _resp, body = Cohttp_lwt_unix.Client.put uri ~headers ~body in
      let%lwt () = Cohttp_lwt.Body.drain_body body in
      Lwt.return ()

let send_run_state ~(settings : Settings.t) ~guid (state : run_state) ~key =
  match settings.notify_url with
  | None ->
      Lwt.return ()
  | Some uri ->
      let uri = Uri.with_path uri "/run_report" in
      let guid = Uuidm.to_string guid in
      let name =
        match settings.name with Some x -> [("name", x)] | None -> []
      in
      let q_params =
        name @ [("guid", guid); ("state", run_state_to_string state)]
      in
      let uri = Uri.with_query' uri q_params in
      let headers = Cohttp.Header.init_with "ApiKey" key in
      let body = Cohttp_lwt.Body.empty in
      (*TODO cohttp_retry. *)
      let%lwt _resp, body = Cohttp_lwt_unix.Client.put uri ~headers ~body in
      let%lwt () = Cohttp_lwt.Body.drain_body body in
      Lwt.return ()

let print_state guid node_name state =
  Lwt_io.printf "[%s][%s] %s\n" guid node_name (state_to_string state)

let print_run_state guid state =
  Lwt_io.printf "[%s] %s\n" guid (run_state_to_string state)
