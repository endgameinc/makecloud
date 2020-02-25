let sprintf = Printf.sprintf

type stage = {node_name: string; state: Engine.Notify.state; time: float}

module Stage = struct
  type t = stage
  let state =
    let open Irmin.Type in
    let open Engine.Notify in
    variant "state" (fun startbox startcommands endboxsuccessful endboxfailed -> function
      | StartBox -> startbox
      | StartCommands -> startcommands
      | EndBoxSuccessful -> endboxsuccessful
      | EndBoxFailed -> endboxfailed)
      |~ case0 "StartBox" StartBox
      |~ case0 "StartCommands" StartCommands
      |~ case0 "EndBoxSuccessful" EndBoxSuccessful
      |~ case0 "EndBoxFailed" EndBoxFailed
      |> sealv
  let t =
    let open Irmin.Type in
    record "stage" (fun node_name state time -> {node_name; state; time})
    |+ field "node_name" string (fun t -> t.node_name)
    |+ field "state" state (fun t -> t.state)
    |+ field "time" float (fun t -> t.time)
    |> sealr
  let merge = Irmin.Merge.(option (idempotent t))
end

type run = {created: float; stages: stage list; status: Engine.Notify.run_state}

module Run = struct
  type t = run
  let status =
    let open Irmin.Type in
    let open Engine.Notify in
    variant "status" (fun runstart runsuccess runfail runexception -> function
      | RunStart -> runstart
      | RunSuccess -> runsuccess
      | RunFail -> runfail
      | RunException -> runexception
      )
      |~ case0 "RunStart" RunStart
      |~ case0 "RunSuccess" RunSuccess
      |~ case0 "RunFail" RunFail
      |~ case0 "RunException" RunException
      |> sealv

  let t =
    let open Irmin.Type in
    record "run" (fun created stages status -> {created; stages; status;})
    |+ field "created" float (fun t -> t.created)
    |+ field "stages" (list Stage.t) (fun t -> t.stages)
    |+ field "status" status (fun t -> t.status)
    |> sealr
  let merge = Irmin.Merge.(option (idempotent t))
end

let create_run stage =
  let created = Unix.time () in
  let stages = [stage] in
  let status = Engine.Notify.RunStart in
  {created; stages; status}

let create_run_init () =
  let created = Unix.time () in
  let stages = [] in
  let status = Engine.Notify.RunStart in
  {created; stages; status}

let add_stage r s =
  let stages = s :: r.stages in
  {r with stages}

let create_stage node_name state =
  let time = Unix.time () in
  {node_name; state; time}

let change_status r status = {r with status}

let to_jingoo (guid, r) =
  Jingoo.Jg_types.(
    Tobj
      [ ("guid", Tstr guid)
      ; ("created", Tint (int_of_float r.created * 1000))
      ; ("link", Tstr (sprintf "/show_run?guid=%s" guid))
      ; ("status", Tstr (Engine.Notify.run_state_to_string r.status)) ])

let stages_to_jingoo (r : run) =
  let aux s =
    Jingoo.Jg_types.(
      Tobj
        [ ("node_name", Tstr s.node_name)
        ; ("state", Tstr (Engine.Notify.state_to_string s.state))
        ; ("time", Tint (int_of_float s.time * 1000)) ])
  in
  let stages = List.map aux r.stages in
  Jingoo.Jg_types.Tlist stages
