let sprintf = Printf.sprintf

type stage = {node_name: string; state: Engine.Notify.state; time: float}

type run = {created: float; stages: stage list; status: Engine.Notify.run_state}

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
