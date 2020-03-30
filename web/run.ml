let sprintf = Printf.sprintf

type stage = {node_name: string; state: Engine.Notify.state; time: float} [@@deriving irmin]

type run = {created: float; stages: stage list; status: Engine.Notify.run_state list} [@@deriving irmin]

module Run = struct
  type t = run
  let t = run_t
  let merge = Irmin.Merge.(option (idempotent t))
end

let create_run stage (status : Engine.Notify.run_state) =
  let created = Unix.time () in
  let stages = [stage] in
  let status = [status] in
  {created; stages; status}

let create_run_init (status : Engine.Notify.run_state) () =
  let created = Unix.time () in
  let stages = [] in
  let status = [status] in
  {created; stages; status}

let add_stage r s =
  let stages = s :: r.stages in
  {r with stages}

let create_stage node_name state =
  let time = Unix.time () in
  {node_name; state; time}

let change_status r status =
  let status = status :: r.status in
  {r with status}

let get_status r =
  List.hd r.status

let to_jingoo (guid, r) =
  Jingoo.Jg_types.(
    Tobj
      [ ("guid", Tstr guid)
      ; ("created", Tint (int_of_float r.created * 1000))
      ; ("link", Tstr (sprintf "/show_run?guid=%s" guid))
      ; ("status", Tstr (Engine.Notify.run_state_name_to_string (get_status r))) ])

let stages_to_jingoo (r : run) =
  let aux s =
    Jingoo.Jg_types.(
      Tobj
        [ ("node_name", Tstr s.node_name)
        ; ("state", Tstr (Engine.Notify.state_name_to_string s.state))
        ; ("time", Tint (int_of_float s.time * 1000)) ])
  in
  let stages = List.map aux r.stages in
  Jingoo.Jg_types.Tlist stages
