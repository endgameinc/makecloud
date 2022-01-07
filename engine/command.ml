open Lib

type t =
  | Upload of (string * string)
  | Download of (string * string)
  | Run of string
  | Publish

let cache_command cmd =
  match cmd with
  | Upload _ -> true
  | Download _ -> false
  | Run _ -> false
  | Publish -> true

let parse_upload cmd_string =
  let src = List.nth cmd_string 0 in
  let dst = List.nth cmd_string 1 in
  Ok (Upload (src, dst))

let parse_download cmd_string =
  let src = List.nth cmd_string 0 in
  let dst = List.nth cmd_string 1 in
  Ok (Download (src, dst))

let parse_run cmd_string =
  let body = String.concat " " cmd_string in
  Ok (Run body)

let parse_publish = Ok Publish

let parse_command cmd =
  let split = String.split_on_char ' ' cmd in
  match (List.hd split, List.tl split) with
  | "RUN", rest -> parse_run rest
  | "UPLOAD", rest -> parse_upload rest
  | "DOWNLOAD", rest -> parse_download rest
  | "PUBLISH", _ -> parse_publish
  | s, _ -> R.error_msgf "Error: %s is not a valid command." s

let parse_commands cmds =
  let aux past_cmds new_cmd : (t list, [> R.msg ]) result =
    match (past_cmds, parse_command new_cmd) with
    | Ok old_cmds, Ok cmd -> Ok (old_cmds @ [ cmd ])
    | Ok _, (Error _ as e) -> e
    | (Error _ as e), _ -> e
  in
  List.fold_left aux (Ok []) cmds

let to_string cmd =
  match cmd with
  | Run s -> Fmt.str "RUN %s" s
  | Upload (fst, dst) -> Fmt.str "UPLOAD %s %s" fst dst
  | Download (fst, dst) -> Fmt.str "DOWNLOAD %s %s" fst dst
  | Publish -> "PUBLISH"

let src_files cmd =
  match cmd with
  | Run _ -> []
  | Upload (_, dst) -> [ dst ]
  | Download (fst, _) -> [ fst ]
  | Publish -> [ ".ami_id" ]
