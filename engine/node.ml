open Lib

type keyword = NoCache | Deploy | Windows | Expire of int [@@deriving show, eq]

let keyword_of_string k =
  let split_cmd = String.split_on_char '-' k in
  match split_cmd with
  | ["nocache"] ->
      Ok NoCache
  | ["deploy"] ->
      Ok Deploy
  | ["windows"] ->
      Ok Windows
  | ["expire"; m] ->
      (try Ok (Expire (int_of_string m))
      with _ -> R.error_msg (sprintf "%s's argument must be a int." k))
  | ["expire"] ->
      R.error_msg (sprintf "%s needs a argument, i.e. expire-30 or expire-60." k)
  | _ ->
      R.error_msg (sprintf "%s is not a known keyword." k)

type real_node =
  { name: string
  ; file_root: string list
  ; base: string
  ; dependson: string list
  ; steps: Command.t list
  ; keywords: keyword list
  ; env: (string * string) list
  ; color: string
  ; cache: bool }

let colors =
  List.map (fun x -> "\027[3" ^ string_of_int x ^ "m") [1; 2; 3; 4; 5; 6; 7]

type synthetic_node = {name: string; dependson: string list}

type node = Rnode of real_node | Snode of synthetic_node

let node_to_string n =
  match n with Rnode {name= n; _} -> n | Snode {name= n; _} -> n

let node_log n log =
  let line = sprintf "%s[AWS][%s] %s" n.color n.name log in
  Lwt_io.printl line

let is_node_cachable n =
  let cmd_cachable = List.exists Command.cache_command n.steps  in
  cmd_cachable && n.cache

let rnode_has_keyword (n : real_node) keyword =
  List.exists (equal_keyword keyword) n.keywords

let rnode_get_expire_time (n : real_node) =
  let aux (current : int option) new_keyword : int option =
    match current, new_keyword with
    | _, Expire m -> Some m
    | Some _ as s, _ -> s
    | None, _ -> None
  in
  match List.fold_left aux None n.keywords with
  | Some m -> m
  (*TODO: Change this to be defined somewhere better, maybe a defaults module in engine?.*)
  | None -> 20

let node_has_keyword (n : node) keyword =
  match n with Rnode x -> rnode_has_keyword x keyword | Snode _ -> false

(*TODO We should do this in ocaml.*)
let hash_of_folder folder =
  let cmd =
    sprintf
      "find %s -type f -print0 | sort -z | xargs -0 sha256sum | sha256sum"
      folder
  in
  let%lwt _status, stdout, _stderr =
    Lib.run_command ~timeout:60.0 ~command:(Lwt_process.shell cmd)
  in
  Lwt.return (String.trim stdout)

let hash_of_node_config (n : real_node) =
  let step_text = List.map Command.to_string n.steps in
  let all_text =
    List.concat [[n.name; n.base]; n.file_root; n.dependson; step_text]
  in
  Digestif.SHA256.digest_string (String.concat " " all_text)
  |> Digestif.SHA256.to_hex

let hash_of_node cwd (n : real_node) =
  let%lwt file_hash = Lwt_list.map_p (fun x -> hash_of_folder (Filename.concat cwd x)) n.file_root in
  let config_hash = hash_of_node_config n in
  let final_hash =
    Digestif.SHA256.digest_string (String.concat " " (config_hash :: file_hash))
  in
  Lwt.return (Digestif.SHA256.to_hex final_hash)

let print_hash_breakdown cwd (n : real_node) =
  let aux x =
    let%lwt file_hash = hash_of_folder (Filename.concat cwd x) in
    Lwt_io.printf "Hash of node folder %s is %s\n" x file_hash
  in
  let%lwt () = Lwt_list.iter_s aux n.file_root in
  let config_hash = hash_of_node_config n in
  let%lwt () = Lwt_io.printf "Hash of node config is: %s\n" config_hash in
  Lwt.return_unit

(* counts how many dep a node has *)
let dependson_count n =
  match n with
  | Rnode {dependson= d; _} | Snode {dependson= d; _} ->
      List.length d

let node_has_dep n dep =
  match n with
  | Rnode {dependson; _} | Snode {dependson; _} ->
      List.exists (String.equal dep) dependson

let get_node_cache n = match n with Rnode x -> x.cache | Snode _ -> true

let get_edges n =
  match n with
  | Rnode r ->
      List.map (fun x -> (x, r.name)) r.dependson
  | Snode s ->
      List.map (fun x -> (x, s.name)) s.dependson

(* removes a dep from a given node *)
let depend_rm n (dep : string) : node =
  let rm l i = List.filter (fun x -> not (String.equal i x)) l in
  match n with
  | Rnode x ->
      let dependson = rm x.dependson dep in
      Rnode {x with dependson}
  | Snode node ->
      let dependson = rm node.dependson dep in
      Snode {node with dependson}

let update_cache_from_node n b =
  match n with
  | Rnode x ->
      let cache = x.cache && b in
      Rnode {x with cache}
  | _ as x ->
      x

let update_node_dependencies todo_node (can_cache, finished_node) =
  let finished_name = node_to_string finished_node in
  let new_todo =
    if node_has_dep todo_node finished_name then
      update_cache_from_node todo_node can_cache
    else todo_node
  in
  depend_rm new_todo finished_name

let make_snode (yaml_root : string * Yaml.value) :
    (synthetic_node, [> R.msg]) result =
  let bind = R.bind in
  let node_name, _ = yaml_root in
  let%bind attrib_list = get_assoc_list (snd yaml_root) in
  let%bind deps = get_value attrib_list "dependson" in
  let%bind deps_list = get_array deps in
  let%bind deps_list_str = result_fold get_string [] deps_list in
  Ok {name= node_name; dependson= deps_list_str}

let make_rnode (yaml_root : string * Yaml.value) :
    (real_node, [> R.msg]) result =
  let bind = R.bind in
  let node_name, _ = yaml_root in
  let%bind attrib_list = get_assoc_list (snd yaml_root) in
  let%bind file_root =
    match get_value attrib_list "fileroot" with
    | Error _ ->
      R.error_msg (sprintf "%s needs an attribute fileroot" node_name)
    | Ok roots ->
      (match get_array roots with
      | Error _ -> get_string (snd roots) |> R.map (fun x -> [x])
      | Ok roots_list -> result_fold get_string [] roots_list)
  in
  let%bind base = get_string_from_attrib_list attrib_list "base" in
  let%bind steps_raw = get_value attrib_list "steps" in
  let%bind steps_yaml = get_array steps_raw in
  let%bind rev_steps = result_fold get_string [] steps_yaml in
  let steps = List.rev rev_steps in
  let%bind steps = Command.parse_commands steps in
  let%bind dependson =
    match get_value attrib_list "dependson" with
    | Error _ ->
        Ok []
    | Ok deps ->
        let%bind deps_list = get_array deps in
        result_fold get_string [] deps_list
  in
  let%bind keywords =
    match get_value attrib_list "keywords" with
    | Error _ ->
        Ok []
    | Ok keys ->
        let%bind keys_list = get_array keys in
        let%bind strings = result_fold get_string [] keys_list in
        result_fold keyword_of_string [] strings
  in
  let%bind env =
    match get_value attrib_list "env" with
    | Error _ ->
        Ok []
    | Ok keys ->
        let%bind keys_list = get_array keys in
        result_fold
          (fun x ->
            let%bind key = get_string x in
            let potential_value = Sys.getenv_opt key in
            let%bind value =
              R.of_option
                ~none:(fun () ->
                  R.error_msg (sprintf "Failed to get an env variable: %s" key))
                potential_value
            in
            R.ok (key, value))
          [] keys_list
  in
  let choice = Random.State.int (Random.State.make_self_init ()) 7 in
  let color = List.nth colors choice in
  let cache = not (List.exists (equal_keyword NoCache) keywords) in
  Ok
    { name= node_name
    ; file_root
    ; base
    ; dependson
    ; steps
    ; color
    ; keywords
    ; env
    ; cache }

let make_node (yaml_root : string * Yaml.value) : (node, [> R.msg]) result =
  let bind = R.bind in
  let%bind assoc_list = get_assoc_list (snd yaml_root) in
  match get_value assoc_list "base" with
  | Ok _ ->
      let%bind node = make_rnode yaml_root in
      Ok (Rnode node)
  | Error _ ->
      let%bind node = make_snode yaml_root in
      Ok (Snode node)
