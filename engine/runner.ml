open Aws_ec2
open Lib

exception Agent_Unauthorized of string

exception Generic_AWS_Error of string

exception Incorrect_Time_Format of string

(*TODO These don't need to be polymophic*)
type verb = [`Get | `Put]

let verb_to_string (verb : [`Get | `Put]) =
  match verb with `Get -> "GET" | `Put -> "PUT"

let files_per_dir cwd =
  let rec aux dir_handle configs : string list Lwt.t =
    match%lwt Lwt_unix.readdir dir_handle with
    | "." ->
        aux dir_handle configs
    | ".." ->
        aux dir_handle configs
    | x ->
        aux dir_handle (Filename.concat cwd x :: configs)
    | exception End_of_file ->
        let%lwt () = Lwt_unix.closedir dir_handle in
        Lwt.return configs
  in
  let%lwt dir_handle = Lwt_unix.opendir cwd in
  aux dir_handle []

let get_configs dir_root =
  let rec aux dirs_togo configs =
    match dirs_togo with
    | hd :: tl ->
        let%lwt files = files_per_dir hd in
        let new_configs =
          List.filter (fun x -> Filename.check_suffix x "makecloud.yaml") files
        in
        let%lwt new_folders = Lwt_list.filter_p is_dir files in
        aux (List.append tl new_folders) (List.append configs new_configs)
    | [] ->
        Lwt.return configs
  in
  aux [dir_root] []

let parse_configs config_list : (Node.node list, [> R.msg]) result =
  let bind = R.bind in
  let%bind dirty_root_nodes =
    result_fold (fun x -> Yaml_unix.of_file Fpath.(v x)) [] config_list
  in
  let%bind cleaned_roots = result_fold get_assoc_list [] dirty_root_nodes in
  let flattened_roots = List.concat cleaned_roots in
  result_fold Node.make_node [] flattened_roots

module type Provider = sig
  type t

  (*TODO: Investigate if we can bring Node.real_node into t*)
  val spinup : Settings.t -> Node.real_node -> t Lwt.t

  val set_env : t -> Node.real_node -> unit Lwt.t

  val wait_until_ready : t -> Node.real_node -> unit -> unit option Lwt.t

  (*TODO: Commands need a type, really really need a type.*)
  val runcmd :
       (first_arg:string -> second_arg:string -> verb:verb -> string)
    -> t
    -> Node.real_node
    -> string
    -> (string, [> R.msg] * string) result Lwt.t

  val spindown : t -> Node.real_node -> unit Lwt.t
end

(* TODO: this is bad and should be rewritten but I don't have a better design. *)
(* download s3 local *)
(* upload local s3 *)
let transfer_to_shell ~(transfer_fn : string -> [`Get | `Put] -> Uri.t)
    ~(n : Node.real_node) ~guid =
  let transfer ~first_arg ~second_arg ~(verb : [`Get | `Put]) =
    match verb with
    | `Get ->
        let uri_str =
          Uri.to_string (transfer_fn (sprintf "/%s/%s" guid first_arg) verb)
        in
        sprintf "curl --retry 5 -X GET \"%s\" -o %s" uri_str second_arg
    | `Put ->
        let uri_str =
          Uri.to_string
            (transfer_fn (sprintf "/%s/%s/%s" guid n.name second_arg) verb)
        in
        sprintf "curl --retry 5 -X PUT \"%s\" --upload-file %s" uri_str first_arg
  in
  transfer

module Runner (M : Provider) = struct
  let run_node ~(settings : Settings.t) ~n
      ~(transfer_fn : string -> [`Get | `Put] -> Uri.t) ~guid :
      (string, [> R.msg] * string) result Lwt.t =
    let%lwt box = M.spinup settings n in
    let key = Sys.getenv "MC_KEY" in
    let%lwt () =
      Notify.send_state ~settings ~guid ~node:(Node.Rnode n) ~key Notify.StartBox
    in
    let%lwt _ready = repeat_until_some (M.wait_until_ready box n) 40 in
    let uri_str =
      Uri.to_string (transfer_fn (sprintf "/%s/%s" guid "source.tar") `Get)
    in
    (*TODO: Get put in a blob ppx that loads from a file.*)
    let prep_steps =
      if Node.rnode_has_keyword n Windows then
        ["dir"; "echo %username%"] |> List.map (( ^ ) "RUN ")
      else
        [ sprintf "curl --retry 5 -X GET \"%s\" -o %s" uri_str "source.tar"
        ; "mkdir /source"
        ; "sha256sum source.tar"
        ; "tar xf /source.tar -C /source" ]
        |> List.map (( ^ ) "RUN ")
    in
    let transfer = transfer_to_shell ~transfer_fn ~n ~guid in
    (*TODO: We should handle failure here.*)
    let%lwt () = M.set_env box n in
    let%lwt () =
      Notify.send_state ~settings ~guid ~node:(Node.Rnode n) ~key Notify.StartCommands
    in
    let%lwt result =
      Lwt_list.fold_left_s
        (fun a x ->
          match a with
          | Ok logs, old_logs ->
              let%lwt r = M.runcmd transfer box n x in
              (* TODO: Use Buffer module to accumulate strings *)
              Lwt.return (r, old_logs ^ logs ^ x ^ "\n")
          | Error (err, logs), old_logs ->
              Lwt.return (Error (err, ""), old_logs ^ logs))
        (R.ok "", "")
        (List.append prep_steps n.steps)
    in
    let%lwt () = M.spindown box n in
    match result with
    | Ok logs, old_logs ->
        let%lwt () =
          Notify.send_state ~settings ~guid ~node:(Node.Rnode n) ~key
            Notify.EndBoxSuccessful
        in
        Lwt.return (Ok (old_logs ^ logs))
    | Error (err, logs), old_logs ->
        let%lwt () =
          Notify.send_state ~settings ~guid ~node:(Node.Rnode n) ~key Notify.EndBoxFailed
        in
        Lwt.return (Error (err, old_logs ^ logs))

  (*TODO Retry this.*)
  let store_logs ~(settings : Settings.t) ~guid ~(n : Node.real_node)
      ~console_logs =
    let%lwt creds = Aws_s3_lwt.Credentials.Helper.get_credentials () in
    let credentials = R.get_ok creds in
    let region = Aws_s3.Region.of_string settings.bucket_region in
    let endpoint =
      Aws_s3.Region.endpoint ~inet:`V4 ~scheme:`Https region
    in
    let key = sprintf "%s/%s/console.log" guid n.name in
    (* TODO we should handle failure here. *)
    let%lwt _logs =
      Aws_s3_lwt.S3.retry ~endpoint ~retries:5
        ~f:(fun ~endpoint () ->
          Aws_s3_lwt.S3.put ~bucket:settings.storage_bucket ~key ~endpoint
            ~credentials ~data:console_logs ())
        ()
    in
    Lwt.return ()

  let store_cache ~(settings : Settings.t) ~guid ~cwd ~n =
    let%lwt key = Node.hash_of_node cwd n in
    let%lwt creds = Aws_s3_lwt.Credentials.Helper.get_credentials () in
    let credentials = R.get_ok creds in
    let region = Aws_s3.Region.of_string settings.bucket_region in
    let endpoint =
      Aws_s3.Region.endpoint ~inet:`V4 ~scheme:`Https region
    in
    let%lwt _check =
      Aws_s3_lwt.S3.retry ~endpoint ~retries:5
        ~f:(fun ~endpoint () ->
          Aws_s3_lwt.S3.put ~bucket:settings.storage_bucket ~key ~endpoint
            ~credentials ~data:guid ())
        ()
    in
    Lwt.return ()

  (*TODO Retry this.*)
  let check_cache ~(settings : Settings.t) ~cwd ~n =
    let%lwt hash = Node.hash_of_node cwd n in
    let%lwt creds = Aws_s3_lwt.Credentials.Helper.get_credentials () in
    let safe_creds = R.get_ok creds in
    let region = Aws_s3.Region.of_string settings.bucket_region in
    let endpoint =
      Aws_s3.Region.endpoint ~inet:`V4 ~scheme:`Https region
    in
    Aws_s3_lwt.S3.retry ~endpoint ~retries:5
      ~f:(fun ~endpoint () ->
        Aws_s3_lwt.S3.get ~bucket:settings.storage_bucket ~key:hash ~endpoint
      ~credentials:safe_creds ()) ()

  let transfer_file ~(settings : Settings.t) ~old_guid ~new_guid ~(n : Node.real_node) filename :
      (unit, Aws_s3_lwt.S3.error) result Lwt.t =
    let%lwt creds = Aws_s3_lwt.Credentials.Helper.get_credentials () in
    let credentials = R.get_ok creds in
    let region = Aws_s3.Region.of_string settings.bucket_region in
    let endpoint =
      Aws_s3.Region.endpoint ~inet:`V4 ~scheme:`Https region
    in
    let retries = 3 in
    let old_key = sprintf "%s/%s/%s" old_guid n.name filename in
    let new_key = sprintf "%s/%s/%s" new_guid n.name filename in
    let bind = Lwt_result.bind in
    let%bind init =
      Aws_s3_lwt.S3.retry ~endpoint ~retries
        ~f:(fun ~endpoint () ->
          Aws_s3_lwt.S3.Multipart_upload.init ~endpoint ~credentials
            ~bucket:settings.storage_bucket ~key:new_key ())
        ()
    in
    let%bind () =
      Aws_s3_lwt.S3.retry ~endpoint ~retries
        ~f:(fun ~endpoint () ->
          Aws_s3_lwt.S3.Multipart_upload.copy_part ~endpoint ~credentials init
            ~bucket:settings.storage_bucket ~key:old_key ~part_number:1 ())
        ()
    in
    let _md5 =
      Aws_s3_lwt.S3.retry ~endpoint ~retries
        ~f:(fun ~endpoint () ->
          Aws_s3_lwt.S3.Multipart_upload.complete ~endpoint ~credentials init
            ())
        ()
    in
    Lwt_result.return ()

  let process_cache ~(settings : Settings.t) ~old_guid ~new_guid ~(n : Node.real_node) =
    let%lwt () = Lwt_io.printl (n.color ^ "Processing cache for " ^ n.name) in
    let cmd_filter x =
      String.split_on_char ' ' x |> List.hd |> String.equal "UPLOAD"
    in
    let upload_steps = List.filter cmd_filter n.steps in
    let%lwt transfers =
      Lwt_list.map_p
        (transfer_file ~settings ~old_guid ~new_guid ~n)
        (List.map
           (fun x -> List.nth (String.split_on_char ' ' x) 2)
           upload_steps)
    in
    Lwt.return
      (List.filter
         (fun x -> match x with Error _ -> true | Ok _ -> false)
         transfers)

  let invoke settings cwd guid (n : Node.real_node)
      (transfer_fn : string -> [`Get | `Put] -> Uri.t) nocache :
      (bool, [> R.msg]) result Lwt.t =
    let is_cachable = Node.is_node_cachable n in
    let%lwt cache_status = check_cache ~settings ~cwd ~n in
    let guid = Uuidm.to_string guid in
    match is_cachable && R.is_ok cache_status && not nocache with
    | true -> (
        let cache_guid = R.get_ok cache_status in
        match%lwt process_cache ~settings ~old_guid:cache_guid ~new_guid:guid ~n with
        | [] ->
            Lwt_result.return true
        (* TODO: this should handle errors better *)
        | _ ->
            Lwt.return
              (R.error_msg "Cache processing failed for some reason. Unusual.")
        )
    | false -> (
        match%lwt run_node ~settings ~n ~transfer_fn ~guid with
        | Ok console_logs ->
            let%lwt () = Lwt_io.printl "Steps completed successfully." in
            let%lwt () = store_logs ~settings ~guid ~n ~console_logs in
            let%lwt () = store_cache ~settings ~guid ~cwd ~n in
            Lwt.return (R.ok false)
        | Error (msg, console_logs) ->
            let%lwt () = Lwt_io.printl "Steps didn't complete successfully." in
            let%lwt () = store_logs ~settings ~guid ~n ~console_logs in
            Lwt.return (R.error msg) )
end

module Stub : Provider = struct
  type t = unit

  let spinup _settings (n : Node.real_node) : t Lwt.t =
    let%lwt () =
      Lwt_io.printl ("[STUB]" ^ "[" ^ n.name ^ "]" ^ "Node Spinning Up")
    in
    Lwt.return ()

  let wait_until_ready _box (n : Node.real_node) () =
    let _ = Lwt_io.printl ("[STUB]" ^ "[" ^ n.name ^ "]" ^ " Ready!") in
    Lwt.return (Some ())

  let set_env _box _n = Lwt.return ()

  let runcmd _transfer_fn _box (n : Node.real_node) cmd =
    let _ = Lwt_io.printl ("[STUB]" ^ "[" ^ n.name ^ "]" ^ cmd) in
    Lwt.return (R.ok "")

  let spindown _ (n : Node.real_node) =
    let _ =
      Lwt_io.printl ("[STUB]" ^ "[" ^ n.name ^ "]" ^ "Node Spinning Down")
    in
    Lwt.return ()
end

let ok_or_raise_aws = function
  | `Ok x ->
      x
  | `Error e ->
      raise (Generic_AWS_Error (Aws.Error.format Aws_ec2.Errors_internal.to_string e))

let ok_or_raise (x : (_, exn) result) =
  match x with Ok y -> y | Error e -> raise e

exception No_value of string

let get = function
  | Some x ->
      x
  | None ->
      raise (No_value "tried to get a some out of an option where a none was.")

module Aws : Provider = struct
  type t =
    { ip_address: string
    ; instance_id: string
    ; settings: Settings.t
    ; aws_key: string
    ; aws_secret: string
    }

  let aws_to_result a =
    match a with
    | `Ok x ->
        Result.Ok x
    | `Error e ->
        R.error_msg (Aws.Error.format Aws_ec2.Errors_internal.to_string e)

  let make_user_data n (settings : Settings.t) =
    let raw =
      if Node.rnode_has_keyword n Windows then
        [%blob "userdata_scripts/windows.txt"]
      else [%blob "userdata_scripts/linux.txt"]
    in
    let uri =
      if Node.rnode_has_keyword n Windows then settings.windows_agent_url
      else settings.linux_agent_url
    in
    let models =
      Jingoo.
        [ ("url", Jg_types.Tstr (Uri.to_string uri))
        ; ("key", Jg_types.Tstr (Sys.getenv "MC_KEY")) ]
    in
    let templated = Jingoo.Jg_template.from_string raw ~models in
    let b64 = Base64.(encode templated) |> R.failwith_error_msg in
    Lwt.return (String.split_on_char '=' b64 |> List.hd)

  let spinup (settings : Settings.t) (n : Node.real_node) : t Lwt.t =
    let%lwt () = Node.node_log n "Node Spinning Up" in
    (* TODO improve this to new cred method? maybe pass creds around *)
    let%lwt unsafe_creds = Aws_s3_lwt.Credentials.Helper.get_credentials () in
    let creds = unsafe_creds |> ok_or_raise in
    let aws_key = creds.access_key in
    let aws_secret = creds.secret_key in
    let%lwt () = Node.node_log n "Got Credentials." in
    let%lwt user_data = make_user_data n settings in
    let instance_params =
      (*TODO We should gen an ssh key, upload it to aws and use that instead of a constant key. *)
      (*TODO Pull instance size from the config file.*)
      Types.RunInstancesRequest.make
        ~image_id:n.base
        ~min_count:1 ~max_count:1
        ~key_name:settings.aws_key_name
        ~security_group_ids:[settings.aws_security_group]
        ~instance_type:Types.InstanceType.M4_xlarge
        ~block_device_mappings:[
          Types.BlockDeviceMapping.make
            ~device_name:"/dev/xvda"
            ~ebs:(Types.EbsBlockDevice.make
                    ~volume_size:settings.disk_size
                    ~delete_on_termination:true ())
            ()]
        ~subnet_id:settings.aws_subnet_id
        ~user_data ()
    in
    let get_instance_id () =
      let%lwt result =
        Aws_lwt.Runtime.run_request
          ~region:settings.aws_region
          ~access_key:aws_key
          ~secret_key:aws_secret
          (module RunInstances)
          instance_params
      in
      Lwt.return (result |> aws_to_result)
    in
    (*TODO aws_retry*)
    let%lwt i = repeat_until_ok get_instance_id 5 in
    let instance_id =
      ((R.failwith_error_msg i).instances |> List.hd).instance_id
    in
    let%lwt () = Node.node_log n (sprintf "instance id: %s" instance_id) in
    let%lwt () = Node.node_log n "box starting, getting details about box" in
    let get_details () =
      let result =
        Aws_lwt.Runtime.run_request ~region:settings.aws_region ~access_key:aws_key
          ~secret_key:aws_secret
          (module DescribeInstances)
          (Types.DescribeInstancesRequest.make ~instance_ids:[instance_id] ())
      in
      let%lwt () = Node.node_log n "Trying to aquire the IP now." in
      let%lwt r = result in
      match (r, settings.only_public_ip) with
      | `Ok
          { reservations=
              {instances= {public_ip_address= Some ip; _} :: _; _} :: _
          ; _ }, true ->
          let%lwt () = Node.node_log n (sprintf "ip addr: %s" ip) in
          Lwt.return_ok ip
      | `Ok
          { reservations=
              {instances= {private_ip_address= Some ip; _} :: _; _} :: _
          ; _ }, false ->
          let%lwt () = Node.node_log n (sprintf "ip addr: %s" ip) in
          Lwt.return_ok ip
      | `Ok _, _ ->
          Lwt.return (R.error_msg "Can't get an ip for this box yet.")
      | `Error _ as e, _ ->
          Lwt.return (aws_to_result e)
    in
    (*TODO aws_retry*)
    let%lwt ip = repeat_until_ok get_details 40 in
    let ip_address = R.failwith_error_msg ip in
    Lwt.return {ip_address; instance_id; settings; aws_key; aws_secret}

  let set_env t (n : Node.real_node) =
    let uri = Uri.of_string ("http://" ^ t.ip_address ^ ":8000/set_env") in
    let env = `Assoc (List.map (fun (k, v) -> (k, `String v)) n.env) in
    let body = Yojson.to_string env |> Cohttp_lwt.Body.of_string in
    let headers = Cohttp.Header.init_with "ApiKey" (Sys.getenv "MC_KEY") in
    (*TODO cohttp_retry & drain the body/consume it.*)
    let%lwt _resp, body = Cohttp_lwt_unix.Client.put uri ~headers ~body in
    let%lwt () = Cohttp_lwt.Body.drain_body body in
    Lwt.return ()

  let wait_until_ready t (n : Node.real_node) () =
    let%lwt () = Node.node_log n "Checking if node is up yet." in
    let uri = Uri.of_string ("http://" ^ t.ip_address ^ ":8000/") in
    match%lwt get_or_timeout ~max_request_duration:3.0 uri with
    | Result.Ok _ ->
        let%lwt () = Node.node_log n "Node is up." in
        Lwt.return (Some ())
    | exception _e ->
        Lwt.return None
    | _ ->
        Lwt.return None

  (*TODO Please no more string types.*)
  let send_command box s ~expire_time : (string, [> R.msg] * string) result Lwt.t =
    let uri = Uri.of_string ("http://" ^ box ^ ":8000/command") in
    let headers = Cohttp.Header.init_with "ApiKey" (Sys.getenv "MC_KEY") in
    let rec repeat_until_ok f c =
      match c with
      | 0 ->
          Lwt.return
            (R.error
               ( R.msg "Couldn't get a successful call for repeat_until_ok"
               , "No log, couldn't get a sucess after repeat." ))
      | _ -> (
          match%lwt f () with
          | Error ("command failed on agent.", err_output) ->
              Lwt.return (R.error (R.msg "command failed.", err_output))
          | Ok x ->
              Lwt.return (R.ok x)
          | exception Unix.Unix_error (Unix.ETIMEDOUT, "connect", "") ->
              let%lwt _test = Lwt_unix.sleep 5.0 in
              repeat_until_ok f (c - 1)
          | Error _ ->
              let%lwt _test = Lwt_unix.sleep 5.0 in
              repeat_until_ok f (c - 1) )
    in
    let send_command () =
      let body = Cohttp_lwt.Body.of_string s in
      let%lwt resp, body = Cohttp_lwt_unix.Client.put uri ~headers ~body in
      let%lwt body = Cohttp_lwt.Body.to_string body in
      let process_response x =
        match Cohttp.Response.status x with
        | `Accepted | `OK ->
            Lwt.return (R.ok (resp, body))
        | `Unauthorized ->
            Lwt.return
              (R.error
                 ( "Unauthorized"
                 , "Unauthorized to talk to agent, this really shouldn't \
                    happen." ))
        | _ ->
            Lwt.return
              (R.error
                 ( "Unknown error."
                 , "Unknown error, command was unable to execute." ))
      in
      process_response resp
    in
    let%lwt _resp, body =
      match%lwt repeat_until_ok send_command 10 with
      | Ok (r, b) ->
          Lwt.return (r, b)
      | Error _ ->
          failwith
            "Can't talk to an agent, this probably means the agent failed to \
             install."
    in
    (*TODO cohttp_retry*)
    let poll_agent () =
      let check_uri =
        Uri.of_string ("http://" ^ box ^ ":8000/check_command")
      in
      let check_uri = Uri.add_query_param' check_uri ("id", body) in
      match%lwt Cohttp_lwt_unix.Client.get check_uri ~headers with
      | exception Unix.Unix_error _ ->
          Lwt.return (R.error ("failed to connect.", ""))
      | resp, body -> (
          let%lwt output = Cohttp_lwt.Body.to_string body in
          let%lwt () = Lwt_io.printl output in
          match Cohttp.Response.status resp with
          | `Accepted ->
              Lwt.return (R.error ("still waiting.", ""))
          | `OK ->
              Lwt.return (R.ok output)
          | `Unprocessable_entity ->
              Lwt.return (R.error ("command failed on agent.", output))
          | _ ->
              Lwt.return (R.error ("some other error.", "some other error.")) )
    in
    repeat_until_ok poll_agent expire_time

  let runcmd transfer t (n : Node.real_node) cmd :
      (string, [> R.msg] * string) result Lwt.t =
    let%lwt () = Node.node_log n cmd in
    let expire_time = 12 * Node.rnode_get_expire_time n in
    match List.hd (String.split_on_char ' ' cmd) with
    | "RUN" ->
        send_command t.ip_address ~expire_time @@ String.sub cmd 4 (String.length cmd - 4)
    | "UPLOAD" ->
        let split_cmd = String.split_on_char ' ' cmd in
        send_command t.ip_address ~expire_time
        @@ transfer ~first_arg:(List.nth split_cmd 1)
             ~second_arg:(List.nth split_cmd 2) ~verb:`Put
    | "DOWNLOAD" ->
        let split_cmd = String.split_on_char ' ' cmd in
        send_command t.ip_address ~expire_time
        @@ transfer ~first_arg:(List.nth split_cmd 1)
             ~second_arg:(List.nth split_cmd 2) ~verb:`Get
    | _ ->
        let%lwt () =
          Node.node_log n "[ERROR] Invalid Command. Please try again."
        in
        Lwt.return
          (R.error (R.msg "Invalid Command.", "No log, invalid command."))

  let spindown t (n : Node.real_node) =
    let%lwt () = Node.node_log n "Node spinning down." in
    let details () =
      Aws_lwt.Runtime.run_request ~region:t.settings.aws_region ~access_key:t.aws_key
        ~secret_key:t.aws_secret
        (module TerminateInstances)
        (Types.TerminateInstancesRequest.make ~instance_ids:[t.instance_id] ())
    in
    let aux () : (TerminateInstances.output, [> R.msg ]) result Lwt.t =
      let%lwt d = details () in
      Lwt.return (aws_to_result d)
    in
    let%lwt results = repeat_until_ok aux 10 in
    (* TODO: we should check this. *)
    let _r = R.failwith_error_msg results in
    Lwt.return ()
end

module StubRunner = Runner (Stub)
module AwsRunner = Runner (Aws)

let invoke_node settings cwd guid node
    (transfer_fn : string -> [`Get | `Put] -> Uri.t) nocache deploy =
  let run node =
    match node with
    | Node.Snode _ ->
        let%lwt () =
          Lwt_io.printl "Node is synthetic so it finished successfully."
        in
        Lwt.return (R.ok true)
        (*TODO Handle other hypervisors.*)
    | Node.Rnode r ->
        AwsRunner.invoke settings cwd guid r transfer_fn nocache
  in
  match Node.node_has_keyword node Deploy && not deploy with
  | true ->
      let%lwt () =
        Lwt_io.printl
          "Node has the deploy keyword and the deploy flag isn't set, not \
           running."
      in
      Lwt.return (R.ok true)
  | false ->
      run node

(* TODO: Return a record with label fields for nodes. *)
(*TODO: Add a settings type here to reduce parameters.*)
let run settings cwd guid node_list
    (transfer_fn : string -> [`Get | `Put] -> Uri.t) nocache deploy =
  let rec aux complete_nodes failed_nodes
      (running_nodes : ((bool, [> R.msg]) result * Node.node) Lwt.t list)
      todo_nodes first_run =
    match (running_nodes, todo_nodes, first_run) with
    (*TODO: Investigate removing first_run, maybe not needed.*)
    | [], [], false ->
        Lwt.return (complete_nodes, failed_nodes, todo_nodes)
    | [], td, false ->
        Lwt.return (complete_nodes, failed_nodes, td)
    | running_nodes, todo_nodes, _ ->
        let%lwt done_nodes, still_running =
          match running_nodes with
          | [] ->
              Lwt.return ([], [])
          | _ ->
              Lwt.nchoose_split running_nodes
        in
        let finished_nodes, new_failed_nodes =
          List.fold_left
            (fun (ok, err) (p, n) ->
              match p with
              | Ok y ->
                  ((y, n) :: ok, err)
              | Error y ->
                  (ok, (y, n) :: err))
            ([], []) done_nodes
        in
        let new_todo =
          List.map
            (fun todo ->
              List.fold_left Node.update_node_dependencies todo finished_nodes)
            todo_nodes
        in
        let runnable =
          List.filter (fun todo -> 0 = Node.dependson_count todo) new_todo
        in
        let still_todo =
          List.filter (fun todo -> 0 <> Node.dependson_count todo) new_todo
        in
        let running =
          List.map
            (fun n ->
              let%lwt r =
                invoke_node settings cwd guid n transfer_fn nocache deploy
              in
              Lwt.return (r, n))
            runnable
        in
        aux
          (List.append (List.map snd finished_nodes) complete_nodes)
          (List.append (List.map snd new_failed_nodes) failed_nodes)
          (List.append running still_running)
          still_todo false
  in
  aux [] [] [] node_list true

let pre_presign ~bucket ~region ~duration =
  let%lwt _credentials =
    match%lwt Aws_s3_lwt.Credentials.Helper.get_credentials () with
    | Ok x ->
        Lwt.return x
    | Error e ->
        raise e
  in
  let credentials =
    Aws_s3.Credentials.make
      ~access_key:_credentials.Aws_s3.Credentials.access_key
      ~secret_key:(String.trim _credentials.Aws_s3.Credentials.secret_key)
      ()
  in
  let date =
    match Ptime.of_float_s (Unix.gettimeofday ()) with
    | Some x ->
        x
    | None ->
        raise
          (Incorrect_Time_Format "Current time isn't formatted correctly!?")
  in
  let region = Aws_s3.Region.of_string region in
  Lwt.return (fun path (verb : [`Get | `Put]) ->
      Aws_s3.Authorization.make_presigned_url ~credentials ~date ~region
        ~bucket ~duration ~path ~verb ())

let pre_source (settings : Settings.t) cwd guid
    (transfer_fn : string -> [`Get | `Put] -> Uri.t) =
  let%lwt () = Lwt_io.printl "Uploading source for agents." in
  let temp = Filename.temp_file "makecloud_" ".tar" in
  let%lwt _pout =
    let excludes =
      List.map (sprintf "--exclude=\'%s\'") settings.ignored_files
      |> String.concat " "
    in
    let cmd = sprintf "tar cf %s %s -C %s ." temp excludes cwd in
    let%lwt () = Lwt_io.printl cmd in
    Lwt_process.pread
      (Lwt_process.shell cmd)
  in
  let upload_url =
    transfer_fn (sprintf "/%s/source.tar" (Uuidm.to_string guid)) `Put
  in
  let upload_cmd = (sprintf "curl --retry 5 -X PUT \'%s\' --upload-file %s"
            (Uri.to_string upload_url) temp)
  in
  let%lwt _pout =
    Lwt_process.pread
      (Lwt_process.shell upload_cmd)
  in
  let%lwt () = Lwt_io.printl "Finished uploading source for agents." in
  Lwt.return @@ Ok ()

let start_checks () = Settings.key_check ()

let prune_nodes ns target_nodes =
  let node_edges = List.map Node.get_edges ns |> List.concat in
  let rev_edges = List.map (fun (x, y) -> (y, x)) node_edges in
  let rec aux edges (targets : string list) =
    let valid_edges =
      List.filter
        (fun (dst, _) -> List.exists (String.equal dst) targets)
        edges
    in
    let new_targets =
      List.fold_left
        (fun ts (_, src) ->
          match List.exists (String.equal src) ts with
          | true ->
              ts
          | false ->
              src :: ts)
        targets valid_edges
    in
    if List.length targets = List.length new_targets then targets
    else aux edges new_targets
  in
  match target_nodes with
  | [] ->
      ns
  | target_nodes ->
      let targets = aux rev_edges target_nodes in
      List.filter
        (fun x -> List.exists (String.equal (Node.node_to_string x)) targets)
        ns

let main repo_dir nocache deploy target_nodes =
  let () = start_checks () in
  let guid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  let%lwt () =
    Lwt_io.printl ("GUID for this run is (" ^ Uuidm.to_string guid ^ ")")
  in
  let settings =
    Settings.parse_settings
      (Fpath.add_seg (Fpath.v repo_dir) "mc_settings.yml")
  in
  let%lwt transfer_fn =
    pre_presign ~bucket:settings.storage_bucket ~duration:86400 ~region:settings.bucket_region
  in
  let%lwt new_configs = get_configs repo_dir in
  (*TODO Handle printing exceptions better, maybe use Fmt?*)
  let nodes = R.failwith_error_msg (parse_configs new_configs) in
  let%lwt () =
    match target_nodes with
    | [] -> Lwt_io.printl "Running all nodes due to no targetting being selected."
    | _ -> Lwt_io.printl "Running only select nodes due to targetting."
  in
  let runable_nodes = prune_nodes nodes target_nodes in
  let%lwt pre_results = pre_source settings repo_dir guid transfer_fn in
  let key = Sys.getenv "MC_KEY" in
  (*TODO Handle failing to upload our source bundle.*)
  let () = R.get_ok pre_results in
  let%lwt () = Notify.send_run_state ~settings ~guid ~key Notify.RunStart in
  match%lwt
    run settings repo_dir guid runable_nodes transfer_fn nocache deploy
  with
  | exception e ->
      let%lwt () = Notify.send_run_state ~settings ~guid ~key Notify.RunException in
      let%lwt () =
        Lwt_io.printf
          "Something has gone wrong, we received in exception:\n %s \n"
          (Printexc.to_string e)
      in
      raise e
  | _, [], [] ->
      let%lwt () = Notify.send_run_state ~settings ~guid ~key Notify.RunSuccess in
      Lwt_io.printl "All nodes completed successfully."
  | complete, failed, todo ->
      let%lwt () = Notify.send_run_state ~settings ~guid ~key Notify.RunFail in
      let completed_names =
        List.map Node.node_to_string complete |> String.concat " "
      in
      let failed_names =
        List.map Node.node_to_string failed |> String.concat " "
      in
      let todo_names =
        List.map Node.node_to_string todo |> String.concat " "
      in
      let%lwt () =
        Lwt_io.printl
          (sprintf
             "\n\n\
              **********\n\
              Finished: %s\n\
              Failed: %s\n\
              Incomplete: %s\n\
              **********\n\n"
             completed_names failed_names todo_names)
      in
      exit 10
