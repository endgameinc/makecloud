open Lib

exception Agent_Unauthorized of string

exception Generic_AWS_Error of string

exception Incorrect_Time_Format of string

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

module Runner (M : Provider_template.Provider) = struct
  let run_node ~(settings : Settings.t) ~params ~n
      ~(transfer_fn : string -> [`Get | `Put] -> Uri.t) ~guid :
      (string, [> R.msg] * string) result Lwt.t =
    let%lwt box = M.spinup params settings n guid in
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
        let tl = [ sprintf {|if exist C:\source rd /s /q C:\source|}
          ; {|powershell -command New-Item C:\source -ItemType "directory"|}
          ; {|powershell -command dir C:\|}
          ; {|tar -x -f C:\source.tar -C C:\source|}]
          |> List.map (fun x -> Command.(Run x))
        in
        Command.(Download (uri_str, {|C:\source.tar|})) :: tl
      else
        let tl = [ "rm -rf /source; mkdir /source"
          ; "sha256sum source.tar"
          ; "tar xf /source.tar -C /source" ]
          |> List.map (fun x -> Command.(Run x))
        in
        Command.(Download (uri_str, "/source.tar")) :: tl
    in
    (*TODO: We should handle failure here.*)
    let additional_env = [("GUID", guid)] in
    let%lwt () = M.set_env box n additional_env in
    let%lwt () =
      let state = Notify.make_start_commands "1.1.1.1" "foobar-secret" in
      Notify.send_state ~settings ~guid ~node:(Node.Rnode n) ~key state
    in
    let%lwt result =
      Lwt_list.fold_left_s
        (fun a x ->
          match a with
          | Ok logs, old_logs ->
              let%lwt r = M.runcmd box params settings n guid x in
              (* TODO: Use Buffer module to accumulate strings *)
              Lwt.return (r, old_logs ^ logs ^ (Command.to_string x) ^ "\n")
          | Error (err, logs), old_logs ->
              Lwt.return (Error (err, ""), old_logs ^ logs))
        (R.ok "", "")
        (List.append prep_steps n.steps)
    in
    let%lwt () = if not (List.exists (String.equal n.name) params.dont_delete) then
        M.spindown box n
      else
        let%lwt () = Node.node_log n (Fmt.str "NOT deleting box %s as per requested." n.name) in
        Lwt.return ()
    in
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
  let store_logs ~(params : Lib.run_parameters) ~(settings : Settings.t) ~guid ~(n : Node.real_node)
      ~console_logs =
    let%lwt creds = Aws_s3_lwt.Credentials.Helper.get_credentials ?profile:params.aws_profile () in
    let credentials : Aws_s3.Credentials.t = R.get_ok creds in
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

  let store_cache ~(params : Lib.run_parameters) ~(settings : Settings.t) ~guid ~cwd ~n =
    let%lwt key = Node.hash_of_node cwd n in
    let%lwt creds = Aws_s3_lwt.Credentials.Helper.get_credentials ?profile:params.aws_profile () in
    let credentials : Aws_s3.Credentials.t = R.get_ok creds in
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

  let check_cache ?profile ~(settings : Settings.t) ~cwd ~n =
    let%lwt hash = Node.hash_of_node cwd n in
    let%lwt creds = Aws_s3_lwt.Credentials.Helper.get_credentials ?profile () in
    let safe_creds = R.get_ok creds in
    let region = Aws_s3.Region.of_string settings.bucket_region in
    let endpoint =
      Aws_s3.Region.endpoint ~inet:`V4 ~scheme:`Https region
    in
    Aws_s3_lwt.S3.retry ~endpoint ~retries:5
      ~f:(fun ~endpoint () ->
        Aws_s3_lwt.S3.get ~bucket:settings.storage_bucket ~key:hash ~endpoint
      ~credentials:safe_creds ()) ()

  let transfer_file ~(params : Lib.run_parameters) ~(settings : Settings.t) ~old_guid ~new_guid ~(n : Node.real_node) filename :
      (unit, Aws_s3_lwt.S3.error) result Lwt.t =
    let%lwt creds = Aws_s3_lwt.Credentials.Helper.get_credentials ?profile:params.aws_profile () in
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

  let process_cache ~params ~(settings : Settings.t) ~old_guid ~new_guid ~(n : Node.real_node) =
    let%lwt () = Lwt_io.printl (n.color ^ "Processing cache for " ^ n.name) in
    let key = Sys.getenv "MC_KEY" in
    let%lwt () = Notify.send_state ~settings ~guid:new_guid ~node:(Node.Rnode n) ~key ProcessCache in
    let upload_steps = List.filter Command.cache_command n.steps in
    let%lwt transfers =
      Lwt_list.map_p
        (transfer_file ~params ~settings ~old_guid ~new_guid ~n)
        (List.map Command.src_files upload_steps |> List.concat)
    in
    Lwt.return
      (List.filter
         (fun x -> match x with Error _ -> true | Ok _ -> false)
         transfers)

  let invoke settings (params : Lib.run_parameters) (n : Node.real_node)
      (transfer_fn : string -> [`Get | `Put] -> Uri.t) :
      (bool, [> R.msg]) result Lwt.t =
    let is_cachable = Node.is_node_cachable n in
    let%lwt cache_status = check_cache ?profile:params.aws_profile ~settings ~cwd:params.repo_dir ~n in
    let guid = Uuidm.to_string params.guid in
    match is_cachable && R.is_ok cache_status && not params.nocache with
    | true -> (
        let cache_guid = R.get_ok cache_status in
        match%lwt process_cache ~params ~settings ~old_guid:cache_guid ~new_guid:guid ~n with
        | [] ->
            Lwt_result.return true
        (* TODO: this should handle errors better *)
        | _ ->
            Lwt.return
              (R.error_msg "Cache processing failed for some reason. Unusual.")
        )
    | false -> (
        match%lwt run_node ~settings ~params ~n ~transfer_fn ~guid with
        | Ok console_logs ->
            let%lwt () = Lwt_io.printl "Steps completed successfully." in
            let%lwt () = store_logs ~params ~settings ~guid ~n ~console_logs in
            let%lwt () = store_cache ~params ~settings ~guid ~cwd:params.repo_dir ~n in
            Lwt.return (R.ok false)
        | Error (msg, console_logs) ->
            let%lwt () = Lwt_io.printl "Steps didn't complete successfully." in
            let%lwt () = store_logs ~params ~settings ~guid ~n ~console_logs in
            Lwt.return (R.error msg) )
end

let ok_or_raise_aws = function
  | `Ok x ->
      x
  | `Error e ->
      raise (Generic_AWS_Error (Aws.Error.format Aws_ec2.Errors_internal.to_string e))

exception No_value of string

module StubRunner = Runner (Provider_stub.Stub)
module AwsRunner = Runner (Provider_aws.Aws)

let invoke_node settings params node
    (transfer_fn : string -> [`Get | `Put] -> Uri.t) =
  let run node =
    match node with
    | Node.Snode _ ->
        let%lwt () =
          Lwt_io.printl "Node is synthetic so it finished successfully."
        in
        Lwt.return (R.ok true)
        (*TODO Handle other hypervisors.*)
    | Node.Rnode r ->
        AwsRunner.invoke settings params r transfer_fn
  in
  match Node.node_has_keyword node Deploy && not params.deploy with
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
let run settings params node_list
    (transfer_fn : string -> [`Get | `Put] -> Uri.t) =
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
                invoke_node settings params n transfer_fn
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

let pre_presign ~params ~bucket ~region ~duration =
  let%lwt credentials =
    match%lwt Aws_s3_lwt.Credentials.Helper.get_credentials ?profile:params.aws_profile () with
    | Ok x ->
        Lwt.return x
    | Error e ->
        raise e
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
    let cmd = sprintf "tar chf %s %s -C %s ." temp excludes cwd in
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

let main (params : run_parameters) =
  let () = start_checks () in
  let guid = params.guid in
  let%lwt () =
    Lwt_io.printl ("GUID for this run is (" ^ Uuidm.to_string params.guid ^ ")")
  in
  let settings =
    Settings.parse_settings
      (Fpath.add_seg (Fpath.v params.repo_dir) "mc_settings.yml")
  in
  let%lwt transfer_fn =
    pre_presign ~params ~bucket:settings.storage_bucket ~duration:86400 ~region:settings.bucket_region
  in
  let%lwt new_configs = get_configs params.repo_dir in
  (*TODO Handle printing exceptions better, maybe use Fmt?*)
  let nodes = R.failwith_error_msg (parse_configs new_configs) in
  let%lwt () =
    match params.target_nodes with
    | [] -> Lwt_io.printl "Running all nodes due to no targetting being selected."
    | _ -> Lwt_io.printl "Running only select nodes due to targetting."
  in
  let runable_nodes = prune_nodes nodes params.target_nodes in
  let%lwt pre_results = pre_source settings params.repo_dir params.guid transfer_fn in
  let key = Sys.getenv "MC_KEY" in
  (*TODO Handle failing to upload our source bundle.*)
  let () = R.get_ok pre_results in
  let node_names = List.map (fun x -> Node.node_to_string x) runable_nodes in
  let edges = List.map Node.get_edges runable_nodes |> List.concat in
  let%lwt () = Notify.send_run_state ~settings ~guid ~key (Notify.make_run_start node_names edges) in
  match%lwt
    run settings params runable_nodes transfer_fn
  with
  | exception e ->
      let%lwt () = Notify.send_run_state ~settings ~guid ~key Notify.RunException in
      let%lwt () =
        Lwt_io.printf
          "Something has gone wrong for run %s, we received in exception:\n %s \n"
          (Uuidm.to_string guid) (Printexc.to_string e)
      in
      raise e
  | _, [], [] ->
      let%lwt () = Notify.send_run_state ~settings ~guid ~key Notify.RunSuccess in
      Lwt_io.printf "All nodes for run (%s) completed successfully.\n" (Uuidm.to_string guid)
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
              GUID: %s\n\
              **********\n\
              Finished: %s\n\
              Failed: %s\n\
              Incomplete: %s\n\
              **********\n\n"
             (Uuidm.to_string guid) completed_names failed_names todo_names)
      in
      exit 10
