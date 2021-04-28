open Lib
open Aws_ec2

let missing_ami_err_msg = "AWS failed to generate an ami when requested, this in theory shouldn't happen."

module Aws : Provider_template.Provider = struct
  type t =
    { ip_address: string
    ; instance_id: string
    ; settings: Settings.t
    ; aws_key: string
    ; aws_secret: string
    ; aws_token: string option
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

  let apply_tags ~(settings : Settings.t) ~aws_key ~aws_secret ?token ~guid ~instance_id () =
    let tag_build = Types.Tag.make ~key:"Name" ~value:"Makecloud Builder" () in
    let tag_run = Types.Tag.make ~key:"Makecloud Run" ~value:guid () in
    let tags = [tag_build; tag_run] in
    let tags_req = Types.CreateTagsRequest.make ~resources:[instance_id] ~tags () in
    let%lwt result =
      Aws_lwt.Runtime.run_request
        ~region:settings.aws_region
        ~access_key:aws_key
        ~secret_key:aws_secret
        ?token
        (module CreateTags)
        tags_req
    in
    Lwt.return (result |> aws_to_result)

  let get_ami ~(params : Lib.run_parameters) ~(settings : Settings.t) ~guid past_stage =
    let%lwt creds = Aws_s3_lwt.Credentials.Helper.get_credentials ?profile:params.aws_profile () in
    let credentials = R.get_ok creds in
    let region = Aws_s3.Region.of_string settings.bucket_region in
    let endpoint =
      Aws_s3.Region.endpoint ~inet:`V4 ~scheme:`Https region
    in
    let key = Fmt.str "%s/%s/.ami_id" guid past_stage in
    let%lwt result = Aws_s3_lwt.S3.retry ~endpoint ~retries:5
      ~f:(fun ~endpoint () ->
        Aws_s3_lwt.S3.get ~bucket:settings.storage_bucket ~key ~endpoint
          ~credentials ())
      ()
    in
    Lwt.return @@ R.reword_error (fun _ -> `Msg "Failed to get ami in s3.") result

  let get_base ~params ~settings ~guid ~(n : Node.real_node) =
    if String.sub n.base 0 (min (String.length n.base) 3) = "ami" then
      Lwt.return n.base
    else
      let%lwt r = get_ami ~params ~settings ~guid n.base in
      Lwt.return (R.get_ok r)

  let spinup (params : Lib.run_parameters) (settings : Settings.t) (n : Node.real_node) guid : t Lwt.t =
    let%lwt () = Node.node_log n "Node Spinning Up" in
    (* TODO improve this to new cred method? maybe pass creds around *)
    let%lwt unsafe_creds = Aws_s3_lwt.Credentials.Helper.get_credentials ?profile:params.aws_profile () in
    let creds = unsafe_creds |> ok_or_raise in
    let aws_key = creds.access_key in
    let aws_secret = creds.secret_key in
    let aws_token = creds.token in
    let%lwt () = Node.node_log n "Got Credentials." in
    let%lwt user_data = make_user_data n settings in
    let%lwt base = get_base ~params ~settings ~guid ~n in
    let instance_params =
      (*TODO We should gen an ssh key, upload it to aws and use that instead of a constant key. *)
      (*TODO Pull instance size from the config file.*)
      Types.RunInstancesRequest.make
        ~image_id:base
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
          ?token:aws_token
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
    let%lwt _t = repeat_until_ok (apply_tags ~settings ~aws_key ~aws_secret ~instance_id ~guid) 5 in
    let get_details () =
      let result =
        Aws_lwt.Runtime.run_request ~region:settings.aws_region ~access_key:aws_key
          ~secret_key:aws_secret ?token:aws_token
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
    Lwt.return {ip_address; instance_id; settings; aws_key; aws_secret; aws_token }

  let set_env t (n : Node.real_node) additional_env =
    let uri = Uri.of_string ("http://" ^ t.ip_address ^ ":8000/set_env") in
    let all_envs = List.concat [n.env; additional_env] in
    let env = `Assoc (List.map (fun (k, v) -> (k, `String v)) all_envs) in
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
  let send_command cmd_uri s ~expire_time : (string, [> R.msg] * string) result Lwt.t =
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
      let%lwt resp, body = Cohttp_lwt_unix.Client.put cmd_uri ~headers ~body in
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
      match%lwt repeat_until_ok send_command 60 with
      | Ok (r, b) ->
          Lwt.return (r, b)
      | Error (`Msg message, note) ->
          Lwt.fail_with (Fmt.str
            "Can't talk to an agent, this probably means the agent failed to \
             install. Error: %s - %s"
             message note)
    in
    (*TODO cohttp_retry*)
    let poll_agent () =
      let check_uri =
        Uri.with_path cmd_uri ("/check_command")
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

  let save_box ~t ~(settings : Settings.t) ~(n : Node.real_node) ~guid ~instance_id () =
    let name = Fmt.str "makecloud-%s-%s" guid n.name in
    let image_req = Types.CreateImageRequest.make ~instance_id ~name () in
    let%lwt () = Node.node_log n (Fmt.str "Trying to save %s." instance_id) in
    let%lwt result =
      Aws_lwt.Runtime.run_request
        ~region:settings.aws_region
        ~access_key:t.aws_key
        ~secret_key:t.aws_secret
        ?token:t.aws_token
        (module CreateImage)
        image_req
    in
    Lwt.return (result |> aws_to_result)

  let store_ami ?profile ~(settings : Settings.t) ~(n : Node.real_node) ~guid ami_id =
    let%lwt creds = Aws_s3_lwt.Credentials.Helper.get_credentials ?profile () in
    let credentials = R.get_ok creds in
    let region = Aws_s3.Region.of_string settings.bucket_region in
    let endpoint =
      Aws_s3.Region.endpoint ~inet:`V4 ~scheme:`Https region
    in
    let key = Fmt.str "%s/%s/.ami_id" guid n.name in
    let%lwt result = Aws_s3_lwt.S3.retry ~endpoint ~retries:5
      ~f:(fun ~endpoint () ->
        Aws_s3_lwt.S3.put ~bucket:settings.storage_bucket ~key ~endpoint
          ~credentials ~data:ami_id ())
      ()
    in
    Lwt.return @@ R.reword_error (fun _ -> `Msg "Failed to store ami in s3.") result

  let check_on_ami ~t ~(settings : Settings.t) ~n image_id () =
    let ( let* ) = Lwt_result.bind in
    let ami_req = Types.DescribeImagesRequest.make ~image_ids:[image_id] () in
    let* result =
      Lwt.Infix.(Aws_lwt.Runtime.run_request
        ~region:settings.aws_region
        ~access_key:t.aws_key
        ~secret_key:t.aws_secret
        ?token:t.aws_token
        (module DescribeImages)
        ami_req >>= fun x ->
        Lwt.return (aws_to_result x))
    in
    (* this might be a bug, I don't think aws could forget about an ami that quickly
     * however, if they do so that would be bad and could raise a not found from the list.hd *)
    let ami = (Types.DescribeImagesResult.(result.images) |> List.hd) in
    let _ = Node.node_log n (Fmt.str "Checking on ami %s with state %s." image_id (Types.ImageState.to_string ami.state)) in
    Lwt.return @@ match ami.state with
    | Available  -> Ok image_id
    | Pending
    | Transient as e -> R.error_msgf "AMI is still not ready with reason %s."
      (Types.ImageState.to_string e)
    | Invalid
    | Deregistered
    | Failed
    | Error as e -> R.error_msgf "AMI has failed with reason %s."
      (Types.ImageState.to_string e)

  let publish_image ~profile ~t ~settings ~n ~guid =
    let instance_id = t.instance_id in
    let ( let* ) = Lwt_result.bind in
    let* box_id = repeat_until_ok (save_box ~t ~settings ~n ~instance_id ~guid) 20 in
    let none = (fun () -> R.error_msg missing_ami_err_msg) in
    let* image_id = Types.CreateImageResult.(box_id.image_id) |> R.of_option ~none |> Lwt.return
    in
    let* waiting_image = repeat_until_ok (check_on_ami ~t ~settings ~n image_id) 240 in
    let* _store_result = store_ami ?profile ~settings ~n ~guid waiting_image in
    Lwt.return_ok waiting_image

  let make_file_transfer_payload src dst =
    let json : Yojson.Safe.t = `Assoc [("src", `String src); ("dst", `String dst)] in
    Yojson.Safe.to_string json

  let runcmd transfer_fn t (params : Lib.run_parameters) (settings : Settings.t) (n : Node.real_node) guid (cmd : Command.t) :
      (string, [> R.msg] * string) result Lwt.t =
    let%lwt () = Node.node_log n (Command.to_string cmd) in
    let expire_time = 12 * Node.rnode_get_expire_time n in
    let base_uri = Uri.make ~scheme:"http" ~port:8000 ~host:t.ip_address () in
    match cmd with
    | Command.(Run shell_cmd) ->
      let u = Uri.with_path base_uri "/command" in
      send_command u ~expire_time shell_cmd
    | Upload (first_arg, second_arg) ->
      let u = Uri.with_path base_uri "/upload" in
      let uri = Uri.to_string (transfer_fn (sprintf "/%s/%s/%s" guid n.name second_arg) `Put) in
      let payload = make_file_transfer_payload first_arg uri in
      send_command u ~expire_time payload
    | Download (first_arg, second_arg)  ->
      let u = Uri.with_path base_uri "/download" in
      let uri = if Uri.of_string first_arg |> Uri.scheme |> Option.is_none then
          Uri.to_string (transfer_fn (sprintf "/%s/%s" guid first_arg) `Get)
        else
          first_arg
      in
      let payload = make_file_transfer_payload uri second_arg in
      send_command u ~expire_time payload
    | Publish ->
      let%lwt image_id = publish_image ~profile:params.aws_profile ~t ~settings ~n ~guid in
      (match image_id with
      | Ok i ->
        let%lwt () = Node.node_log n (Fmt.str "Saved instance as %s" i) in
        Lwt.return_ok i
      | Error e ->
        Lwt.return_error (e, missing_ami_err_msg))

  let spindown t (n : Node.real_node) =
    let%lwt () = Node.node_log n "Node spinning down." in
    let details () =
      Aws_lwt.Runtime.run_request ~region:t.settings.aws_region ~access_key:t.aws_key
        ~secret_key:t.aws_secret ?token:t.aws_token
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

