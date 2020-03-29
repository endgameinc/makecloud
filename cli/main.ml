let sprintf = Printf.sprintf

module R = Rresult.R
open Engine.Lib

let main repo_dir nocache deploy target_nodes =
  let params = Engine.Lib.make_params ~repo_dir ~nocache ~deploy ~target_nodes in
  Lwt_main.run (Engine.Runner.main params)

let check repo_dir =
  let aux () =
    let () = Engine.Runner.start_checks () in
    let _settings =
      Engine.Settings.parse_settings
        (Fpath.add_seg (Fpath.v repo_dir) "mc_settings.yml")
    in
    let%lwt new_configs = Engine.Runner.get_configs repo_dir in
    (*TODO Handle printing exceptions better, maybe use Fmt?*)
    let _nodes =
      R.failwith_error_msg (Engine.Runner.parse_configs new_configs)
    in
    Lwt_io.printl "Looks good."
  in
  Lwt_main.run (aux ())

let check_configs_main cwd =
  Lwt_main.run
    (let%lwt configs = Engine.Runner.get_configs cwd in
     Lwt_list.iter_s Lwt_io.printl configs)

let purge repo_dir target_hash () =
    let%lwt creds = Aws_s3_lwt.Credentials.Helper.get_credentials () in
    let credentials = R.get_ok creds in
     let settings =
       Engine.Settings.parse_settings
         (Fpath.add_seg (Fpath.v repo_dir) "mc_settings.yml")
    in
    let region = Aws_s3.Region.of_string settings.bucket_region in
    let endpoint =
      Aws_s3.Region.endpoint ~inet:`V4 ~scheme:`Https region
    in
    let check key =
      Aws_s3_lwt.S3.retry ~endpoint ~retries:5
        ~f:(fun ~endpoint () ->
          Aws_s3_lwt.S3.delete ~bucket:settings.storage_bucket ~key ~endpoint ~credentials ())
        ()
    in
    let _ = Lwt_list.map_p check target_hash in
    Lwt.return ()

let purge_main repo_dir target_hash =
    Lwt_main.run (
        purge repo_dir target_hash ()
    )

let check_cache_per_node (node_name : string) cwd =
  Lwt_main.run
    (let%lwt new_configs = Engine.Runner.get_configs cwd in
     let nodes =
       R.failwith_error_msg (Engine.Runner.parse_configs new_configs)
     in
     match
       List.find_opt
         (fun (x : Engine.Node.node) ->
           Engine.Node.node_to_string x = node_name)
         nodes
     with
     | None ->
         let%lwt () = Lwt_io.printl "No such node." in
         exit 11
     | Some node -> (
       match node with
       | Snode _ ->
           Lwt_io.printl "Synthetic nodes don't have a hash."
       | Rnode node ->
           Engine.Node.print_hash_breakdown cwd node ))

let dot_main repo_dir =
  Lwt_main.run
    (let%lwt () = Lwt_unix.chdir repo_dir in
     let%lwt new_configs = Engine.Runner.get_configs repo_dir in
     (*TODO Handle printing exceptions better, maybe use Fmt?*)
     let nodes =
       R.failwith_error_msg (Engine.Runner.parse_configs new_configs)
     in
     let pairs = List.map Engine.Node.get_edges nodes |> List.concat in
     let content =
       List.map (fun (f, s) -> sprintf "\"%s\" -> \"%s\";\n" f s) pairs
     in
     Lwt_io.printf "digraph { \n%s}\n" (String.concat "" content))

let show_all_cache repo_dir =
  Lwt_main.run
    (let%lwt () = Lwt_unix.chdir repo_dir in
     let%lwt new_configs = Engine.Runner.get_configs repo_dir in
     let settings =
       Engine.Settings.parse_settings
         (Fpath.add_seg (Fpath.v repo_dir) "mc_settings.yml")
     in
     (*TODO Handle printing exceptions better, maybe use Fmt?*)
     let nodes =
       R.failwith_error_msg (Engine.Runner.parse_configs new_configs)
     in
     let rnodes =
       List.map
         (fun x ->
           match x with
           | Engine.Node.Rnode r ->
               [r]
           | Engine.Node.Snode _ ->
               [])
         nodes
       |> List.concat
     in
     let cnodes = List.filter Engine.Node.is_node_cachable rnodes in
     let print_node_cache (n : Engine.Node.real_node) =
       let name = Engine.Node.node_to_string (Engine.Node.Rnode n) in
       let%lwt rstatus =
         Engine.Runner.AwsRunner.check_cache ~settings ~cwd:repo_dir ~n
       in
       let status = R.is_ok rstatus in
       let%lwt hash = Engine.Node.hash_of_node repo_dir n in
       Lwt_io.printf "%s\t%B\t%s\n" name status hash
     in
     Lwt_list.iter_s print_node_cache cnodes)

open Cmdliner

let make_repo_dir p =
  let doc = "Path to the repository to process." in
  Arg.(value & pos p string "." & info [] ~docv:"REPO_DIR" ~doc)

let node_name =
  let doc = "A Makecloud node name as defined in the makecloud.yaml files." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"NODE_NAME" ~doc)

let target_nodes =
  let doc =
    "A list of makecloud nodes that you want those and only those to run."
  in
  Arg.(value & pos_right 0 string [] & info [] ~docv:"TARGET_NODES" ~doc)

let target_hash =
  let doc =
    "A hash of a node to remove the cache of"
  in
  Arg.(value & pos_right 0 string [] & info [] ~docv:"TARGET_HASH" ~doc)

let nocache =
  let doc =
    "Don't use any historical caches. Used to force a fresh build. (will \
     store new caches though)"
  in
  Arg.(value & flag & info ["nocache"] ~doc)

let deploy =
  let doc =
    "allow nodes with keyword \"deploy\" to be run, otherwise they will not \
     be run."
  in
  Arg.(value & flag & info ["d"; "deploy"] ~doc)

let make_info term_name doc =
  let man =
    [ `S Manpage.s_bugs
    ; `P "Please report bugs to the project's github issue tracker." ]
  in
  Term.info term_name ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits
    ~man

let invoke =
  let info =
    make_info "invoke" "run the series of nodes to build a repository."
  in
  let term =
    Term.(const main $ make_repo_dir 0 $ nocache $ deploy $ target_nodes)
  in
  (term, info)

let check =
  let info =
    make_info "check" "run all the pre-steps but don't actually run it."
  in
  let term = Term.(const check $ make_repo_dir 0) in
  (term, info)

let check_configs =
  let info =
    make_info "check-config"
      "print all makecloud configurations in this directory."
  in
  let term = Term.(const check_configs_main $ make_repo_dir 0) in
  (term, info)

let check_cache =
  let info =
    make_info "check-cache"
      "print the node hash and the file hash that are composed into a node's \
       overall hash (mostly for debugging.)"
  in
  let term = Term.(const check_cache_per_node $ node_name $ make_repo_dir 1) in
  (term, info)

let dot =
  let info =
    make_info "dot"
      "prints out the graph of nodes in dot syntax, renderable with graphviz."
  in
  let term = Term.(const dot_main $ make_repo_dir 0) in
  (term, info)

let show_all_cache =
  let info =
    make_info "show-all-cache"
      "print all nodes that can cache, their status, and current hash"
  in
  let term = Term.(const show_all_cache $ make_repo_dir 0) in
  (term, info)

let purge_cache =
    let info =
        make_info "purge-cache"
        "purges one node's cached files for build"
    in
    let term = Term.(const purge_main $ make_repo_dir 0 $ target_hash) in
    (term, info)

let help =
  let term =
    Cmdliner.Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ()))
  in
  (term, Cmdliner.Term.info "mc")

let cmds = [invoke; check_configs; check_cache; dot; show_all_cache; check; purge_cache]

let () = Term.(exit @@ eval_choice help cmds)
