open Lib

module Stub : Provider_template.Provider = struct
  type t = unit

  let spinup _settings (n : Node.real_node) _guid : t Lwt.t =
    let%lwt () =
      Lwt_io.printl ("[STUB]" ^ "[" ^ n.name ^ "]" ^ "Node Spinning Up")
    in
    Lwt.return ()

  let wait_until_ready _box (n : Node.real_node) () =
    let _ = Lwt_io.printl ("[STUB]" ^ "[" ^ n.name ^ "]" ^ " Ready!") in
    Lwt.return (Some ())

  let set_env _box _n = Lwt.return ()

  let runcmd _transfer_fn _settings _box (n : Node.real_node) _guid cmd =
    let str_cmd = Command.to_string cmd in
    let _ = Lwt_io.printl ("[STUB]" ^ "[" ^ n.name ^ "]" ^ str_cmd) in
    Lwt.return (R.ok "")

  let spindown _ (n : Node.real_node) =
    let _ =
      Lwt_io.printl ("[STUB]" ^ "[" ^ n.name ^ "]" ^ "Node Spinning Down")
    in
    Lwt.return ()
end

