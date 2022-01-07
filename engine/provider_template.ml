open Lib

module type Provider = sig
  type t

  (*TODO: Investigate if we can bring Node.real_node into t*)
  val spinup :
    Lib.run_parameters -> Settings.t -> Node.real_node -> string -> t Lwt.t

  val set_env : t -> Node.real_node -> (string * string) list -> unit Lwt.t
  val wait_until_ready : t -> Node.real_node -> unit -> unit option Lwt.t

  val runcmd :
    (first_arg:string -> second_arg:string -> verb:verb -> string) ->
    t ->
    Lib.run_parameters ->
    Settings.t ->
    Node.real_node ->
    string ->
    Command.t ->
    (string, [> R.msg ] * string) result Lwt.t

  val spindown : t -> Node.real_node -> unit Lwt.t
end
