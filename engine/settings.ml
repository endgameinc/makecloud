let sprintf = Printf.sprintf

module R = Rresult.R

exception UriNeedsString of string

module Uri = struct
  include Uri

  let to_yaml u = `String (Uri.to_string u)

  let of_yaml_exn = function
    | `String s ->
        Uri.of_string s
    (*TODO Figure out if we can get rid of this exn.*)
    | _ ->
        raise (UriNeedsString "the url field in yaml must be a string.")
end

type t =
  { cachable: bool [@default true]
  ; ignored_files: string list [@default []]
  ; notify_url: Uri.t option [@default None]
  ; name: string option [@default None]
  ; storage_bucket: string
  ; aws_key_name: string
  ; aws_security_group: string
  ; aws_subnet_id: string
  ; linux_agent_url: Uri.t
  ; windows_agent_url: Uri.t
  ; only_public_ip: bool [@default false]}
[@@deriving protocol ~driver:(module Protocol_conv_yaml.Yaml)]

let parse_settings filepath =
  let aux () =
    let bind = R.bind in
    let%bind contents = Bos.OS.File.read filepath in
    let%bind yaml = Yaml.of_string contents in
    match of_yaml yaml with
    | Ok y ->
        R.ok y
    | Error yaml_error ->
        R.error_msg (Protocol_conv_yaml.Yaml.error_to_string_hum yaml_error)
  in
  R.failwith_error_msg (aux ())
