let uname =
  let cmd = Bos.Cmd.(v "uname" % "-s") in
  lazy (match Bos.OS.Cmd.(run_out cmd |> out_string |> success) with
      | Ok s when s = "FreeBSD" -> `FreeBSD
      | Ok s when s = "Linux" -> `Linux
      | Ok s -> invalid_arg (Printf.sprintf "OS %s not supported" s)
      | Error (`Msg m) -> invalid_arg m)

let default_datadir =
  match Lazy.force uname with
  | `FreeBSD -> "/var/db/builder-web"
  | `Linux -> "/var/lib/builder-web"

let default_cachedir =
  default_datadir ^ "/_cache"

let default_configdir =
  match Lazy.force uname with
  | `FreeBSD -> "/usr/local/etc/builder-web"
  | `Linux -> "/etc/builder-web"
