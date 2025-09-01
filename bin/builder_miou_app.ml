(* BEGIN: copy from miragevpn *)
let reporter_with_ts ~dst () =
  let pp_tags f tags =
    let pp tag () =
      let (Logs.Tag.V (def, value)) = tag in
      Format.fprintf f " %s=%a" (Logs.Tag.name def) (Logs.Tag.printer def) value;
      ()
    in
    Logs.Tag.fold pp tags ()
  in
  let report src level ~over k msgf =
    let tz_offset_s = Ptime_clock.current_tz_offset_s () in
    let posix_time = Ptime_clock.now () in
    let src = Logs.Src.name src in
    let k _ =
      over ();
      k ()
    in
    msgf @@ fun ?header ?tags fmt ->
    Format.kfprintf k dst
      ("%a:%a %a [%s] @[" ^^ fmt ^^ "@]@.")
      (Ptime.pp_rfc3339 ?tz_offset_s ())
      posix_time
      Fmt.(option ~none:(any "") pp_tags)
      tags Logs_fmt.pp_header (level, header) src
  in
  { Logs.report }

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs_threaded.enable ();
  Logs.set_reporter (reporter_with_ts ~dst:Format.std_formatter ())
(* END: copy from miragevpn *)

let main () =
  Miou_unix.run @@ fun () ->
  let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 3000)) in
  let cfg = Vif.config sockaddr in
  Caqti_miou.Switch.run @@ fun sw ->
  let datadir = Fpath.v "_builder" in
  let configdir = Fpath.v "/etc/builder-web" in
  let uri = Uri.make ~scheme:"sqlite3" ~path:"_builder/builder.sqlite3"
    ~query:["create", ["false"]] () in
  try
    Vif.run ~cfg ~devices:Vif.Devices.[ Builder_miou.caqti ]
      ~middlewares:Vif.Middlewares.[ Builder_miou.auth_middleware ]
      (Builder_miou.routes ())
      { Builder_miou.sw; uri; datadir; configdir; filter_builds_later_than= 32 }
  with Builder_miou.Wrong_version (appid, version) ->
    if appid = Builder_db.application_id
    then Printf.eprintf "Wrong database version: %Lu, expected %Lu"
           version Builder_db.current_version
    else Printf.eprintf "Wrong database application id: %lu, expected %lu"
           appid Builder_db.application_id;
    exit 2
open Cmdliner

let () =
  setup_log None None;
  let cmd =
    let info = Cmd.info "builder-miou" in
    Cmd.v info Term.(const main $ Mirage_logs_cli.setup)
  in
  exit (Cmd.eval cmd)
