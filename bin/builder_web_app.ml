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

let setup_log style_renderer () =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs_threaded.enable ();
  Logs.set_reporter (reporter_with_ts ~dst:Format.std_formatter ())
(* END: copy from miragevpn *)

let main () port datadir configdir filter_builds_later_than =
  Miou_unix.run @@ fun () ->
  (* TODO: host argument *)
  let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  let cfg = Vif.config sockaddr in
  Caqti_miou.Switch.run @@ fun sw ->
  let datadir = Fpath.v datadir and configdir = Fpath.v configdir in
  let uri =
    let path = Fpath.(datadir / "builder.sqlite3" |> to_string) in
    Uri.make ~scheme:"sqlite3" ~path ~query:["create", ["false"]] () in
  try
    Vif.run ~cfg ~devices:Vif.Devices.[ Builder_web.caqti ]
      ~middlewares:Vif.Middlewares.[ Builder_web.auth_middleware ]
      (Builder_web.routes ())
      { Builder_web.sw; uri; datadir; configdir; filter_builds_later_than }
  with Builder_web.Wrong_version (appid, version) ->
    if appid = Builder_db.application_id
    then Printf.eprintf "Wrong database version: %Lu, expected %Lu"
           version Builder_db.current_version
    else Printf.eprintf "Wrong database application id: %lu, expected %lu"
           appid Builder_db.application_id;
    exit 2
open Cmdliner

let port =
  let doc = "port" in
  Arg.(value & opt int 3000 & info [ "p"; "port" ] ~doc)

let datadir =
  let doc = "data directory" in
  let docv = "DATA_DIR" in
  let env = Cmdliner.Cmd.Env.info "BUILDER_WEB_DATADIR" in
  Arg.(
    value &
    opt dir Builder_system.default_datadir &
    info ~env [ "d"; "datadir" ] ~doc ~docv
  )

let configdir =
  let doc = "config directory" in
  let docv = "CONFIG_DIR" in
  Arg.(
    value &
    opt dir Builder_system.default_configdir &
    info [ "c"; "configdir" ] ~doc ~docv)

let expired_jobs =
  let doc = "Amount of days after which a job is considered to be inactive if \
             no successful build has been achieved (use 0 for infinite)" in
  Arg.(value & opt int 30 & info [ "expired-jobs" ] ~doc)

let () =
  let setup_log =
    Term.(const setup_log $ Fmt_cli.style_renderer () $ Mirage_logs_cli.setup) in
  let cmd =
    let info = Cmd.info "builder-miou" ~doc:"Builder web" in
    Cmd.v info Term.(const main $ setup_log $ port $ datadir $ configdir $ expired_jobs)
  in
  exit (Cmd.eval cmd)
