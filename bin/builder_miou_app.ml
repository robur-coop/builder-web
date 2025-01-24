module Log_404 =
  (val Logs.(src_log @@
             Src.create ~doc:"builder-web unrouted requests" "builder-web.unrouted")
    : Logs.LOG)

let default target server _req _ =
  Log_404.debug (fun m -> m "Unrouted request for %S" target);
  Vif.Response.with_string server `Not_found ""

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
  Logs.set_reporter (reporter_with_ts ~dst:Format.std_formatter ())
(* END: copy from miragevpn *)

let main () =
  Miou_unix.run @@ fun () ->
  Logs.debug (fun m -> m "Starting miou stuff");
  let sockaddr = Unix.(ADDR_INET (inet_addr_loopback, 3000)) in
  let cfg = Vif.config sockaddr in
  Caqti_miou.Switch.run @@ fun sw ->
  (* XXX: This fails!? *)
  Caqti_miou.Switch.check sw;
  match
    Caqti_miou_unix.connect_pool  ~sw
      (Uri.make ~scheme:"sqlite3" ~path:"_builder/builder.sqlite3"
         ~query:["create", ["false"]] ())
  with
  | Error e ->
    Logs.err (fun m -> m "Database connect error: %a" Caqti_error.pp e);
    exit 2
  | Ok db ->
    let devices =
      Vif.[
        D.rng;
        Builder_miou.filter_builds_later_than_as_arg;
      ]
    in
    Vif.run ~cfg ~default ~devices (Builder_miou.routes ()) db

open Cmdliner
let () =
  setup_log None None;
  let cmd =
    let info = Cmd.info "builder-miou" in
    Cmd.v info Term.(const main $ Mirage_logs_cli.setup)
  in
  exit (Cmd.eval cmd)
