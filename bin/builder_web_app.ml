open Opium

let timestamp_reporter () =
  let report src level ~over k msgf =
    let k _ = over (); k () in
    msgf @@ fun ?header ?tags:_ fmt ->
    let posix_time = Ptime_clock.now () in
    let src_name = Logs.Src.name src in
    Format.kfprintf k Format.std_formatter
      ("%a [%s] %a @[" ^^ fmt ^^ "@]@.")
      (Ptime.pp_rfc3339 ()) posix_time src_name
      Logs.pp_header (level, header)
  in
  { Logs.report }

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (timestamp_reporter ()) (* (Logs_fmt.reporter ~dst:Format.std_formatter ()) *)

let app t =
  App.empty
  |> App.cmd_name "Builder Web"
  |> Builder_web.add_routes t

let setup_app () port host datadir =
  let dbpath = Printf.sprintf "%s/builder.sqlite3" datadir in
  let datadir = Fpath.v datadir in
  match Builder_web.init dbpath datadir with
  | Error (#Caqti_error.load as e) ->
    Format.eprintf "Error: %a\n%!" Caqti_error.pp e;
    exit 2
  | Error (#Builder_web.db_error | `Wrong_version _ as e) ->
    Format.eprintf "Error: %a\n%!" Builder_web.pp_error e;
    exit 1
  | Ok t ->
    app t
    |> App.port port
    |> App.host host
    |> (match Logs.level () with Some Debug -> (fun x -> x |> App.debug true |> App.verbose true) | Some Info -> App.verbose true | _ -> (fun x -> x))
    |> App.start

open Cmdliner

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let datadir =
  let doc = "data directory" in
  Arg.(value & opt dir "/var/db/builder-web/" & info [ "d"; "datadir" ] ~doc)

let port =
  let doc = "port" in
  Arg.(value & opt int 3000 & info [ "p"; "port" ] ~doc)

let host =
  let doc = "host" in
  Arg.(value & opt string "0.0.0.0" & info [ "h"; "host" ] ~doc)

let () =
  let () = Mirage_crypto_rng_unix.initialize () in
  let term = Term.(pure setup_app $ setup_log $ port $ host $ datadir) in
  let info = Term.info "Builder web" ~doc:"Builder web" ~man:[] in
  match Term.eval (term, info) with
  | `Ok s ->
    Printexc.record_backtrace true;
    let () = Lwt.async (fun () -> Lwt.bind s (fun _ -> Lwt.return_unit)) in
    let forever, _ = Lwt.wait () in
    Lwt_main.run forever
  | `Error _ -> exit 1
  | _ -> exit 0
