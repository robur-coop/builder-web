open Opium

open Lwt.Infix

let safe_close fd =
  Lwt.catch
    (fun () -> Lwt_unix.close fd)
    (fun _ -> Lwt.return_unit)

let connect addrtype sockaddr =
  let c = Lwt_unix.(socket addrtype SOCK_STREAM 0) in
  Lwt_unix.set_close_on_exec c ;
  Lwt.catch (fun () ->
      Lwt_unix.(connect c sockaddr) >|= fun () ->
      Some c)
    (fun e ->
       Logs.warn (fun m -> m "error %s connecting to influx"
                     (Printexc.to_string e));
       safe_close c >|= fun () ->
       None)

let write_raw s buf =
  let rec w off l =
    Lwt.catch (fun () ->
        Lwt_unix.send s buf off l [] >>= fun n ->
        if n = l then
          Lwt.return (Ok ())
        else
          w (off + n) (l - n))
      (fun e ->
         Logs.err (fun m -> m "exception %s while writing" (Printexc.to_string e)) ;
         safe_close s >|= fun () ->
         Error `Exception)
  in
  (*  Logs.debug (fun m -> m "writing %a" Cstruct.hexdump_pp (Cstruct.of_bytes buf)) ; *)
  w 0 (Bytes.length buf)

let process =
  Metrics.field ~doc:"name of the process" "vm" Metrics.String

let init_influx name data =
  match data with
  | None -> ()
  | Some (ip, port) ->
    Logs.info (fun m -> m "stats connecting to %a:%d" Ipaddr.V4.pp ip port);
    Metrics.enable_all ();
    Metrics_lwt.init_periodic (fun () -> Lwt_unix.sleep 10.);
    Metrics_lwt.periodically (Metrics_rusage.rusage_src ~tags:[]);
    Metrics_lwt.periodically (Metrics_rusage.kinfo_mem_src ~tags:[]);
    let get_cache, reporter = Metrics.cache_reporter () in
    Metrics.set_reporter reporter;
    let fd = ref None in
    let rec report () =
      let send () =
        (match !fd with
         | Some _ -> Lwt.return_unit
         | None ->
           let addr = Lwt_unix.ADDR_INET (Ipaddr_unix.V4.to_inet_addr ip, port) in
           connect Lwt_unix.PF_INET addr >|= function
           | None -> Logs.err (fun m -> m "connection failure to stats")
           | Some fd' -> fd := Some fd') >>= fun () ->
        match !fd with
        | None -> Lwt.return_unit
        | Some socket ->
          let tag = process name in
          let datas = Metrics.SM.fold (fun src (tags, data) acc ->
              let name = Metrics.Src.name src in
              Metrics_influx.encode_line_protocol (tag :: tags) data name :: acc)
              (get_cache ()) []
          in
          let datas = String.concat "" datas in
          write_raw socket (Bytes.unsafe_of_string datas) >|= function
          | Ok () -> ()
          | Error `Exception ->
            Logs.warn (fun m -> m "error on stats write");
            fd := None
      and sleep () = Lwt_unix.sleep 10.
      in
      Lwt.join [ send () ; sleep () ] >>= report
    in
    Lwt.async report

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

let setup_app () influx port host datadir =
  let dbpath = Printf.sprintf "%s/builder.sqlite3" datadir in
  let datadir = Fpath.v datadir in
  let () = init_influx "builder-web" influx in
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

let ip_port : (Ipaddr.V4.t * int) Arg.converter =
  let default_port = 8094 in
  let parse s =
    match
      match String.split_on_char ':' s with
      | [ s ] -> Ok (s, default_port)
      | [ip; port] -> begin match int_of_string port with
        | exception Failure _ -> Error "non-numeric port"
        | port -> Ok (ip, port)
      end
        | _ -> Error "multiple : found"
    with
    | Error msg -> `Error msg
    | Ok (ip, port) -> match Ipaddr.V4.of_string ip with
      | Ok ip -> `Ok (ip, port)
      | Error `Msg msg -> `Error msg
  in
  parse, fun ppf (ip, port) -> Format.fprintf ppf "%a:%d" Ipaddr.V4.pp ip port

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

let influx =
  let doc = "IP address and port (default: 8094) to report metrics to in influx line protocol" in
  Arg.(value & opt (some ip_port) None & info [ "influx" ] ~doc ~docv:"INFLUXHOST[:PORT]")


let () =
  let () = Mirage_crypto_rng_unix.initialize () in
  let term = Term.(pure setup_app $ setup_log $ influx $ port $ host $ datadir) in
  let info = Term.info "Builder web" ~doc:"Builder web" ~man:[] in
  match Term.eval (term, info) with
  | `Ok s ->
    Printexc.record_backtrace true;
    let () = Lwt.async (fun () -> Lwt.bind s (fun _ -> Lwt.return_unit)) in
    let forever, _ = Lwt.wait () in
    Lwt_main.run forever
  | `Error _ -> exit 1
  | _ -> exit 0
