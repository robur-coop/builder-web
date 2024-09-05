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
  (*  Logs.debug (fun m -> m "writing %a" (Ohex.pp_hexdump ()) (Bytes.unsafe_to_string buf)) ; *)
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

let run_batch_viz ~cachedir ~datadir ~configdir =
  let open Rresult.R.Infix in
  begin
    let script = Fpath.(configdir / "batch-viz.sh")
    and script_log = Fpath.(cachedir / "batch-viz.log")
    and viz_script = Fpath.(configdir / "upload-hooks" / "visualizations.sh")
    in
    Bos.OS.File.exists script >>= fun script_exists ->
    if not script_exists then begin
      Logs.warn (fun m -> m "Didn't find %s" (Fpath.to_string script));
      Ok ()
    end else
      let args =
        [ "--cache-dir=" ^ Fpath.to_string cachedir;
          "--data-dir=" ^ Fpath.to_string datadir;
          "--viz-script=" ^ Fpath.to_string viz_script ]
        |> List.map (fun s -> "\"" ^ String.escaped s ^ "\"")
        |> String.concat " "
      in
      (*> Note: The reason for appending, is that else a new startup could
          overwrite an existing running batch's log*)
      (Fpath.to_string script ^ " " ^ args
       ^ " 2>&1 >> " ^ Fpath.to_string script_log
       ^ " &")
      |> Sys.command
      |> ignore
      |> Result.ok
  end
  |> function
  | Ok () -> ()
  | Error err ->
    Logs.warn (fun m ->
        m "Error while starting batch-viz.sh: %a"
          Rresult.R.pp_msg err)

let setup_app level influx port host datadir cachedir configdir run_batch_viz_flag expired_jobs =
  let dbpath = Printf.sprintf "%s/builder.sqlite3" datadir in
  let datadir = Fpath.v datadir in
  let cachedir =
    cachedir |> Option.fold ~none:Fpath.(datadir / "_cache") ~some:Fpath.v
  in
  let configdir = Fpath.v configdir in
  let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna) in
  let () = init_influx "builder-web" influx in
  let () =
    if run_batch_viz_flag then
      run_batch_viz ~cachedir ~datadir ~configdir
  in
  match Builder_web.init dbpath datadir with
  | exception Sqlite3.Error e ->
    Format.eprintf "Error: @[@,%s.\
                    @,Does the database file exist? Create with `builder-db migrate`.@]\n%!"
      e;
    exit 2
  | Error (#Caqti_error.load as e) ->
    Format.eprintf "Error: %a\n%!" Caqti_error.pp e;
    exit 2
          | Error (`Wrong_version _ as e) ->
    Format.eprintf "Error: @[@,%a.\
                    @,Migrate database version with `builder-migrations`,\
                    @,or start with a fresh database with `builder-db migrate`.@]\n%!"
      Builder_web.pp_error e;
  | Error (
      #Caqti_error.connect
    | #Caqti_error.call_or_retrieve
    | `Msg _ as e
    ) ->
    Format.eprintf "Error: %a\n%!" Builder_web.pp_error e;
    exit 1
  | Ok () ->
    let level = match level with
      | None -> None
      | Some Logs.Debug -> Some `Debug
      | Some Info -> Some `Info
      | Some Warning -> Some `Warning
      | Some Error -> Some `Error
      | Some App -> None
    in
    let error_handler = Dream.error_template Builder_web.error_template in
    Dream.initialize_log ?level ();
    let dream_routes = Builder_web.(
        routes ~datadir ~cachedir ~configdir ~expired_jobs
        |> to_dream_routes
      )
    in
    Dream.run ~port ~interface:host ~tls:false ~error_handler
    @@ Dream.logger
    @@ Dream.sql_pool ("sqlite3:" ^ dbpath)
    @@ Http_status_metrics.handle
    @@ Builder_web.Middleware.remove_trailing_url_slash
    @@ Dream.router dream_routes

open Cmdliner

let ip_port : (Ipaddr.V4.t * int) Arg.conv =
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
    | Error msg -> Error (`Msg msg)
    | Ok (ip, port) -> match Ipaddr.V4.of_string ip with
      | Ok ip -> Ok (ip, port)
      | Error `Msg msg -> Error (`Msg msg)
  in
  let printer ppf (ip, port) =
    Format.fprintf ppf "%a:%d" Ipaddr.V4.pp ip port in
  Arg.conv (parse, printer)

let datadir =
  let doc = "data directory" in
  let docv = "DATA_DIR" in
  let env = Cmdliner.Cmd.Env.info "BUILDER_WEB_DATADIR" in
  Arg.(
    value &
    opt dir Builder_system.default_datadir &
    info ~env [ "d"; "datadir" ] ~doc ~docv
  )

let cachedir =
  let doc = "cache directory" in
  let docv = "CACHE_DIR" in
  Arg.(
    value
    & opt (some ~none:"DATADIR/_cache" dir) None
    & info [ "cachedir" ] ~doc ~docv)

let configdir =
  let doc = "config directory" in
  let docv = "CONFIG_DIR" in
  Arg.(
    value &
    opt dir Builder_system.default_configdir &
    info [ "c"; "configdir" ] ~doc ~docv)

let port =
  let doc = "port" in
  Arg.(value & opt int 3000 & info [ "p"; "port" ] ~doc)

let host =
  let doc = "host" in
  Arg.(value & opt string "0.0.0.0" & info [ "h"; "host" ] ~doc)

let influx =
  let doc = "IP address and port (default: 8094) to report metrics to \
             influx line protocol" in
  Arg.(
    value &
    opt (some ip_port) None &
    info [ "influx" ] ~doc ~docv:"INFLUXHOST[:PORT]")

let run_batch_viz =
  let doc = "Run CONFIG_DIR/batch-viz.sh on startup. \
             Note that this is started in the background - so the user \
             is in charge of not running several instances of this. A \
             log is written to CACHE_DIR/batch-viz.log" in
  Arg.(value & flag & info [ "run-batch-viz" ] ~doc)

let expired_jobs =
  let doc = "Amount of days after which a job is considered to be inactive if \
             no successful build has been achieved (use 0 for infinite)" in
  Arg.(value & opt int 30 & info [ "expired-jobs" ] ~doc)

let () =
  let term =
    Term.(const setup_app $ Logs_cli.level () $ influx $ port $ host $ datadir $
          cachedir $ configdir $ run_batch_viz $ expired_jobs)
  in
  let info = Cmd.info "Builder web" ~doc:"Builder web" ~man:[] in
  Cmd.v info term
  |> Cmd.eval
  |> exit
