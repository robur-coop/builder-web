open Opium


let app t =
  App.empty
  |> App.cmd_name "Builder Web"
  |> Builder_web.add_routes t

let setup_app port host _debug _verbose datadir =
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
    |> App.start

open Cmdliner

let datadir =
  let doc = "data directory" in
  Arg.(value & opt dir "/var/db/builder-web/" & info [ "-d"; "--datadir" ] ~doc)

let port =
  let doc = "port" in
  Arg.(value & opt int 3000 & info [ "p"; "port" ] ~doc)

let host =
  let doc = "host" in
  Arg.(value & opt string "0.0.0.0" & info [ "h"; "host" ] ~doc)

let debug =
  let doc = "enable debug information" in
  Arg.(value & flag & info [ "d"; "debug" ] ~doc)

let verbose =
  let doc = "enable verbose mode" in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)



let () =
  let () = Mirage_crypto_rng_unix.initialize () in
  let term = Term.(pure setup_app $ port $ host $ pure false $ pure false $ datadir) in
  let info = Term.info "Builder web" ~doc:"Builder web" ~man:[] in
  match Term.eval (term, info) with
  | `Ok s ->
    Lwt_main.run (Lwt.async (fun () ->
        Lwt.bind s (fun _ -> Lwt.return_unit));
       let forever, _ = Lwt.wait () in
       forever)
  | `Error _ -> exit 1
  | _ -> exit 0
