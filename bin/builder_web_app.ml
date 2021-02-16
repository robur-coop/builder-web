open Opium


let app t =
  App.empty
  |> App.cmd_name "Builder Web"
  |> Builder_web.add_routes t

let () =
  let () = Mirage_crypto_rng_unix.initialize () in
  let datadir = Fpath.v "/var/db/builder-web/" in
  match Builder_web.init "/var/db/builder-web/builder.sqlite3" datadir with
  | Error (#Caqti_error.load as e) ->
    Format.eprintf "Error: %a\n" Caqti_error.pp e;
  | Error (#Builder_web.db_error | `Wrong_version _ as e) ->
    Format.eprintf "Error: %a\n" Builder_web.pp_error e;
    exit 1
  | Ok t ->
    App.run_command (app t)
