open Opium


let app t =
  App.empty
  |> App.cmd_name "Builder Web"
  |> Builder_web.add_routes t

let () =
  let () = Mirage_crypto_rng_unix.initialize () in
  let datadir = Fpath.v "/var/db/builder-web/" in
  let t = Result.get_ok (Builder_web.init "builder.sqlite3" datadir) in
  App.run_command (app t)
