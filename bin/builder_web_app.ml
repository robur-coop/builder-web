open Opium

let t = Result.get_ok (Builder_web.init "builder.sqlite3")

let app =
  App.empty
  |> App.cmd_name "Builder Web"
  |> Builder_web.add_routes t

let () =
  Mirage_crypto_rng_unix.initialize ();
  App.run_command app
