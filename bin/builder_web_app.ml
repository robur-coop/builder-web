open Opium

let t = Result.get_ok (Builder_web.init "builder.sqlite3")

let app =
  App.empty
  |> App.cmd_name "Builder Web"
  |> Builder_web.add_routes t

let () =
  App.run_command app
