open Opium

let t = { Builder_web.dir = Fpath.v "sample" }

let app =
  App.empty
  |> App.cmd_name "Builder Web"
  |> Builder_web.add_routes t

let () =
  App.run_command app
