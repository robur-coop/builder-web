let src = Logs.Src.create "builder-web" ~doc:"Builder_web"
module Log = (val Logs.src_log src : Logs.LOG)

open Opium

module Model = Model

let routes (t : Model.t) =
  let builder _req =
    match Model.jobs t with
    | Error (`Msg e) ->
      Log.warn (fun f -> f "Error getting jobs: %s" e);
      Response.of_plain_text ~status:`Internal_server_error
        "Error getting jobs" |> Lwt.return
    | Ok jobs ->
      Views.builder jobs |> Response.of_html |> Lwt.return
  in

  [ App.get "/" builder ]

let add_routes t (app : App.t) =
  List.fold_right
    (fun route app -> route app)
    (routes t)
    app
