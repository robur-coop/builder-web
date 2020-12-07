let src = Logs.Src.create "builder-web" ~doc:"Builder_web"
module Log = (val Logs.src_log src : Logs.LOG)

open Opium

type t = Model.t = {
  dir : Fpath.t
}

let routes (t : Model.t) =
  let builder _req =
    match Model.jobs t with
    | Error (`Msg e) ->
      Log.warn (fun m -> m "Error getting jobs: %s" e);
      Response.of_plain_text ~status:`Internal_server_error
        "Error getting jobs" |> Lwt.return
    | Ok jobs ->
      Views.builder jobs |> Response.of_html |> Lwt.return
  in

  let job req =
    let job = Router.param req "job" in
    match Model.job t (Fpath.v job) with
    | Error (`Msg e) ->
      Log.warn (fun m -> m "Error getting job: %s" e);
      Response.of_plain_text ~status:`Internal_server_error
        "Error getting job" |> Lwt.return
    | Ok job ->
      Views.job job |> Response.of_html |> Lwt.return
  in

  let job_run req =
    let job = Router.param req "job"
    and run = Router.param req "run" in
    match Model.read_full t (Fpath.v job) (Fpath.v run) with
    | Error (`Msg e) ->
      Log.warn (fun m -> m "Error getting job run: %s" e);
      Response.of_plain_text ~status:`Internal_server_error
        "Error getting job run" |> Lwt.return
    | Ok job_run ->
      Views.job_run job_run |> Response.of_html |> Lwt.return
  in

  [
    App.get "/" builder;
    App.get "/job/:job" job;
    App.get "/job/:job/run/:run" job_run;
  ]

let add_routes t (app : App.t) =
  List.fold_right
    (fun route app -> route app)
    (routes t)
    app
