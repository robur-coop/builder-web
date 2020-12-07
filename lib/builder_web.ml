let src = Logs.Src.create "builder-web" ~doc:"Builder_web"
module Log = (val Logs.src_log src : Logs.LOG)

open Opium
open Rresult.R.Infix

type t = Model.t

let init = Model.init

let safe_seg path =
  if Fpath.is_seg path && not (Fpath.is_rel_seg path)
  then Ok (Fpath.v path)
  else Rresult.R.error_msgf "unsafe path %S" path

let routes (t : Model.t) =
  let builder _req =
    match Model.jobs t with
    | Error (`Msg e) ->
      Log.warn (fun m -> m "Error getting jobs: %s" e);
      Response.of_plain_text ~status:`Internal_server_error
        "Error getting jobs" |> Lwt.return
    | Ok jobs ->
      List.sort (fun j1 j2 -> Fpath.compare j1.Model.path j2.Model.path) jobs
      |> Views.builder |> Response.of_html |> Lwt.return
  in

  let job req =
    let job = Router.param req "job" in
    match safe_seg job >>= fun job ->
      Model.job t job with
    | Ok job ->
      let name = Model.job_name job
      and runs = job.Model.runs in
      Views.job name runs |> Response.of_html |> Lwt.return
    | Error (`Msg e) ->
      Log.warn (fun m -> m "Error getting job: %s" e);
      Response.of_plain_text ~status:`Internal_server_error
        "Error getting job" |> Lwt.return
  in

  let job_run req =
    let job = Router.param req "job"
    and run = Router.param req "run" in
    match safe_seg job >>= fun job ->
      safe_seg run >>= fun run ->
      Model.read_full t job run with
    | Error (`Msg e) ->
      Log.warn (fun m -> m "Error getting job run: %s" e);
      Response.of_plain_text ~status:`Internal_server_error
        "Error getting job run" |> Lwt.return
    | Ok job_run ->
      Views.job_run job_run |> Response.of_html |> Lwt.return
  in

  [
    App.get "/" builder;
    App.get "/job/:job/" job;
    App.get "/job/:job/run/:run/" job_run;
  ]

let add_routes t (app : App.t) =
  List.fold_right
    (fun route app -> route app)
    (routes t)
    app
