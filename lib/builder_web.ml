let src = Logs.Src.create "builder-web" ~doc:"Builder_web"
module Log = (val Logs.src_log src : Logs.LOG)

open Opium
open Lwt.Syntax
open Lwt_result.Infix

type t = Model.t

let init = Model.init

let safe_seg path =
  if Fpath.is_seg path && not (Fpath.is_rel_seg path)
  then Ok (Fpath.v path)
  else Rresult.R.error_msgf "unsafe path %S" path

let routes (t : Model.t) =
  let builder _req =
    let+ jobs = Model.jobs t in
    match jobs with
    | Error (`Msg e) ->
      Log.warn (fun m -> m "Error getting jobs: %s" e);
      Response.of_plain_text ~status:`Internal_server_error
        "Error getting jobs"
    | Ok jobs ->
      List.sort Fpath.compare jobs
      |> Views.builder |> Response.of_html
  in

  let job req =
    let job = Router.param req "job" in
    let+ job = Lwt_result.lift (safe_seg job) >>= fun job -> Model.job t job in
    match job with
    | Ok job ->
      let name = Model.job_name job
      and runs = List.sort
          (fun (b1 : Model.job_run_meta) (b2 : Model.job_run_meta) ->
             Ptime.compare b1.start b2.start)
          job.Model.runs in
      Views.job name runs |> Response.of_html
    | Error (`Msg e) ->
      Log.warn (fun m -> m "Error getting job: %s" e);
      Response.of_plain_text ~status:`Internal_server_error
        "Error getting job"
  in

  let job_run req =
    let job = Router.param req "job"
    and run = Router.param req "run" in
    let+ job_run =
      Lwt_result.lift (safe_seg job) >>= fun job ->
      Lwt_result.lift (safe_seg run) >>= fun run ->
      Model.read_full_with_digests t job run in
    match job_run with
    | Error (`Msg e) ->
      Log.warn (fun m -> m "Error getting job run: %s" e);
      Response.of_plain_text ~status:`Internal_server_error
        "Error getting job run"
    | Ok (job_run, digests) ->
      Views.job_run job_run digests |> Response.of_html
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
