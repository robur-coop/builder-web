let src = Logs.Src.create "builder-web" ~doc:"Builder_web"
module Log = (val Logs.src_log src : Logs.LOG)

open Opium
open Lwt.Syntax
open Lwt_result.Infix

type db_error = [ Caqti_error.connect | Model.error ]

let pp_error ppf = function
  | #Caqti_error.connect as e -> Caqti_error.pp ppf e
  | #Model.error as e -> Model.pp_error ppf e

type 'a t = {
  pool : (Caqti_lwt.connection, [> db_error ] as 'a) Caqti_lwt.Pool.t
}

let init ?(pool_size = 10) dbpath =
  Caqti_lwt.connect_pool
    ~max_size:pool_size
    (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
  |> Result.map (fun pool ->
      { pool = (pool :> (Caqti_lwt.connection, [> db_error ]) Caqti_lwt.Pool.t); })

let safe_seg path =
  if Fpath.is_seg path && not (Fpath.is_rel_seg path)
  then Ok (Fpath.v path)
  else Rresult.R.error_msgf "unsafe path %S" path

(* mime lookup with orb knowledge *)
let mime_lookup path =
  match Fpath.to_string path with
  | "build-environment" | "opam-switch" | "system-packages" ->
    "text/plain"
  | _ ->
    if Fpath.has_ext "build-hashes" path
    then "text/plain"
    else if Fpath.is_prefix Fpath.(v "bin/") path
    then "application/octet-stream"
    else Magic_mime.lookup (Fpath.to_string path)

let routes t =
  let builder _req =
    let+ jobs = Caqti_lwt.Pool.use Model.jobs t.pool in
    match jobs with
    | Error e ->
      Log.warn (fun m -> m "Error getting jobs: %a" pp_error e);
      Response.of_plain_text ~status:`Internal_server_error
        "Error getting jobs"
    | Ok jobs ->
      List.sort String.compare jobs
      |> Views.builder |> Response.of_html
  in

  let job req =
    let job_name = Router.param req "job" in
    let+ job = Caqti_lwt.Pool.use (Model.job job_name) t.pool in
    match job with
    | Error e ->
      Log.warn (fun m -> m "Error getting job: %a" pp_error e);
      Response.of_plain_text ~status:`Internal_server_error
        "Error getting job"
    | Ok builds ->
      Views.job job_name (List.map snd builds) |> Response.of_html
  in

  let job_build req =
    let job_name = Router.param req "job"
    and build = Router.param req "build" in
    match Uuidm.of_string build with
    | None ->
      Response.of_plain_text ~status:`Bad_request
        "Bad request.\n"
      |> Lwt.return
    | Some uuid ->
      let+ build_and_artifacts =
        Caqti_lwt.Pool.use (Model.build uuid) t.pool >>= fun (build_id, build) ->
        Caqti_lwt.Pool.use (Model.build_artifacts build_id) t.pool >|= fun artifacts ->
        (build, artifacts)
      in
      match build_and_artifacts with
      | Error e ->
        Log.warn (fun m -> m "Error getting job build: %a" pp_error e);
        Response.of_plain_text ~status:`Internal_server_error
          "Error getting job build"
      | Ok (build, artifacts) ->
        Views.job_build job_name build artifacts |> Response.of_html
  in

  let job_build_file req =
    let _job_name = Router.param req "job"
    and build = Router.param req "build"
    and filepath = Router.splat req |> String.concat "/" in
    (* XXX: We don't check safety of [file]. This should be fine however since
     * we don't use [file] for the filesystem but is instead used as a key for
     * lookup in the data table of the 'full' file. *)
    match Uuidm.of_string build, Fpath.of_string filepath with
    | None, _ ->
      Log.debug (fun m -> m "bad uuid: %s" build);
      Response.of_plain_text ~status:`Not_found "File not found"
      |> Lwt.return
    | _, Error (`Msg e) ->
      Log.debug (fun m -> m "bad path: %s" e);
      Response.of_plain_text ~status:`Not_found "File not found"
      |> Lwt.return
    | Some build, Ok filepath ->
      let+ artifact = Caqti_lwt.Pool.use (Model.build_artifact build filepath) t.pool in
      match artifact with
      | Error e ->
        Log.warn (fun m -> m "Error getting build artifact: %a" pp_error e);
        Response.of_plain_text ~status:`Internal_server_error
          "Error getting build artifact"
      | Ok (data, digest) ->
        let body = Body.of_string data in
        Response.make ~body ()
        |> Response.add_header ("Content-type", mime_lookup filepath)
        |> Response.set_etag (Base64.encode_string (Cstruct.to_string digest))
  in

  let refresh _req =
    let status = Lwt_unix.system "builder-db add" in
    let stream =
      let i = ref 0 in
      Lwt_stream.from (fun () ->
          i := !i + 1;
          match !i with
          | 1 ->
            Lwt.return_some "Refresh job started.\n"
          | 2 ->
            let* status = status in
            (match status with
             | Unix.WEXITED 0 ->
               Lwt.return_some "Refresh job was successful!"
             | Unix.WEXITED n ->
               Lwt.return_some (Printf.sprintf "Refresh job failed : %d" n)
             | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
               Lwt.return_some ("Refresh job signalled"))
          | _ ->
            Lwt.return_none)
    in
    let body = Body.of_stream stream in
    Response.make ~body ()
    |> Response.set_content_type "text/plain"
    |> Lwt.return
  in

  [
    App.get "/" builder;
    App.get "/job/:job/" job;
    App.get "/job/:job/build/:build/" job_build;
    App.get "/job/:job/build/:build/f/**" job_build_file;
    App.get "/refresh" refresh;
  ]

let add_routes t (app : App.t) =
  List.fold_right
    (fun route app -> route app)
    (routes t)
    app
