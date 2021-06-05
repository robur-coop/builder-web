let src = Logs.Src.create "builder-web" ~doc:"Builder_web"
module Log = (val Logs.src_log src : Logs.LOG)

open Lwt.Syntax
open Lwt_result.Infix

type db_error = [ Caqti_error.connect | Model.error ]

let pp_error ppf = function
  | #Caqti_error.connect as e -> Caqti_error.pp ppf e
  | #Model.error as e -> Model.pp_error ppf e
  | `Wrong_version (application_id, version) ->
    if application_id = Builder_db.application_id
    then Format.fprintf ppf "Wrong database version: %Ld" version
    else Format.fprintf ppf "Wrong database application id: %ld" application_id

let realm = "builder-web"

let init_datadir datadir =
  let open Rresult.R.Infix in
  Bos.OS.Dir.exists datadir >>= (fun exists ->
      if exists
      then Ok ()
      else Error (`Msg "Datadir does not exist")) >>= fun () ->
  Bos.OS.Dir.create ~path:false (Model.staging datadir) >>| fun _ -> ()

let init dbpath datadir =
  Rresult.R.bind (init_datadir datadir) @@ fun () ->
  Lwt_main.run (
    Caqti_lwt.connect
      (Uri.make ~scheme:"sqlite3" ~path:dbpath ~query:["create", ["false"]] ())
    >>= fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find Builder_db.get_application_id () >>= fun application_id ->
    Db.find Builder_db.get_version () >>= (fun version ->
        if (application_id, version) = Builder_db.(application_id, current_version)
        then Lwt_result.return ()
        else Lwt_result.fail (`Wrong_version (application_id, version)))
    >>= fun () ->
    Model.cleanup_staging datadir (module Db))

let pp_exec ppf (job, uuid, _, _, _, _, _) =
  Format.fprintf ppf "%s(%a)" job.Builder.name Uuidm.pp uuid

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

let authorized handler = fun req ->
  let unauthorized () =
    let headers = ["WWW-Authenticate", Printf.sprintf "Basic realm=\"%s\"" realm] in
    Dream.respond ~headers ~status:`Unauthorized "Forbidden!"
  in
  match Dream.header "Authorization" req with
  | None -> unauthorized ()
  | Some data -> match String.split_on_char ' ' data with
     | [ "Basic" ; user_pass ] ->
       (match Base64.decode user_pass with
       | Error `Msg msg ->
         Log.info (fun m -> m "Invalid user / pasword encoding in %S: %S" data msg);
         Dream.respond ~status:`Bad_Request "Couldn't decode authorization"
       | Ok user_pass -> match String.split_on_char ':' user_pass with
         | [] | [_] ->
           Log.info (fun m -> m "Invalid user / pasword encoding in %S" data);
           Dream.respond ~status:`Bad_Request "Couldn't decode authorization"
         | user :: password ->
           let pass = String.concat ":" password in
           let* user_info = Dream.sql req (Model.user user) in
           match user_info with
           | Ok (Some user_info) ->
             if Builder_web_auth.verify_password pass user_info
             then handler req
             else unauthorized ()
           | Ok None ->
             let _ : _ Builder_web_auth.user_info =
               Builder_web_auth.hash ~username:user ~password:pass () in
             unauthorized ()
           | Error e ->
             Log.warn (fun m -> m "Error getting user: %a" pp_error e);
             Dream.respond ~status:`Internal_Server_Error "Internal server error")
     | _ ->
       Log.warn (fun m -> m "Error retrieving authorization %S" data);
       Dream.respond ~status:`Bad_Request "Couldn't decode authorization"

let string_of_html =
  Format.asprintf "%a" (Tyxml.Html.pp ())

let add_routes datadir =
  let datadir_global = Dream.new_global ~name:"datadir" (fun () -> datadir) in

  let builder req =
    let* jobs = Dream.sql req Model.jobs in
    match jobs with
    | Error e ->
      Log.warn (fun m -> m "Error getting jobs: %a" pp_error e);
      Dream.respond ~status:`Internal_Server_Error "Error getting jobs"
    | Ok jobs ->
      let* jobs =
        List.fold_right
          (fun (job_id, job_name) r ->
             r >>= fun acc ->
             Dream.sql req (Model.build_meta job_id) >>= function
             | Some (latest_build, latest_artifact) ->
               Lwt_result.return ((job_name, latest_build, latest_artifact) :: acc)
             | None ->
               Log.warn (fun m -> m "Job without builds: %s" job_name);
               Lwt_result.return acc)
          jobs
          (Lwt_result.return [])
      in
      match jobs with
      | Error e ->
        Log.warn (fun m -> m "Error getting jobs: %a" pp_error e);
        Dream.respond ~status:`Internal_Server_Error "Error getting jobs"
      | Ok jobs ->
        Views.builder jobs |> string_of_html |> Dream.html
  in

  let job req =
    let job_name = Dream.param "job" req in
    let* job = Dream.sql req (Model.job job_name) in
    match job with
    | Error e ->
      Log.warn (fun m -> m "Error getting job: %a" pp_error e);
      Dream.respond ~status:`Internal_Server_Error "Error getting job"
    | Ok builds ->
      Views.job job_name builds |> string_of_html |> Dream.html
  in

  let redirect_latest req =
    let job_name = Dream.param "job" req in
    let path = Dream.path req |> String.concat "/" in
    let* build =
      Dream.sql req (Model.job_id job_name) >>= fun job_id ->
      Dream.sql req (Model.latest_successful_build_uuid job_id)
      >>= Model.not_found
    in
    match build with
    | Error e ->
      Log.warn (fun m -> m "Error getting job: %a" pp_error e);
      Dream.respond ~status:`Not_Found "Error getting job"
    | Ok build ->
      Dream.redirect req
        (Fmt.strf "/job/%s/build/%a/%s" job_name Uuidm.pp build path)
  in

  let job_build req =
    let job_name = Dream.param "job" req
    and build = Dream.param "build" req in
    match Uuidm.of_string build with
    | None ->
      Dream.respond "Bad request." ~status:`Bad_Request
    | Some uuid ->
      let* data =
        Dream.sql req (Model.build uuid) >>= fun (build_id, build) ->
        Dream.sql req (Model.build_artifacts build_id) >>= fun artifacts ->
        Dream.sql req (Model.latest_successful_build_uuid build.job_id) >>= fun latest_uuid ->
        Dream.sql req (Model.previous_successful_build build_id) >|= fun previous_build ->
        (build, artifacts, latest_uuid, previous_build)
      in
      match data with
      | Error e ->
        Log.warn (fun m -> m "Error getting job build: %a" pp_error e);
        Dream.respond "Error getting job build" ~status:`Internal_Server_Error
      | Ok (build, artifacts, latest_uuid, previous_build) ->
        Views.job_build job_name build artifacts latest_uuid previous_build |> string_of_html |> Dream.html
  in

  let job_build_file req =
    let datadir = Dream.global datadir_global req in
    let _job_name = Dream.param "job" req
    and build = Dream.param "build" req
    and filepath = Dream.path req |> String.concat "/" in
    let if_none_match = Dream.header "if-none-match" req in
    (* XXX: We don't check safety of [file]. This should be fine however since
     * we don't use [file] for the filesystem but is instead used as a key for
     * lookup in the data table of the 'full' file. *)
    match Uuidm.of_string build, Fpath.of_string filepath with
    | None, _ ->
      Log.debug (fun m -> m "bad uuid: %s" build);
      Dream.respond ~status:`Not_Found "File not found"
    | _, Error (`Msg e) ->
      Log.debug (fun m -> m "bad path: %s" e);
      Dream.respond ~status:`Not_Found "File not found"
    | Some build, Ok filepath ->
      let* file = Dream.sql req (Model.build_artifact build filepath) in
      match file with
      | Error e ->
        Log.warn (fun m -> m "Error getting build artifact: %a" pp_error e);
        Dream.respond ~status:`Internal_Server_Error "Error getting build artifact"
      | Ok file ->
        let etag = Base64.encode_string (Cstruct.to_string file.Builder_db.sha256) in
        match if_none_match with
        | Some etag' when etag = etag' ->
          Dream.empty `Not_Modified
        | _ ->
          let* data = Model.build_artifact_data datadir file in
          match data with
          | Ok data ->
            let headers = [
              "Content-Type", mime_lookup file.Builder_db.filepath;
              "ETag", etag;
            ] in
            Dream.respond ~headers data
          | Error e ->
            Log.warn (fun m -> m "Error getting build artifact: %a" pp_error e);
            Dream.respond ~status:`Internal_Server_Error "Error getting build artifact"
  in

  let upload req =
    let* body = Dream.body req in
    match Builder.Asn.exec_of_cs (Cstruct.of_string body) with
    | Error (`Msg e) ->
      Log.warn (fun m -> m "Received bad builder ASN.1");
      Log.debug (fun m -> m "Parse error: %s" e);
      Dream.respond ~status:`Bad_Request "Bad request"
    | Ok ((_, uuid, _, _, _, _, _) as exec) ->
      Log.info (fun m -> m "Received build %a" pp_exec exec);
      let* r = Dream.sql req (Model.build_exists uuid) in
      match r with
      | Error e ->
        Log.warn (fun m -> m "Error saving build %a: %a" pp_exec exec pp_error e);
        Dream.respond ~status:`Internal_Server_Error "Internal server error"
      | Ok true ->
        Log.warn (fun m -> m "Build with same uuid exists: %a" pp_exec exec);
        Dream.respond ~status:`Conflict
          (Fmt.strf "Build with same uuid exists: %a\n" Uuidm.pp uuid)
      | Ok false ->
        let datadir = Dream.global datadir_global req in
        let* r = Dream.sql req (Model.add_build datadir exec) in
        match r with
        | Ok () ->
          Dream.respond ""
        | Error e ->
          Log.warn (fun m -> m "Error saving build %a: %a" pp_exec exec pp_error e);
          Dream.respond ~status:`Internal_Server_Error "Internal server error"
  in

  let hash req =
    let hash_hex = Dream.query "sha256" req in
    match Option.map (fun h -> Hex.to_cstruct (`Hex h)) hash_hex with
    | None ->
      Log.debug (fun m -> m "sha256 query parameter not provided");
      Dream.respond ~status:`Bad_Request "Bad request"
    | Some hash ->
      let* build = Dream.sql req (Model.build_hash hash) in
      (match build with
       | Error e ->
         Log.warn (fun m -> m "Database error: %a" pp_error e);
         Dream.respond ~status:`Internal_Server_Error "Internal server error"
       | Ok None ->
         Log.debug (fun m -> m "Hash not found: %S" (Option.get hash_hex));
         Dream.respond ~status:`Not_Found "Artifact not found"
       | Ok (Some (job_name, build)) ->
         Dream.redirect req
           (Fmt.strf "/job/%s/build/%a/" job_name Uuidm.pp build.Builder_db.Build.uuid))
    | exception Invalid_argument _ ->
      Log.debug (fun m -> m "Invalid hash hex %S" (Option.get hash_hex));
      Dream.respond ~status:`Bad_Request "Bad request"
  in

  let compare_opam req =
    let datadir = Dream.global datadir_global req in
    let build_left = Dream.param "build_left" req in
    let build_right = Dream.param "build_right" req in
    match Uuidm.of_string build_left, Uuidm.of_string build_right with
    | None, _ | _, None ->
      Dream.respond ~status:`Bad_Request "Bad request"
    | Some build_left, Some build_right ->
      let* r =
        Dream.sql req (Model.build_artifact build_left (Fpath.v "opam-switch")) >>=
        Model.build_artifact_data datadir >>= fun switch_left ->
        Dream.sql req (Model.build_artifact build_right (Fpath.v "opam-switch")) >>=
        Model.build_artifact_data datadir >>= fun switch_right ->
        Dream.sql req (Model.build build_left) >>= fun (_id, build_left) ->
        Dream.sql req (Model.build build_right) >>= fun (_id, build_right) ->
        Dream.sql req (Model.job_name build_left.job_id) >>= fun job_left ->
        Dream.sql req (Model.job_name build_right.job_id) >>= fun job_right ->
        Lwt_result.return (job_left, job_right, build_left, build_right, switch_left, switch_right)
      in
      match r with
      | Error e ->
        Log.warn (fun m -> m "Database error: %a" pp_error e);
        Dream.respond ~status:`Internal_Server_Error "Internal server error"
      | Ok (job_left, job_right, build_left, build_right, switch_left, switch_right) ->
        let switch_left = OpamFile.SwitchExport.read_from_string switch_left
        and switch_right = OpamFile.SwitchExport.read_from_string switch_right in
        Opamdiff.compare switch_left switch_right
        |> Views.compare_opam job_left job_right build_left build_right
        |> string_of_html |> Dream.html
  in

  Dream.router [
    Dream.get "/" builder;
    Dream.get "/job/:job/" job;
    Dream.get "/job/:job/build/latest/**" redirect_latest;
    Dream.get "/job/:job/build/:build/" job_build;
    Dream.get "/job/:job/build/:build/f/**" job_build_file;
    Dream.get "/hash" hash;
    Dream.get "/compare/:build_left/:build_right/opam-switch" compare_opam;
    Dream.post "/upload" (authorized upload);
  ]
