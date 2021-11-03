let src = Logs.Src.create "builder-web" ~doc:"Builder_web"
module Log = (val Logs.src_log src : Logs.LOG)

open Lwt.Syntax
open Lwt_result.Infix

let pp_error ppf = function
  | #Caqti_error.connect as e -> Caqti_error.pp ppf e
  | #Model.error as e -> Model.pp_error ppf e
  | `Wrong_version (application_id, version) ->
    if application_id = Builder_db.application_id
    then Format.fprintf ppf "Wrong database version: %Ld" version
    else Format.fprintf ppf "Wrong database application id: %ld" application_id

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

let pp_exec ppf ((job : Builder.script_job), uuid, _, _, _, _, _) =
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

let or_error_response r =
  let* r = r in
  match r with
  | Ok response -> Lwt.return response
  | Error (text, status) -> Dream.respond ~status text

let if_error ?(status  = `Internal_Server_Error) ?(log=(fun e -> Log.warn (fun m -> m "%s: %a" (Dream.status_to_string status) pp_error e))) message r =
  let* r = r in
  match r with
  | Error `Not_found ->
    Lwt_result.fail ("Resource not found", `Not_Found)
  | Error (#Model.error as e) ->
    log e;
    Lwt_result.fail (message, status)
  | Ok _ as r -> Lwt.return r

let string_of_html =
  Format.asprintf "%a" (Tyxml.Html.pp ())

let get_uuid s =
  Lwt.return
    (if String.length s = 36 then
       match Uuidm.of_string s with
       | Some uuid -> Ok uuid
       | None -> Error ("Bad uuid", `Bad_Request)
     else Error ("Bad uuid", `Bad_Request))

let add_routes datadir =
  let datadir_global = Dream.new_global ~name:"datadir" (fun () -> datadir) in

  let builder req =
    (* TODO filter unsuccessful builds, ?failed=true *)
    Dream.sql req Model.jobs_with_section_synopsis
    |> if_error "Error getting jobs"
      ~log:(fun e -> Log.warn (fun m -> m "Error getting jobs: %a" pp_error e))
    >>= fun jobs ->
    List.fold_right
      (fun (job_id, job_name, section, synopsis) r ->
         r >>= fun acc ->
         Dream.sql req (Model.build_meta job_id) >>= function
         | Some (latest_build, latest_artifact) ->
           let v = (job_name, synopsis, latest_build, latest_artifact) in
           let section = Option.value ~default:"Failed" section in
           Lwt_result.return (Utils.String_map.add_or_create section v acc)
         | None ->
           Log.warn (fun m -> m "Job without builds: %s" job_name);
           Lwt_result.return acc)
      jobs
      (Lwt_result.return Utils.String_map.empty)
    |> if_error "Error getting jobs"
      ~log:(fun e -> Log.warn (fun m -> m "Error getting jobs: %a" pp_error e))
    >>= fun jobs ->
    Views.builder jobs |> string_of_html |> Dream.html |> Lwt_result.ok
  in

  let job req =
    let job_name = Dream.param "job" req in
    Dream.sql req (Model.job_and_readme job_name)
    |> if_error "Error getting job"
      ~log:(fun e -> Log.warn (fun m -> m "Error getting job: %a" pp_error e))
    >>= fun (readme, builds) ->
    Views.job job_name readme builds |> string_of_html |> Dream.html |> Lwt_result.ok
  in

  let redirect_latest req =
    let job_name = Dream.param "job" req in
    let path = Dream.path req |> String.concat "/" in
    (Dream.sql req (Model.job_id job_name) >>= Model.not_found >>= fun job_id ->
     Dream.sql req (Model.latest_successful_build_uuid job_id))
    >>= Model.not_found
    |> if_error "Error getting job" >>= fun build ->
    Dream.redirect req
      (Fmt.str "/job/%s/build/%a/%s" job_name Uuidm.pp build path)
    |> Lwt_result.ok
  in

  let redirect_main_binary req =
    let job_name = Dream.param "job" req
    and build = Dream.param "build" req in
    get_uuid build >>= fun uuid ->
    Dream.sql req (Model.build uuid)
    |> if_error "Error getting job build"
     ~log:(fun e -> Log.warn (fun m -> m "Error getting job build: %a" pp_error e))
    >>= fun (_id, build) ->
    match build.Builder_db.Build.main_binary with
    | None -> Lwt_result.fail ("Resource not found", `Not_Found)
    | Some main_binary ->
      Dream.sql req (Model.build_artifact_by_id main_binary)
      |> if_error "Error getting main binary" >>= fun main_binary ->
      Dream.redirect req
        (Fmt.str "/job/%s/build/%a/f/%a" job_name Uuidm.pp uuid
          Fpath.pp main_binary.Builder_db.filepath)
      |> Lwt_result.ok
  in

  let job_build req =
    let job_name = Dream.param "job" req
    and build = Dream.param "build" req in
    get_uuid build >>= fun uuid ->
    (Dream.sql req (Model.readme job_name) >>= fun readme ->
     Dream.sql req (Model.build uuid) >>= fun (build_id, build) ->
     Dream.sql req (Model.build_artifacts build_id) >>= fun artifacts ->
     Dream.sql req (Model.builds_with_same_input_and_same_main_binary build_id) >>= fun same_input_same_output ->
     Dream.sql req (Model.builds_with_different_input_and_same_main_binary build_id) >>= fun different_input_same_output ->
     Dream.sql req (Model.builds_with_same_input_and_different_main_binary build_id) >>= fun same_input_different_output ->
     Dream.sql req (Model.latest_successful_build_uuid build.job_id) >|= fun latest_uuid ->
         (readme, build, artifacts, same_input_same_output, different_input_same_output, same_input_different_output, latest_uuid))
    |> if_error "Error getting job build"
      ~log:(fun e -> Log.warn (fun m -> m "Error getting job build: %a" pp_error e))
    >>= fun (readme, build, artifacts, same_input_same_output, different_input_same_output, same_input_different_output, latest_uuid) ->
    Views.job_build job_name readme build artifacts same_input_same_output different_input_same_output same_input_different_output latest_uuid
    |> string_of_html |> Dream.html |> Lwt_result.ok
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
    get_uuid build >>= fun build ->
    Fpath.of_string filepath |> Rresult.R.open_error_msg |> Lwt_result.lift
    |> if_error ~status:`Not_Found "File not found" >>= fun filepath ->
    Dream.sql req (Model.build_artifact build filepath)
    |> if_error "Error getting build artifact" >>= fun file ->
    let etag = Base64.encode_string (Cstruct.to_string file.Builder_db.sha256) in
    match if_none_match with
    | Some etag' when etag = etag' ->
      Dream.empty `Not_Modified |> Lwt_result.ok
    | _ ->
      Model.build_artifact_data datadir file
      |> if_error "Error getting build artifact"
          ~log:(fun e -> Log.warn (fun m -> m "Error getting build artifact data for file %a in %a: %a"
                           Fpath.pp file.Builder_db.filepath Fpath.pp file.Builder_db.localpath
                           pp_error e)) >>= fun data ->
      let headers = [
        "Content-Type", mime_lookup file.Builder_db.filepath;
        "ETag", etag;
      ] in
      Dream.respond ~headers data |> Lwt_result.ok
  in

  let job_build_static_file (file : [< `Console | `Script ]) req =
    let datadir = Dream.global datadir_global req in
    let _job_name = Dream.param "job" req
    and build = Dream.param "build" req in
    get_uuid build >>= fun build ->
    (match file with
    | `Console ->
      Dream.sql req (Model.build_console_by_uuid datadir build)
    | `Script ->
      Dream.sql req (Model.build_script_by_uuid datadir build))
    |> if_error "Error getting data"
      ~log:(fun e -> Log.warn (fun m -> m "Error getting script or console data for build %a: %a"
                                  Uuidm.pp build pp_error e)) >>= fun data ->
    let headers = [ "Content-Type", "text/plain" ] in
    Dream.respond ~headers data |> Lwt_result.ok
  in

  let upload req =
    let* body = Dream.body req in
    Builder.Asn.exec_of_cs (Cstruct.of_string body) |> Lwt.return
    |> if_error ~status:`Bad_Request "Bad request"
      ~log:(fun e ->
        Log.warn (fun m -> m "Received bad builder ASN.1: %a" pp_error e))
    >>= fun ((({ name ; _ } : Builder.script_job), uuid, _, _, _, _, _) as exec) ->
    Log.debug (fun m -> m "Received build %a" pp_exec exec);
    Authorization.authorized req name
    |> if_error ~status:`Forbidden "Forbidden" >>= fun () ->
    Dream.sql req (Model.build_exists uuid)
    |> if_error "Internal server error"
      ~log:(fun e ->
              Log.warn (fun m -> m "Error saving build %a: %a" pp_exec exec pp_error e))
    >>= function
    | true ->
      Log.warn (fun m -> m "Build with same uuid exists: %a" pp_exec exec);
      Dream.respond ~status:`Conflict
        (Fmt.str "Build with same uuid exists: %a\n" Uuidm.pp uuid)
      |> Lwt_result.ok
    | false ->
      let datadir = Dream.global datadir_global req in
      (Lwt.return (Dream.local Authorization.user_info_local req |>
                   Option.to_result ~none:(`Msg "no authenticated user")) >>= fun (user_id, _) ->
       Dream.sql req (Model.add_build datadir user_id exec))
      |> if_error "Internal server error"
        ~log:(fun e -> Log.warn (fun m -> m "Error saving build %a: %a" pp_exec exec pp_error e))
      >>= fun () -> Dream.respond "" |> Lwt_result.ok
  in

  let hash req =
    Dream.query "sha256" req |> Option.to_result ~none:(`Msg "Missing sha256 query parameter") |> Lwt.return
    |> if_error ~status:`Bad_Request "Bad request" >>= fun hash_hex ->
    begin try Hex.to_cstruct (`Hex hash_hex) |> Lwt_result.return
      with Invalid_argument e -> Lwt_result.fail (`Msg ("Bad hex: " ^ e))
    end
    |> if_error ~status:`Bad_Request "Bad request" >>= fun hash ->
    Dream.sql req (Model.build_hash hash) >>= Model.not_found
    |> if_error "Internal server error" >>= fun (job_name, build) ->
    Dream.redirect req
      (Fmt.str "/job/%s/build/%a/" job_name Uuidm.pp build.Builder_db.Build.uuid)
    |> Lwt_result.ok
  in

  let compare_opam req =
    let datadir = Dream.global datadir_global req in
    let build_left = Dream.param "build_left" req in
    let build_right = Dream.param "build_right" req in
    get_uuid build_left >>= fun build_left ->
    get_uuid build_right >>= fun build_right ->
    (Dream.sql req (Model.build build_left) >>= fun (_id, build_left) ->
     Dream.sql req (Model.build build_right) >>= fun (_id, build_right) ->
     Dream.sql req (Model.build_artifact build_left.Builder_db.Build.uuid (Fpath.v "opam-switch")) >>=
     Model.build_artifact_data datadir >>= fun switch_left ->
     Dream.sql req (Model.build_artifact build_left.Builder_db.Build.uuid (Fpath.v "build-environment")) >>=
     Model.build_artifact_data datadir >>= fun build_env_left ->
     Dream.sql req (Model.build_artifact build_left.Builder_db.Build.uuid (Fpath.v "system-packages")) >>=
     Model.build_artifact_data datadir >>= fun system_packages_left ->
     Dream.sql req (Model.build_artifact build_right.Builder_db.Build.uuid (Fpath.v "opam-switch")) >>=
     Model.build_artifact_data datadir >>= fun switch_right ->
     Dream.sql req (Model.build_artifact build_right.Builder_db.Build.uuid (Fpath.v "build-environment")) >>=
     Model.build_artifact_data datadir >>= fun build_env_right ->
     Dream.sql req (Model.build_artifact build_right.Builder_db.Build.uuid (Fpath.v "system-packages")) >>=
     Model.build_artifact_data datadir >>= fun system_packages_right ->
     Dream.sql req (Model.job_name build_left.job_id) >>= fun job_left ->
     Dream.sql req (Model.job_name build_right.job_id) >|= fun job_right ->
     (job_left, job_right, build_left, build_right,
      switch_left, build_env_left, system_packages_left,
      switch_right, build_env_right, system_packages_right))
    |> if_error "Internal server error"
    >>= fun (job_left, job_right, build_left, build_right,
             switch_left, build_env_left, system_packages_left,
             switch_right, build_env_right, system_packages_right) ->
    let env_diff = Utils.compare_env build_env_left build_env_right
    and pkg_diff = Utils.compare_pkgs system_packages_left system_packages_right
    in
    let switch_left = OpamFile.SwitchExport.read_from_string switch_left
    and switch_right = OpamFile.SwitchExport.read_from_string switch_right in
    Opamdiff.compare switch_left switch_right
    |> Views.compare_opam job_left job_right build_left build_right env_diff pkg_diff
    |> string_of_html |> Dream.html |> Lwt_result.ok
  in

  let upload_binary req =
    let job = Dream.param "job" req in
    let platform = Dream.param "platform" req in
    let binary_name =
      Dream.query "binary_name" req
      |> Option.map Fpath.of_string
      |> Option.value ~default:(Ok Fpath.(v job + "bin"))
    in
    if_error "Bad request" ~status:`Bad_Request (Lwt.return binary_name) >>=
    fun binary_name ->
    let* body = Dream.body req in
    Authorization.authorized req job
    |> if_error ~status:`Forbidden "Forbidden" >>= fun () ->
    let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
    Dream.sql req (Model.build_exists uuid)
    |> if_error "Internal server error"
      ~log:(fun e ->
              Log.warn (fun m -> m "Error saving binary %S: %a" job pp_error e))
    >>= function
    | true ->
      Log.warn (fun m -> m "Build %S with same uuid exists: %a" job Uuidm.pp uuid);
      Dream.respond ~status:`Conflict
        (Fmt.str "Build with same uuid exists: %a\n" Uuidm.pp uuid)
      |> Lwt_result.ok
    | false ->
      let datadir = Dream.global datadir_global req in
      let exec =
        let now = Ptime_clock.now () in
        ({ Builder.name = job ; platform ; script = "" }, uuid, [], now, now, Builder.Exited 0,
         [ (Fpath.(v "bin" // binary_name), body) ])
      in
      (Lwt.return (Dream.local Authorization.user_info_local req |>
                   Option.to_result ~none:(`Msg "no authenticated user")) >>= fun (user_id, _) ->
       Dream.sql req (Model.add_build datadir user_id exec))
      |> if_error "Internal server error"
        ~log:(fun e -> Log.warn (fun m -> m "Error saving build %a: %a" pp_exec exec pp_error e))
      >>= fun () -> Dream.respond "" |> Lwt_result.ok
  in

  let w f req = or_error_response (f req) in

  Dream.router [
    Dream.get "/" (w builder);
    Dream.get "/job/:job/" (w job);
    Dream.get "/job/:job/build/latest/**" (w redirect_latest);
    Dream.get "/job/:job/build/:build/" (w job_build);
    Dream.get "/job/:job/build/:build/f/**" (w job_build_file);
    Dream.get "/job/:job/build/:build/main-binary" (w redirect_main_binary);
    Dream.get "/job/:job/build/:build/script" (w (job_build_static_file `Script));
    Dream.get "/job/:job/build/:build/console" (w (job_build_static_file `Console));
    Dream.get "/hash" (w hash);
    Dream.get "/compare/:build_left/:build_right/opam-switch" (w compare_opam);
    Dream.post "/upload" (Authorization.authenticate (w upload));
    Dream.post "/job/:job/platform/:platform/upload" (Authorization.authenticate (w upload_binary));
  ]
