let src = Logs.Src.create "builder-web" ~doc:"Builder-web"
module Log = (val Logs.src_log src : Logs.LOG)

exception Wrong_version of int32 * int64

type cfg =
  { sw : Caqti_miou.Switch.t
  ; uri : Uri.t
  ; datadir : Fpath.t
  ; configdir : Fpath.t
  ; filter_builds_later_than : int }

type error = [ Caqti_error.t | `Not_found | `Msg of string | `File_error of Fpath.t ]

let pp_error = Model.pp_error

let fix_q = function
  | [] -> None
  | comma_delim -> Some (String.concat "," comma_delim)

let caqti : (cfg, (Caqti_miou.connection, error) Caqti_miou_unix.Pool.t) Vif.Device.device =
  let finally pool = Caqti_miou_unix.Pool.drain pool in
  Vif.Device.v ~name:"caqti" ~finally [] @@ fun { sw; uri; _ } ->
  match Caqti_miou_unix.connect_pool ~sw uri with
  | Error err ->
      Logs.err (fun m -> m "%a" Caqti_error.pp err);
      Fmt.failwith "%a" Caqti_error.pp err
  | Ok pool ->
      let fn (module Conn : Caqti_miou.CONNECTION) =
        let application_id = Conn.find Builder_db.get_application_id () in
        let version = Conn.find Builder_db.get_version () in
        Ok (application_id, version) in
      match Caqti_miou_unix.Pool.use fn pool with
      | Ok (Ok appid, Ok version) ->
          if appid = Builder_db.application_id && version = Builder_db.current_version then
            pool
          else
            raise (Wrong_version (appid, version))
      | Error e ->
          Fmt.failwith "Error connecting to database: %a" pp_error e
      | Ok (Error e, _ | _, Error e) ->
          Fmt.failwith "Error getting database version: %a" pp_error e

module Url = struct
  open Vif.Uri
  let uuid =
    Vif.Uri.conv
      (fun s -> match (Uuidm.of_string s) with
         | None -> Log.err (fun m -> m "bad uuid %S" s); invalid_arg "Uuidm.of_string"
         | Some uuid -> uuid)
      (Uuidm.to_string ~upper:false)
      (Vif.Uri.string `Path)
  let script_or_console =
    Tyre.(str "script" <|> str "console")
    |> Tyre.conv (function `Left () -> `Script | `Right () -> `Console)
      (function `Script -> `Left () | `Console -> `Right ())
  let viz =
    Tyre.(str "viztreemap" <|> str "vizdependencies")
    |> Tyre.conv (function `Left () -> `Treemap | `Right () -> `Dependencies)
      (function `Treemap -> `Left () | `Dependencies -> `Right ())
  let hex =
    Tyre.regex Re.(rep (seq [xdigit; xdigit]))
    |> Vif.Uri.conv (Ohex.decode ~skip_whitespace:false)
      Ohex.encode

  let root = rel /?? nil
  let all_builds = rel / "all-builds" /?? nil
  let failed_builds = rel / "failed-builds" /?? any

  let prefix_job = rel / "job" /% string `Path
  let job = prefix_job /?? any
  let job_with_failed = prefix_job / "failed" /?? any

  (* XXX: we can't use [prefix_job] below due to types /o\ *)
  (* XXX: it is important that
     [Url.(redirect_latest,redirect_latest_empty)] occur before the other
     endpoints living at /job/:job/build/:build as otherwise the former will
     get shadowed by their regular expressions!

     The fix would be to write a regular expression that matches exactly uuids. *)
  let redirect_latest = rel / "job" /% string `Path / "build" / "latest" /% path /?? any
  let redirect_latest_empty = rel / "job" /% string `Path / "build" / "latest" /?? any
  let job_build = rel / "job" /% string `Path / "build" /% uuid /?? any
  let job_build_file = rel / "job" /% string `Path / "build" /% uuid / "f" /% path /?? any
  let job_build_static_file = rel / "job" /% string `Path / "build" /% uuid /% script_or_console /?? any
  let job_build_viz = rel / "job" /% string `Path / "build" /% uuid /% viz /?? any
  let redirect_main_binary = rel / "job" /% string `Path / "build" /% uuid / "main-binary" /?? any
  let exec = rel / "job" /% string `Path / "build" /% uuid / "exec" /?? any
  let upload_binary = rel / "job" /% string `Path / "platform" /% string `Path / "upload" /?? any

  let compare_builds = rel / "compare" /% uuid /% uuid /?? nil

  let hash = rel / "hash" /?? ("sha256", hex) ** any

  let robots = rel / "robots.txt" /?? any

  let upload = rel / "upload" /?? nil
end

let auth_middleware =
  let fn req _target server (_cfg : cfg) =
    let pool = Vif.Server.device caqti server in
    let hdrs = Vif.Request.headers_of_request req in
    match Vif.Headers.get hdrs "Authorization" with
    | None ->
      Log.debug (fun m -> m "No Authorization header");
      None
    | Some data ->
      match String.split_on_char ' ' data with
      | ["Basic"; user_pass] ->
        (match Base64.decode user_pass with
         | Error `Msg msg ->
           Log.info (fun m -> m "Invalid user / pasword encoding in %S: %S" data msg);
           None
         | Ok user_pass -> match String.split_on_char ':' user_pass with
           | [] | [_] ->
             Log.info (fun m -> m "Invalid user / pasword encoding in %S" data);
             None
           | user :: password ->
             let pass = String.concat ":" password in
             match Caqti_miou_unix.Pool.use (Model.user user) pool with
             | Ok Some (id, user_info) ->
               if Builder_web_auth.verify_password pass user_info
               then (Log.debug (fun m -> m "Authenticated as %S" user); Some (id, user_info))
               else (Log.info (fun m -> m "Invalid password for %S" user); None)
             | Ok None ->
               Log.info (fun m -> m "Login attempt for nonexistent user %S" user);
               None
             | Error e ->
               Log.warn (fun m -> m "Error getting user: %a" pp_error e);
               None)
      | _ ->
        Log.info (fun m -> m "Bad Authorization header: %S" data);
        None
  in
  Vif.Middlewares.v ~name:"Basic auth" fn

let authenticated req k =
  match Vif.Request.get auth_middleware req with
  | None ->
    let open Vif.Response.Syntax in
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.add ~field:"www-authenticate" "Basic realm=\"builder-web\"" in
    let* () = Vif.Response.with_string req "Unauthorized!\n" in
    Vif.Response.respond `Unauthorized
  | Some (user_id, user_info) ->
    k user_id user_info

let or_model_error req k =
  let open Vif.Response.Syntax in
  function
  | Ok r -> k r
  | Error `Not_found ->
    let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
    let html = Views.page_not_found ~target:(Vif.Request.target req) ~referer:None in
    let* () = Vif.Response.with_tyxml req html in
    Vif.Response.respond `Not_found
  | Error err ->
    Log.warn (fun m -> m "Database error: %a" pp_error err);
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Internal server error\n" in
    Vif.Response.respond `Internal_server_error

(* mime lookup with orb knowledge *)
let append_charset = function
  (* mime types from nginx:
     http://nginx.org/en/docs/http/ngx_http_charset_module.html#charset_types *)
  | "text/html" | "text/xml" | "text/plain" | "text/vnd.wap.wml"
  | "application/javascript" | "application/rss+xml" | "application/atom+xml"
  as content_type ->
    content_type ^ "; charset=utf-8" (* default to utf-8 *)
  | content_type -> content_type

let mime_lookup path =
  append_charset
    (match Fpath.to_string path with
     | "build-environment" | "opam-switch" | "system-packages" ->
       "text/plain"
     | _ ->
       if Fpath.has_ext "build-hashes" path
       then "text/plain"
       else if Fpath.is_prefix Fpath.(v "bin/") path
       then "application/octet-stream"
       else Magic_mime.lookup (Fpath.to_string path))
(* MIME-type(;q=float)? *("," MIME-type (;q=float)?)) *)

let is_accept_json req =
  let types = Vif.Request.accept req in
  List.exists ((=) "application/json") types

let builds ?(filter_builds = false) ~all req server cfg =
  let pool = Vif.Server.device caqti server in
  let than =
    if not filter_builds then
      Ptime.epoch
    else
      let n = Ptime.Span.v (cfg.filter_builds_later_than, 0L) in
      let now = Ptime_clock.now () in
      (* in the old code we use Ptime.Span.sub... *)
      Ptime.sub_span now n |> Option.value ~default:Ptime.epoch
  in
  let open Vif.Response.Syntax in
  let fn conn =
    let ( let* ) = Result.bind in
    let fn1 job_id job_name platform = function
      | Error _ as err -> err
      | Ok acc ->
          let* res = Model.build_with_main_binary job_id platform conn in
          match res with
          | Some (build, artifact) ->
            if Ptime.is_later ~than build.finish
            then Ok ((platform, build, artifact) :: acc)
            else Ok acc
          | None ->
            Log.warn (fun m -> m "Job without builds: %s" job_name);
            Ok acc in
    let fn0 (job_id, job_name, section, synopsis) = function
      | Error _ as err -> err
      | Ok acc ->
        let* ps = Model.platforms_of_job job_id conn in
        let* platform_builds = List.fold_right (fn1 job_id job_name) ps (Ok []) in
        match platform_builds with
        | [] -> Ok acc
        | platform_builds ->
            let v = (job_name, synopsis, platform_builds) in
            let section = Option.value ~default:"Uncategorized" section in
            Ok (Utils.String_map.add_or_create section v acc) in
    let* jobs = Model.jobs_with_section_synopsis conn in
    List.fold_right fn0 jobs (Ok Utils.String_map.empty) in
  Caqti_miou_unix.Pool.use fn pool
  |> or_model_error req @@ function
  | jobs when is_accept_json req ->
    let json = Views.Builds.make_json ~all jobs in
    let str = Yojson.Basic.to_string json in
    let* () = Vif.Response.add ~field:"content-type" "application/json" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `OK
  | jobs ->
    let html = Views.Builds.make ~all jobs in
    let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
    let* () = Vif.Response.with_tyxml req html in
    Vif.Response.respond `OK

let failed_builds req server _cfg =
  let pool = Vif.Server.device caqti server in
  let platform = fix_q (Vif.Queries.get req "platform") in
  let start, count =
    let to_int default s = Option.(value ~default (bind s int_of_string_opt)) in
    to_int 0 (fix_q (Vif.Queries.get req "start")),
    to_int 20 (fix_q (Vif.Queries.get req "count"))
  in
  let fn conn = Model.failed_builds ~start ~count platform conn in
  let open Vif.Response.Syntax in
  Caqti_miou_unix.Pool.use fn pool
  |> or_model_error req @@ fun builds ->
  let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
  let html = Views.failed_builds ~start ~count builds in
  let* () = Vif.Response.with_tyxml req html in
  Vif.Response.respond `OK

let job req job_name server _cfg =
  let pool = Vif.Server.device caqti server in
  let platform = fix_q (Vif.Queries.get req "platform") in
  let fn conn =
    let ( let* ) = Result.bind in
    let* job_id, readme = Model.job_and_readme job_name conn in
    let* builds = Model.builds_grouped_by_output job_id platform conn in
    Ok (readme, builds) in
  let open Vif.Response.Syntax in
  Caqti_miou_unix.Pool.use fn pool
  |> or_model_error req @@ function
  | (readme, builds) when is_accept_json req ->
    let json = Views.Job.make_json ~failed:false ~job_name ~platform ~readme builds in
    let str = Yojson.Basic.to_string json in
    let* () = Vif.Response.add ~field:"content-type" "application/json" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `OK
  | (readme, builds) ->
    let html = Views.Job.make ~failed:false ~job_name ~platform ~readme builds in
    let* () = Vif.Response.with_tyxml req html in
    Vif.Response.respond `OK

let job_with_failed req job_name server _cfg =
  let pool = Vif.Server.device caqti server in
  let platform = fix_q (Vif.Queries.get req "platform") in
  let fn conn =
    let ( let* ) = Result.bind in
    let* job_id, readme = Model.job_and_readme job_name conn in
    let* builds = Model.builds_grouped_by_output_with_failed job_id platform conn in
    Ok (readme, builds) in
  let open Vif.Response.Syntax in
  Caqti_miou_unix.Pool.use fn pool
  |> or_model_error req @@ function
  | (readme, builds) when is_accept_json req ->
    let json = Views.Job.make_json ~failed:true ~job_name ~platform ~readme builds in
    let str = Yojson.Basic.to_string json in
    let* () = Vif.Response.add ~field:"content-type" "application/json" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `OK
  | (readme, builds) ->
    let html = Views.Job.make ~failed:true ~job_name ~platform ~readme builds in
    let* () = Vif.Response.with_tyxml req html in
    Vif.Response.respond `OK

let redirect_latest req job_name path server _cfg =
  let pool = Vif.Server.device caqti server in
  let platform = fix_q (Vif.Queries.get req "platform") in
  let artifact = path in
  let fn conn =
    let ( let* ) = Result.bind in
    let* job_id = Model.job_id job_name conn in
    let* job_id = Model.not_found job_id in
    let* build = Model.latest_successful_build_uuid job_id platform conn in
    Model.not_found build in
  let open Vif.Response.Syntax in
  Caqti_miou_unix.Pool.use fn pool
  |> or_model_error req @@ fun build ->
  let uri = Link.Job_build_artifact.make_from_string ~job_name ~build ~artifact () in
  Log.err (fun m -> m "Redirect link %S" uri);
  let* () = Vif.Response.add ~field:"Location" uri in
  let* () = Vif.Response.with_string req String.empty in
  Vif.Response.respond `Temporary_redirect

let redirect_latest_empty req job_name server cfg =
  redirect_latest req job_name "" server cfg

let redirect_main_binary req job_name build server _cfg =
  let pool = Vif.Server.device caqti server in
  let fn conn =
    let ( let* ) = Result.bind in
    let* _build_id, build = Model.build build conn in
    match build.Builder_db.Build.main_binary with
    | None -> Ok None
    | Some artifact ->
      let* artifact = Model.build_artifact_by_id artifact conn in
      Ok (Some artifact)
  in
  let open Vif.Response.Syntax in
  Caqti_miou_unix.Pool.use fn pool
  |> or_model_error req @@ function
  | None ->
    let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
    let html = Views.page_not_found ~target:(Vif.Request.target req) ~referer:None in
    let* () = Vif.Response.with_tyxml req html in
    Vif.Response.respond `Not_found
  | Some artifact ->
    let url =
      let open Vif.Uri in
      rel / "job" /% string `Path / "build" /% Url.uuid / "f" /% path /?? any
    in
    let* () = Vif.Response.empty in
    Vif.Response.redirect_to req url
      job_name build (Fpath.to_string artifact.Builder_db.filepath)

let job_build req job_name build server cfg =
  let pool = Vif.Server.device caqti server in
  let fn conn =
    let ( let* ) = Result.bind in
    let* (build_id, build) = Model.build build conn in
    let* main_binary =
      match build.Builder_db.Build.main_binary with
      | None -> Ok None
      | Some artifact ->
        let* artifact = Model.build_artifact_by_id artifact conn in
        Ok (Some artifact)
    in
    let* artifacts = Model.build_artifacts build_id conn in
    let* same_input_same_output = Model.builds_with_same_input_and_same_main_binary build_id conn in
    let* different_input_same_output = Model.builds_with_different_input_and_same_main_binary build_id conn in
    let* same_input_different_output = Model.builds_with_same_input_and_different_main_binary build_id conn in
    let* latest = Model.latest_successful_build build.job_id (Some build.Builder_db.Build.platform) conn in
    let* next = Model.next_successful_build_different_output build_id conn in
    let* previous = Model.previous_successful_build_different_output build_id conn in
    Ok (build, main_binary, artifacts, same_input_same_output, different_input_same_output, same_input_different_output, latest, next, previous)
  in
  let open Vif.Response.Syntax in
  Caqti_miou_unix.Pool.use fn pool
  |> or_model_error req
  @@ fun (build, main_binary, artifacts, same_input_same_output, different_input_same_output, same_input_different_output, latest, next, previous) ->
  let solo5_manifest = Option.bind main_binary (Model.solo5_manifest cfg.datadir) in
  let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
  let html =
    Views.Job_build.make
      ~job_name
      ~build
      ~artifacts
      ~main_binary
      ~solo5_manifest
      ~same_input_same_output
      ~different_input_same_output
      ~same_input_different_output
      ~latest
      ~next
      ~previous
  in
  let* () = Vif.Response.with_tyxml req html in
  Vif.Response.respond `OK

let job_build_file req _job_name build path server cfg =
  let pool = Vif.Server.device caqti server in
  let if_none_match = Vif.Headers.get (Vif.Request.headers req) "if-none-match" in
  let fn path conn = Model.build_artifact build path conn in
  let open Vif.Response.Syntax in
  match Fpath.of_string path with
  | Error `Msg _e ->
    let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
    (* FIXME: referer *)
    let html = Views.page_not_found ~target:(Vif.Request.target req) ~referer:None in
    let* () = Vif.Response.with_tyxml req html in
    Vif.Response.respond `Not_found
  | Ok path ->
    Caqti_miou_unix.Pool.use (fn path) pool
    |> or_model_error req @@ fun artifact ->
    let etag = Base64.encode_string artifact.Builder_db.sha256 in
    match if_none_match with
    | Some etag' when etag = etag' ->
      let* () = Vif.Response.empty in
      Vif.Response.respond `Not_modified
    | _ ->
      let path = Fpath.append cfg.datadir (Model.artifact_path artifact) in
      let mime = mime_lookup artifact.Builder_db.filepath in
      Vif.Response.with_file ~mime ~etag req path

let job_build_static_file req _job_name build (file : [< `Console | `Script ]) server cfg =
  let pool = Vif.Server.device caqti server in
  let fn conn =
    match file with
    | `Console ->
      Model.build_console_by_uuid cfg.datadir build conn
    | `Script ->
      Model.build_script_by_uuid cfg.datadir build conn
  in
  let open Vif.Response.Syntax in
  Caqti_miou_unix.Pool.use fn pool
  |> or_model_error req @@ fun filepath ->
  let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
  let* () = Vif.Response.with_source req (Vif.Stream.Source.file (Fpath.to_string filepath)) in
  Vif.Response.respond `OK

let job_build_viz req _job_name build viz server cfg =
  let pool = Vif.Server.device caqti server in
  let open Vif.Response.Syntax in
  let fn =
    Model.Viz.try_load_cached_visualization
      ~datadir:cfg.datadir ~uuid:build viz
  in
  Caqti_miou_unix.Pool.use fn pool
  |> or_model_error req @@ fun filepath ->
  let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
  let* () = Vif.Response.with_source req (Vif.Stream.Source.file (Fpath.to_string filepath)) in
  Vif.Response.respond `OK

let exec req _job_name build server cfg =
  let pool = Vif.Server.device caqti server in
  let fn = Model.exec_of_build cfg.datadir build in
  let open Vif.Response.Syntax in
  Caqti_miou_unix.Pool.use fn pool
  |> or_model_error req @@ fun exec ->
  let* () = Vif.Response.add ~field:"content-type" "application/octet-stream" in
  let* () = Vif.Response.with_string req exec in
  Vif.Response.respond `OK

let process_comparison datadir build_left_uuid build_right_uuid conn =
  let ( let* ) = Result.bind in
  let data build path = Model.build_artifact_data datadir build (Fpath.v path) conn in
  let resolve_artifact_size = function
    | None -> Ok None
    | Some id ->
      let* file = Model.build_artifact_by_id id conn in
      Ok (Some file.size)
  in
  let* (_id, build_left) = Model.build build_left_uuid conn in
  let* (_id, build_right) = Model.build build_right_uuid conn in
  let* switch_left = data build_left_uuid "opam-switch" in
  let* build_env_left = data build_left_uuid "build-environment" in
  let* system_pkgs_left = data build_left_uuid "system-packages" in
  let* switch_right = data build_right_uuid "opam-switch" in
  let* build_env_right = data build_right_uuid "build-environment" in
  let* system_pkgs_right = data build_right_uuid "system-packages" in
  let* build_left_file_size = resolve_artifact_size build_left.main_binary in
  let* build_right_file_size = resolve_artifact_size build_right.main_binary in
  let* job_left = Model.job_name build_left.job_id conn in
  let* job_right = Model.job_name build_right.job_id conn in

  Ok (job_left, job_right, build_left, build_right, build_left_file_size,
      build_right_file_size, switch_left, switch_right,
      build_env_left, build_env_right,
      system_pkgs_left, system_pkgs_right)

let compare_builds req build_left build_right server cfg =
  let pool = Vif.Server.device caqti server in
  let open Vif.Response.Syntax in
  Caqti_miou_unix.Pool.use (process_comparison cfg.datadir build_left build_right) pool
  |> or_model_error req @@
  fun (job_left, job_right, build_left, build_right,
       build_left_file_size, build_right_file_size,
       switch_left, switch_right,
       build_env_left, build_env_right,
       system_pkgs_left, system_pkgs_right) ->
  let env_diff = Utils.compare_env build_env_left build_env_right
  and pkg_diff = Utils.compare_pkgs system_pkgs_left system_pkgs_right
  and switch_left = OpamFile.SwitchExport.read_from_string switch_left
  and switch_right = OpamFile.SwitchExport.read_from_string switch_right in
  let opam_diff = Opamdiff.compare switch_left switch_right in
  if is_accept_json req then
    let json =
      Views.compare_builds_json
        ~job_left ~job_right ~build_left ~build_right
        ~build_left_file_size ~build_right_file_size
        ~env_diff ~pkg_diff ~opam_diff
    in
    let* () = Vif.Response.add ~field:"content-type" "application/json" in
    let* () = Vif.Response.with_string req (Yojson.Basic.to_string json) in
    Vif.Response.respond `OK
  else
    let html =
      Views.compare_builds
        ~job_left ~job_right
        ~build_left ~build_right
        ~env_diff
        ~pkg_diff
        ~opam_diff
    in
    let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
    let* () = Vif.Response.with_tyxml req html in
    Vif.Response.respond `OK

let hash req hash server _cfg =
  let pool = Vif.Server.device caqti server in
  let fn conn =
    Model.build_hash hash conn
  in
  let open Vif.Response.Syntax in
  Caqti_miou_unix.Pool.use fn pool
  |> or_model_error req @@ function
  | None ->
    let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
    let html = Views.page_not_found ~target:(Vif.Request.target req) ~referer:None in
    let* () = Vif.Response.with_tyxml req html in
    Vif.Response.respond `Not_found
  | Some (job_name, build) ->
    let url =
      let open Vif.Uri in
      rel / "job" /% string `Path / "build" /% Url.uuid /?? any
    in
    let* () = Vif.Response.empty in
    Vif.Response.redirect_to req url
      job_name build.Builder_db.Build.uuid

let robots req _server _cfg =
  let open Vif.Response.Syntax in
  let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
  let* () = Vif.Response.with_string req Views.robots_txt in
  Vif.Response.respond `OK

let pp_exec ppf ((job : Builder.script_job), uuid, _, _, _, _, _) =
  Format.fprintf ppf "%s(%a)" job.Builder.name Uuidm.pp uuid

let upload req server { datadir; configdir; _ } =
  authenticated req @@ fun user_id user_info ->
  let src = Vif.Request.source req in
  let data = Vif.Stream.(Stream.into Sink.string (Stream.from src)) in
  let pool = Vif.Server.device caqti server in
  let fn job_name build exec conn =
    let ( let* ) = Result.bind in
    let* authorized = Model.authorized user_id user_info job_name conn in
    if authorized then
      let* exists = Model.build_exists build conn in
      if exists then
        Ok `Conflict
      else
        let* () = Model.add_build ~datadir ~configdir user_id exec conn in
        Ok `Created
    else Ok `Unauthorized
  in
  let open Vif.Response.Syntax in
  match Builder.Asn.exec_of_str data with
  | Error e ->
    Log.warn (fun m -> m "Received bad builder ASN.1 from %S" user_info.username);
    Log.debug (fun m -> m "Bad builder ASN.1: %a" pp_error e);
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Internal server error\n" in
    Vif.Response.respond `Internal_server_error
  | Ok (({ name = job_name; _ }, uuid, _, _, _, _, _) as exec) ->
    Log.debug (fun m -> m "Received build %a" pp_exec exec);
    Caqti_miou_unix.Pool.use (fn job_name uuid exec) pool
    |> or_model_error req @@ function
    | `Unauthorized ->
      let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
      let* () = Vif.Response.with_string req "Unauthorized!\n" in
      Vif.Response.respond `Unauthorized
    | `Conflict ->
      Log.info (fun m -> m "Build with same uuid exists: %a" pp_exec exec);
      let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
      let* () =
        Fmt.kstr (Vif.Response.with_string req) "Build with same uuid exists: %a\n"
          pp_exec exec
      in
      Vif.Response.respond `Conflict
    | `Created ->
      let* () = Vif.Response.empty in
      Vif.Response.respond `Created

let upload_binary req job_name platform server { datadir; configdir; _ } =
  authenticated req @@ fun user_id user_info ->
  let pool = Vif.Server.device caqti server in
  let binary_name =
    fix_q (Vif.Queries.get req "binary_name")
    |> Option.map Fpath.of_string
    |> Option.value ~default:(Ok Fpath.(v job_name + "bin"))
    |> Result.map Fpath.normalize
  in
  let fn binary_path src conn =
    let ( let* ) = Result.bind in
    let* authorized = Model.authorized user_id user_info job_name conn in
    if authorized then
      let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
      let* exists = Model.build_exists uuid conn in
      if exists then
        Ok `Internal_server_error
      else
        let exec =
          let now = Ptime_clock.now () in
          ({ Builder.name = job_name ; platform ; script = "# This artifact was manually built\n" }, uuid, [], now, now, Builder.Exited 0,
           [ binary_path, Vif.Stream.(Stream.into Sink.string (Stream.from src)) ])
        in
        let* () = Model.add_build ~datadir ~configdir user_id exec conn in
        Ok `Created
    else Ok `Unauthorized
  in
  let open Vif.Response.Syntax in
  match binary_name with
  | Error `Msg err ->
    Log.debug (fun m -> m "Client provided bad binary name: %s" err);
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Bad binary name provided.\n" in
    Vif.Response.respond `Bad_request
  | Ok binary_name when not (String.equal (Fpath.basename binary_name) (Fpath.to_string binary_name)) ->
    Log.debug (fun m -> m "Client provided illegal binary name: %a"
                  (Fmt.quote Fpath.pp) binary_name);
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Bad binary name provided.\n" in
    Vif.Response.respond `Bad_request
  | Ok binary_name ->
    let binary_path = Fpath.(v "bin" // binary_name) in
    let src = Vif.Request.source req in
    Caqti_miou_unix.Pool.use (fn binary_path src) pool
    |> or_model_error req @@ function
    | `Internal_server_error ->
      let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
      let* () = Vif.Response.with_string req "Internal server error\n" in
      Vif.Response.respond `Internal_server_error
    | `Unauthorized ->
      let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
      let* () = Vif.Response.with_string req "Unauthorized!\n" in
      Vif.Response.respond `Unauthorized
    | `Created ->
      let* () = Vif.Response.empty in
      Vif.Response.respond `Created

let not_found_handler req target _server _cfg =
  let r =
    let open Vif.Response.Syntax in
    let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
    let html = Views.page_not_found ~target:target ~referer:None in
    let* () = Vif.Response.with_tyxml req html in
    Vif.Response.respond `Not_found
  in
  Some r

let routes =
  let open Vif.Route in
  [
    get Url.root --> builds ~all:false ~filter_builds:true
  ; get Url.all_builds --> builds ~all:true ~filter_builds:false
  ; get Url.failed_builds --> failed_builds
  ; get Url.job --> job
  ; get Url.job_with_failed --> job_with_failed
  ; get Url.redirect_latest --> redirect_latest
  ; get Url.redirect_latest_empty --> redirect_latest_empty
  ; get Url.job_build --> job_build
  ; get Url.job_build_file --> job_build_file
  ; get Url.job_build_static_file --> job_build_static_file
  ; get Url.job_build_viz --> job_build_viz
  ; get Url.exec --> exec
  ; get Url.redirect_main_binary --> redirect_main_binary
  ; get Url.compare_builds --> compare_builds
  ; get Url.hash --> hash
  ; get Url.robots --> robots
  ; post Vif.Type.any Url.upload --> upload
  ; post Vif.Type.any Url.upload_binary --> upload_binary
  ]
