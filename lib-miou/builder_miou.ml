let src = Logs.Src.create "builder.miou"

module Log = (val Logs.src_log src : Logs.LOG)

exception Wrong_version of int32 * int64

type cfg =
  { sw : Caqti_miou.Switch.t
  ; uri : Uri.t
  ; datadir : Fpath.t
  ; filter_builds_later_than : int }

type error = [ Caqti_error.t | `Not_found | `Msg of string | `File_error of Fpath.t ]

let pp_error ppf = function
  | #Caqti_error.t as err -> Caqti_error.pp ppf err
  | `Not_found -> Fmt.string ppf "Job not found"
  | `Msg e -> Fmt.string ppf e
  | `File_error path -> Fmt.pf ppf "Error reading file %a" Fpath.pp path

let hd_opt = function x :: _ -> Some x | [] -> None

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
      | Error e | Ok (Error e, _ | _, Error e) ->
          ignore e;
          Fmt.failwith "Error getting database version: %s" "TODO"

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
  let redirect_latest = prefix_job / "build" / "latest" /?? any
  let job_build = rel / "job" /% string `Path / "build" /% uuid /?? any
  let job_build_file = rel / "job" /% string `Path / "build" /% uuid / "f" /% path /?? any
  let job_build_static_file = rel / "job" /% string `Path / "build" /% uuid /% script_or_console /?? any
  let job_build_viz = rel / "job" /% string `Path / "build" /% uuid /% viz /?? any
  let redirect_main_binary = rel / "job" /% string `Path / "build" /% uuid / "main-binary" /?? any
  let exec = rel / "job" /% string `Path / "build" /% uuid / "exec" /?? any

  let hash = rel / "hash" /?? ("sha256", hex) ** any

  let robots = rel / "robots.txt" /?? any
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
let string_of_html =
  Format.asprintf "%a" (Tyxml.Html.pp ())

let default_log_warn ~status _e =
  Log.warn (fun m -> m "%a: some error" H2.Status.pp_hum status)

(*
let if_error server ?(status = `Internal_server_error)
    ?(log = default_log_warn ~status)
    ?(message = "") r =
  match r with
  | Error e ->
    log e;
    Vif.Response.with_string server status message
  | Ok r -> r
*)

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
  match Caqti_miou_unix.Pool.use fn pool with
  | Ok jobs when is_accept_json req ->
    let json = Views.Builds.make_json ~all jobs in
    let str = Yojson.Basic.to_string json in
    let* () = Vif.Response.add ~field:"content-type" "application/json" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `OK
  | Ok jobs ->
    let html = Views.Builds.make ~all jobs in
    let str = string_of_html html in
    let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `OK
  | Error err ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain" in
    let str = Fmt.str "Database error: %a" pp_error err in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `Internal_server_error

let failed_builds req server _cfg =
  let pool = Vif.Server.device caqti server in
  let platform = hd_opt (Vif.Queries.get req "platform") in
  let start, count =
    let to_int default s = Option.(value ~default (bind s int_of_string_opt)) in
    to_int 0 (hd_opt (Vif.Queries.get req "start")),
    to_int 20 (hd_opt (Vif.Queries.get req "count"))
  in
  let fn conn = Model.failed_builds ~start ~count platform conn in
  let open Vif.Response.Syntax in
  match Caqti_miou_unix.Pool.use fn pool with
  | Error err ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    Log.warn (fun m -> m "Database error: %a" pp_error err);
    let* () = Vif.Response.with_string req "Internal server error.\n" in
    Vif.Response.respond `Internal_server_error
  | Ok builds ->
    let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
    let html = Views.failed_builds ~start ~count builds in
    let* () = Vif.Response.with_tyxml req html in
    Vif.Response.respond `OK

let job req job_name server _cfg =
  let pool = Vif.Server.device caqti server in
  let platform = hd_opt (Vif.Queries.get req "platform") in
  let fn conn =
    let ( let* ) = Result.bind in
    let* job_id, readme = Model.job_and_readme job_name conn in
    let* builds = Model.builds_grouped_by_output job_id platform conn in
    Ok (readme, builds) in
  let open Vif.Response.Syntax in
  match Caqti_miou_unix.Pool.use fn pool with
  | Error err ->
    let* () = Vif.Response.add ~field:"content-type" "text/plain" in
    let str = Fmt.str "Database error: %a" pp_error err in
    let* () = Vif.Response.with_string req str in
    Vif.Response.respond `Internal_server_error
  | Ok (readme, builds) when is_accept_json req ->
      let json = Views.Job.make_json ~failed:false ~job_name ~platform ~readme builds in
      let str = Yojson.Basic.to_string json in
      let* () = Vif.Response.add ~field:"content-type" "application/json" in
      let* () = Vif.Response.with_string req str in
      Vif.Response.respond `OK
  | Ok (readme, builds) ->
      let html = Views.Job.make ~failed:false ~job_name ~platform ~readme builds in
      let* () = Vif.Response.with_tyxml req html in
      Vif.Response.respond `OK

let job_with_failed req job_name server _cfg =
  let pool = Vif.Server.device caqti server in
  let platform = hd_opt (Vif.Queries.get req "platform") in
  let fn conn =
    let ( let* ) = Result.bind in
    let* job_id, readme = Model.job_and_readme job_name conn in
    let* builds = Model.builds_grouped_by_output_with_failed job_id platform conn in
    Ok (readme, builds) in
  let open Vif.Response.Syntax in
  match Caqti_miou_unix.Pool.use fn pool with
  | Error err ->
      let* () = Vif.Response.add ~field:"content-type" "text/plain" in
      let str = Fmt.str "Database error: %a" pp_error err in
      let* () = Vif.Response.with_string req str in
      Vif.Response.respond `Internal_server_error
  | Ok (readme, builds) when is_accept_json req ->
      let json = Views.Job.make_json ~failed:true ~job_name ~platform ~readme builds in
      let str = Yojson.Basic.to_string json in
      let* () = Vif.Response.add ~field:"content-type" "application/json" in
      let* () = Vif.Response.with_string req str in
      Vif.Response.respond `OK
  | Ok (readme, builds) ->
      let html = Views.Job.make ~failed:true ~job_name ~platform ~readme builds in
      let* () = Vif.Response.with_tyxml req html in
      Vif.Response.respond `OK

let redirect_latest req job_name server _cfg =
  let pool = Vif.Server.device caqti server in
  let platform = hd_opt (Vif.Queries.get req "platform") in
  let artifact = String.empty in
  let fn conn =
    let ( let* ) = Result.bind in
    let* job_id = Model.job_id job_name conn in
    let* job_id = Model.not_found job_id in
    let* build = Model.latest_successful_build_uuid job_id platform conn in
    Model.not_found build in
  let open Vif.Response.Syntax in
  match Caqti_miou_unix.Pool.use fn pool with
  | Error err ->
      let* () = Vif.Response.add ~field:"content-type" "text/plain" in
      let str = Fmt.str "Database error: %a" pp_error err in
      let* () = Vif.Response.with_string req str in
      Vif.Response.respond `Internal_server_error
  | Ok build ->
    let uri = Link.Job_build_artifact.make_from_string ~job_name ~build ~artifact () in
    let* () = Vif.Response.add ~field:"Location" uri in
    let* () = Vif.Response.with_string req String.empty in
    Vif.Response.respond `Moved_permanently

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
  match Caqti_miou_unix.Pool.use fn pool with
  | Error err ->
    Log.warn (fun m -> m "Database error: %a" pp_error err);
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Internal server error\n" in
    Vif.Response.respond `Internal_server_error
  | Ok None ->
    let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
    let html = Views.page_not_found ~target:(Vif.Request.target req) ~referer:None in
    let* () = Vif.Response.with_tyxml req html in
    Vif.Response.respond `Not_found
  | Ok Some artifact ->
    let url =
      let open Vif.Uri in
      rel / "job" /% string `Path / "build" /% Url.uuid / "f" /% path /?? any
    in
    let* () = Vif.Response.empty in
    Vif.Response.redirect_to req url
      job_name build (Fpath.to_string artifact.filepath)

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
  match Caqti_miou_unix.Pool.use fn pool with
  | Error err ->
    Log.warn (fun m -> m "Database error: %a" pp_error err);
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Internal server error\n" in
    Vif.Response.respond `Internal_server_error
  | Ok (build, main_binary, artifacts, same_input_same_output, different_input_same_output, same_input_different_output, latest, next, previous) ->
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
    let* () = Vif.Response.with_string req (string_of_html html) in
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
    let* () = Vif.Response.with_string req (string_of_html html) in
    Vif.Response.respond `Not_found
  | Ok path ->
    match Caqti_miou_unix.Pool.use (fn path) pool with
    | Error err ->
      Log.warn (fun m -> m "Database error: %a" pp_error err);
      let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
      let* () = Vif.Response.with_string req "Internal server error\n" in
      Vif.Response.respond `Internal_server_error
    | Ok artifact ->
      let etag = Base64.encode_string artifact.Builder_db.sha256 in
      match if_none_match with
      | Some etag' when etag = etag' ->
        let* () = Vif.Response.empty in
        Vif.Response.respond `Not_modified
      | _ ->
        let path = Model.build_artifact_path cfg.datadir artifact in
        (* XXX: do we need to sanitize the path further?! *)
        (* TODO: error handling for the file?! *)
        let stream = Vif.Stream.Source.file (Fpath.to_string path) in
        let* () = Vif.Response.add ~field:"content-type" (mime_lookup artifact.Builder_db.filepath) in
        let* () = Vif.Response.add ~field:"etag" etag in
        let* () = Vif.Response.with_source req stream in
        Vif.Response.respond `OK

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
  match Caqti_miou_unix.Pool.use fn pool with
  | Error err ->
    Log.warn (fun m -> m "Database error: %a" pp_error err);
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Internal server error\n" in
    Vif.Response.respond `Internal_server_error
  | Ok filepath ->
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
  match Caqti_miou_unix.Pool.use fn pool with
  | Error err ->
    Log.warn (fun m -> m "Database error: %a" pp_error err);
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Internal server error\n" in
    Vif.Response.respond `Internal_server_error
  | Ok filepath ->
    let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
    let* () = Vif.Response.with_source req (Vif.Stream.Source.file (Fpath.to_string filepath)) in
    Vif.Response.respond `OK

let exec req _job_name build server cfg =
  let pool = Vif.Server.device caqti server in
  let fn = Model.exec_of_build cfg.datadir build in
  let open Vif.Response.Syntax in
  match Caqti_miou_unix.Pool.use fn pool with
  | Error err ->
    Log.warn (fun m -> m "Database error: %a" pp_error err);
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Internal server error\n" in
    Vif.Response.respond `Internal_server_error
  | Ok exec ->
    let* () = Vif.Response.add ~field:"content-type" "application/octet-stream" in
    let* () = Vif.Response.with_string req exec in
    Vif.Response.respond `OK


let hash req hash server _cfg =
  let pool = Vif.Server.device caqti server in
  let fn conn =
    Model.build_hash hash conn
  in
  let open Vif.Response.Syntax in
  match Caqti_miou_unix.Pool.use fn pool with
  | Error err ->
    Log.warn (fun m -> m "Database error: %a" pp_error err);
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Vif.Response.with_string req "Internal server error\n" in
    Vif.Response.respond `Internal_server_error
  | Ok None ->
    let* () = Vif.Response.add ~field:"content-type" "text/html; charset=utf-8" in
    let html = Views.page_not_found ~target:(Vif.Request.target req) ~referer:None in
    let* () = Vif.Response.with_tyxml req html in
    Vif.Response.respond `Not_found
  | Ok Some (job_name, build) ->
    let url =
      let open Vif.Uri in
      rel / "job" /% string `Path / "build" /% Url.uuid /?? any
    in
    let* () = Vif.Response.empty in
    Vif.Response.redirect_to req url
      job_name build.uuid

let robots req _server _cfg =
  let open Vif.Response.Syntax in
  let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
  let* () = Vif.Response.with_string req Views.robots_txt in
  Vif.Response.respond `OK

let auth_test req _server _cfg =
  authenticated req @@ fun user_id user_info ->
  let open Vif.Response.Syntax in
    let* () = Vif.Response.add ~field:"content-type" "text/plain; charset=utf-8" in
    let* () = Fmt.kstr (Vif.Response.with_string req) "Authorized as %S (id %Lu).\n" user_info.username (Builder_db.Rep.id_to_int64 user_id) in
    Vif.Response.respond `Unauthorized


let[@warning "-33"] routes () =
  let open Vif.Route in
  [
    get Url.root --> builds ~all:false ~filter_builds:true
  ; get Url.all_builds --> builds ~all:true ~filter_builds:false
  ; get Url.failed_builds --> failed_builds
  ; get Url.job --> job
  ; get Url.job_with_failed --> job_with_failed
  ; get Url.redirect_latest --> redirect_latest
  ; get Url.job_build --> job_build
  ; get Url.job_build_file --> job_build_file
  ; get Url.job_build_static_file --> job_build_static_file
  ; get Url.job_build_viz --> job_build_viz
  ; get Url.exec --> exec
  ; get Url.redirect_main_binary --> redirect_main_binary
  ; get Url.hash --> hash
  ; get Url.robots --> robots
  (* TODO:
    `Get, "/job/:job/build/:build/all.tar.gz", (w job_build_targz);
    `Get, "/compare/:build_left/:build_right", (w compare_builds);
    `Post, "/upload", (Authorization.authenticate (w upload));
    `Post, "/job/:job/platform/:platform/upload", (Authorization.authenticate (w upload_binary));
  *)
    ; get Vif.Uri.(rel / "auth-test" /?? any) --> auth_test
  ]
