(* FIXME: log source *)
module Log = Logs

exception Wrong_version  of int32 * int64

(* partial *)
type cfg =
  { sw : Caqti_miou.Switch.t
  ; uri : Uri.t
  ; filter_builds_later_than : int }

type error = [ Caqti_error.t | `Not_found ]

let pp_error ppf = function
  | #Caqti_error.t as err -> Caqti_error.pp ppf err
  | `Not_found -> Fmt.string ppf "Job not found"

let caqti : (cfg, (Caqti_miou.connection, error) Caqti_miou_unix.Pool.t) Vif.D.device =
  let finally pool = Caqti_miou_unix.Pool.drain pool in
  Vif.D.device ~name:"caqti" ~finally [] @@ fun { sw; uri; _ } ->
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
  let pool = Vif.G.device caqti server in
  let than =
    if not filter_builds then
      Ptime.epoch
    else
      let n = Ptime.Span.v (cfg.filter_builds_later_than, 0L) in
      let now = Ptime_clock.now () in
      (* in the old code we use Ptime.Span.sub... *)
      Ptime.sub_span now n |> Option.value ~default:Ptime.epoch
  in
  let open Vif in
  let fn conn =
    let ( let* ) = Result.bind in
    let fn1 job_id job_name platform = function
      | Error _ as err -> err
      | Ok acc ->
          let* res = Model.build_with_main_binary job_id platform conn in
          match res with
          | Some (build, artifact) ->
            if Ptime.is_later ~than build.finish
            then (Log.warn (fun m -> m "is later than"); Ok ((platform, build, artifact) :: acc))
            else (Log.warn (fun m -> m "is earlier than"); Ok acc)
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
    let* () = Response.add ~field:"content-type" "application/json" in
    let* () = Response.with_string req str in
    Response.respond `OK
  | Ok _ -> assert false
  | Error err ->
    let* () = Response.add ~field:"content-type" "text/plain" in
    let str = Fmt.str "Database error: %a" pp_error err in
    let* () = Response.with_string req str in
    Response.respond `Internal_server_error

let job req job_name server _cfg =
  let pool = Vif.G.device caqti server in
  let platform = match Vif.Q.get req "platform" with
    | platform :: _ -> Some platform
    | [] -> None in
  let fn conn =
    let ( let* ) = Result.bind in
    let* job_id, readme = Model.job_and_readme job_name conn in
    let* builds = Model.builds_grouped_by_output job_id platform conn in
    Ok (readme, builds) in
  let open Vif in
  match Caqti_miou_unix.Pool.use fn pool with
  | Error err ->
    let* () = Response.add ~field:"content-type" "text/plain" in
    let str = Fmt.str "Database error: %a" pp_error err in
    let* () = Response.with_string req str in
    Response.respond `Internal_server_error
  | Ok (readme, builds) when is_accept_json req ->
      let json = Views.Job.make_json ~failed:false ~job_name ~platform ~readme builds in
      let str = Yojson.Basic.to_string json in
      let* () = Response.add ~field:"content-type" "application/json" in
      let* () = Response.with_string req str in
      Response.respond `OK
  | Ok (readme, builds) ->
      let html = Views.Job.make ~failed:false ~job_name ~platform ~readme builds in
      let* () = Response.with_tyxml req html in
      Response.respond `OK

let[@warning "-33"] routes () =
  let open Vif.U in
  let open Vif.R in
  [
    get (rel /?? nil) --> builds ~all:false ~filter_builds:true
  ; get (rel / "all-builds" /?? nil) --> builds ~all:true ~filter_builds:false
  ; get (rel / "job" /% string `Path /?? any) --> job
  ]
