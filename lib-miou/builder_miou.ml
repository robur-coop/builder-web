(* FIXME: log source *)
module Log = Logs

let default_log_warn ~status _e =
  Log.warn (fun m -> m "%a: some error" H2.Status.pp_hum status)

let if_error server ?(status = `Internal_server_error)
    ?(log = default_log_warn ~status)
    ?(message = "") r =
  match r with
  | Error e ->
    log e;
    Vif.Response.with_string server status message
  | Ok r -> r

let filter_builds_later_than_as_arg, filter_builds_later_than =
  Vif.D.device ~name:"filter_builds_later_than" ~finally:(Fun.const ()) [] 30

let use_pool ?priority pool fn =
  Caqti_miou_unix.Pool.use ?priority fn pool

let builds ?(filter_builds = false) ~all server req pool =
  match Vif.Request.meth req with
  | `GET | `HEAD ->
    if_error server ~status:`Internal_server_error ~log:(fun e ->
        Log.err (fun m -> m "database error %a" Caqti_error.pp e))
    @@
    Fun.flip Caqti_miou_unix.Pool.use pool @@ fun (module Db : Caqti_miou.CONNECTION) ->
    let than =
      if filter_builds then
        Ptime.epoch
      else
        let filter_builds_later_than = Vif.S.device filter_builds_later_than server in
        let n = Ptime.Span.v (filter_builds_later_than, 0L) in
        let now = Ptime_clock.now () in
        (* in the old code we use Ptime.Span.sub... *)
        Ptime.sub_span now n |> Option.value ~default:Ptime.epoch
    in
    let[@warning "-8"] (Ok application_id) = Db.find Builder_db.get_application_id () in
    ignore all; ignore than;
    Printf.ksprintf (Vif.Response.with_string server `OK)
      "TODO\nApplication ID: %ld\n" application_id;
    Ok ()
  | _ ->
    Vif.Response.with_string server `Method_not_allowed ""

let routes () =
  let open Vif.U in
  let open Vif.R in
  [
    rel /?? nil --> builds ~all:false ~filter_builds:true ;
  ]

