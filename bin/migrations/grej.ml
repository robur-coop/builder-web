(* Grej is utilities *)
open Rresult.R.Infix

let set_version version =
  Caqti_request.exec ~oneshot:true
    Caqti_type.unit
    (Printf.sprintf "PRAGMA user_version = %Ld" version)

let check_version
    ?application_id:(desired_application_id=Builder_db.application_id)
    ~user_version:desired_user_version
    (module Db : Caqti_blocking.CONNECTION) =
  Db.find Builder_db.get_application_id () >>= fun application_id ->
  Db.find Builder_db.get_version () >>= fun user_version ->
  if application_id <> desired_application_id || user_version <> desired_user_version
  then Error (`Wrong_version (application_id, user_version))
  else Ok ()

let list_iter_result f xs =
  List.fold_left
    (fun r x -> r >>= fun () -> f x)
    (Ok ())
    xs

let foreign_keys on =
  let on = if on then "ON" else "OFF" in
  Caqti_request.exec
    Caqti_type.unit
    (Printf.sprintf "PRAGMA foreign_keys = %s" on)
