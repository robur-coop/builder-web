(* Grej is utilities *)
module Syntax = struct
  open Caqti_request.Infix
  let ( let* ) = Result.bind
  let ( let+ ) x f = Result.map f x
  let ( ->. ) = ( ->. ) ~oneshot:true
  let ( ->! ) = ( ->! ) ~oneshot:true
  let ( ->? ) = ( ->? ) ~oneshot:true
  let ( ->* ) = ( ->* ) ~oneshot:true
end

module Infix = struct
  open Caqti_request.Infix
  let ( >>= ) = Result.bind
  let ( >>| ) x f = Result.map f x
  let ( ->. ) = ( ->. ) ~oneshot:true
  let ( ->! ) = ( ->! ) ~oneshot:true
  let ( ->? ) = ( ->? ) ~oneshot:true
  let ( ->* ) = ( ->* ) ~oneshot:true
end

open Syntax

let set_version version =
  Caqti_type.unit ->. Caqti_type.unit @@
  Printf.sprintf "PRAGMA user_version = %Ld" version

let check_version
    ?application_id:(desired_application_id=Builder_db.application_id)
    ~user_version:desired_user_version
    (module Db : Caqti_blocking.CONNECTION) =
  let* application_id = Db.find Builder_db.get_application_id () in
  let* user_version = Db.find Builder_db.get_version () in
  if application_id <> desired_application_id || user_version <> desired_user_version
  then Error (`Wrong_version (application_id, user_version))
  else Ok ()

let list_iter_result f xs =
  List.fold_left
    (fun r x -> let* () = r in f x)
    (Ok ())
    xs

let foreign_keys on =
  let on = if on then "ON" else "OFF" in
  Caqti_type.unit ->. Caqti_type.unit @@
  Printf.sprintf "PRAGMA foreign_keys = %s" on
