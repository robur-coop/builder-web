let new_version = 16L and old_version = 15L
and identifier = "2022-05-09"
and migrate_doc = "switch uuid encoding to hex"
and rollback_doc = "switch uuid encoding back to binary"

let old_uuid_rep =
  let encode uuid = Ok (Uuidm.to_bytes uuid) in
  let decode s =
    Uuidm.of_bytes s
    |> Option.to_result ~none:"failed to decode uuid"
  in
  Caqti_type.custom ~encode ~decode Caqti_type.string

let uuids_byte_encoded_q =
  Caqti_request.collect ~oneshot:true
    Caqti_type.unit
    (Caqti_type.tup2 (Builder_db.Rep.id (`build : [`build])) old_uuid_rep)
    "SELECT id, uuid FROM build"

let uuids_hex_encoded_q =
  Caqti_request.collect ~oneshot:true
    Caqti_type.unit
    (Caqti_type.tup2 (Builder_db.Rep.id (`build : [`build])) Builder_db.Rep.uuid)
    "SELECT id, uuid FROM build"

let migrate_q =
  Caqti_request.exec ~oneshot:true
    (Caqti_type.tup2 (Builder_db.Rep.id (`build : [`build])) Builder_db.Rep.uuid)
    "UPDATE build SET uuid = $2 WHERE id = $1"

let rollback_q =
  Caqti_request.exec ~oneshot:true
    (Caqti_type.tup2 (Builder_db.Rep.id (`build : [`build])) old_uuid_rep)
    "UPDATE build SET uuid = $2 WHERE id = $1"

let migrate _datadir (module Db : Caqti_blocking.CONNECTION) =
  let open Grej.Infix in
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.collect_list uuids_byte_encoded_q () >>= fun old_uuids ->
  Grej.list_iter_result (Db.exec migrate_q) old_uuids >>= fun () ->
  Db.exec (Grej.set_version new_version) ()

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  let open Grej.Infix in
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
  Db.collect_list uuids_hex_encoded_q () >>= fun new_uuids ->
  Grej.list_iter_result (Db.exec rollback_q) new_uuids >>= fun () ->
  Db.exec (Grej.set_version old_version) ()
