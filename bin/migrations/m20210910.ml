open Grej.Infix

let mixups =
  Caqti_type.unit ->*
  Caqti_type.tup3 (Builder_db.Rep.id (`build : [`build]))
    Builder_db.Rep.fpath Builder_db.Rep.fpath @@
  "SELECT id, console, script FROM build \
   WHERE console LIKE '%/script.txt' AND script LIKE '%/console.txt'"

let fixup =
  Caqti_type.tup3 (Builder_db.Rep.id (`build : [`build]))
    Builder_db.Rep.fpath Builder_db.Rep.fpath ->.
  Caqti_type.unit @@
  "UPDATE build SET console = $2, script = $3 WHERE id = $1"

let fixup _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:14L (module Db) >>= fun () ->
  Db.collect_list mixups () >>= fun mixups ->
  Grej.list_iter_result (fun (id, console, script) ->
      Db.exec fixup (id, script, console))
    mixups
