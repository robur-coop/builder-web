let new_version = 9L and old_version = 8L
let identifier = "2021-06-29"
let migrate_doc = "add tag and job_tag table"
let rollback_doc = "remove tag and job tag table"

open Grej.Infix

let tag =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| CREATE TABLE tag (
       id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
       tag VARCHAR(255) NOT NULL UNIQUE
     )
  |}

let job_tag =
  Caqti_type.unit ->. Caqti_type.unit @@
  {| CREATE TABLE job_tag (
       id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
       tag INTEGER NOT NULL,
       value TEXT NOT NULL,
       job INTEGER NOT NULL,

       FOREIGN KEY(job) REFERENCES job(id),
       FOREIGN KEY(tag) REFERENCES tag(id),
       UNIQUE(tag, job)
     )
  |}

let jobs =
  Caqti_type.unit ->* Builder_db.Rep.untyped_id @@
  "SELECT id FROM job"

let latest_successful_build =
  Builder_db.Rep.untyped_id ->? Builder_db.Rep.untyped_id @@
  {| SELECT b.id
     FROM build b
     WHERE b.job = ? AND b.result_kind = 0 AND b.result_code = 0
     ORDER BY b.start_d DESC, b.start_ps DESC
     LIMIT 1
  |}

let build_artifacts =
  Builder_db.Rep.untyped_id ->*
  Caqti_type.tup2 Builder_db.Rep.fpath Builder_db.Rep.fpath @@
  {| SELECT a.filepath, a.localpath
     FROM build_artifact a
     WHERE a.build = ?
  |}


let infer_section_and_synopsis artifacts =
  let opam_switch =
    match List.find_opt (fun (p, _) -> String.equal (Fpath.basename p) "opam-switch") artifacts with
    | None -> None
    | Some (_, data) -> Some (OpamFile.SwitchExport.read_from_string data)
  in
  let infer_synopsis_and_descr switch =
    let root = switch.OpamFile.SwitchExport.selections.OpamTypes.sel_roots in
    if OpamPackage.Set.cardinal root <> 1 then
      None, None
    else
      let root = OpamPackage.Set.choose root in
      match OpamPackage.Name.Map.find_opt root.OpamPackage.name switch.OpamFile.SwitchExport.overlays with
      | None -> None, None
      | Some opam -> OpamFile.OPAM.synopsis opam, OpamFile.OPAM.descr_body opam
  in
  let infer_section_from_packages switch =
    let influx = OpamPackage.Name.of_string "metrics-influx" in
    if OpamPackage.Set.exists (fun p -> OpamPackage.Name.equal p.OpamPackage.name influx)
         switch.OpamFile.SwitchExport.selections.OpamTypes.sel_installed
    then
      "Unikernel (with metrics reported to Influx)"
    else
      "Unikernel"
  in
  let infer_section_from_extension =
     match List.find_opt (fun (p, _) -> Fpath.(is_prefix (v "bin/") p)) artifacts with
     | None -> None
     | Some (p, _) ->
       match Fpath.get_ext p with
       | ".deb" -> Some "Debian Package"
       | ".txz" -> Some "FreeBSD Package"
       | _ -> None
  in
  match opam_switch with
  | None -> None, (None, None)
  | Some opam_switch ->
    let section =
      match infer_section_from_extension with
      | Some x -> x
      | None -> infer_section_from_packages opam_switch
    in
    Some section, infer_synopsis_and_descr opam_switch

let remove_tag =
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP TABLE tag"

let remove_job_tag =
  Caqti_type.unit ->. Caqti_type.unit @@
  "DROP TABLE job_tag"

let insert_tag =
  Caqti_type.string ->. Caqti_type.unit @@
  "INSERT INTO tag (tag) VALUES (?)"

let insert_job_tag =
  Caqti_type.(tup3 Builder_db.Rep.untyped_id string Builder_db.Rep.untyped_id) ->.
  Caqti_type.unit @@
  "INSERT INTO job_tag (tag, value, job) VALUES (?, ?, ?)"

let find_tag =
  Caqti_type.string ->! Builder_db.Rep.untyped_id @@
  "SELECT id FROM tag where tag = ?"

let migrate datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:old_version (module Db) >>= fun () ->
  Db.exec tag () >>= fun () ->
  Db.exec job_tag () >>= fun () ->
  Db.exec insert_tag "section" >>= fun () ->
  Db.exec insert_tag "synopsis" >>= fun () ->
  Db.exec insert_tag "description" >>= fun () ->
  Db.find find_tag "section" >>= fun section_id ->
  Db.find find_tag "synopsis" >>= fun synopsis_id ->
  Db.find find_tag "description" >>= fun descr_id ->
  Db.collect_list jobs () >>= fun jobs ->
  Grej.list_iter_result (fun job ->
    Db.find_opt latest_successful_build job >>= function
    | None ->
      Ok ()
    | Some build ->
      Db.collect_list build_artifacts build >>= fun artifacts ->
      List.fold_left (fun acc (fpath, lpath) ->
          acc >>= fun acc ->
          Bos.OS.File.read Fpath.(append datadir lpath) >>= fun data ->
          Ok ((fpath, data) :: acc))
        (Ok [])
        artifacts >>= fun files ->
      let sec_syn = infer_section_and_synopsis files in
      (match fst sec_syn with None -> Ok () | Some s -> Db.exec insert_job_tag (section_id, s, job)) >>= fun () ->
      (match snd sec_syn with None, _ -> Ok () | Some s, _ -> Db.exec insert_job_tag (synopsis_id, s, job)) >>= fun () ->
      (match snd sec_syn with _, None -> Ok () | _, Some s -> Db.exec insert_job_tag (descr_id, s, job)))
    jobs >>= fun () ->
  Db.exec (Grej.set_version new_version) ()

let rollback _datadir (module Db : Caqti_blocking.CONNECTION) =
  Grej.check_version ~user_version:new_version (module Db) >>= fun () ->
  Db.exec remove_tag () >>= fun () ->
  Db.exec remove_job_tag () >>= fun () ->
  Db.exec (Grej.set_version old_version) ()


