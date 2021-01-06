let src = Logs.Src.create "builder-web.model" ~doc:"Builder_web model"
module Log = (val Logs.src_log  src : Logs.LOG)

open Lwt.Syntax
open Lwt_result.Infix

module RunMap = Map.Make(struct
    type t = Fpath.t * Fpath.t
    let compare (j1,r1) (j2,r2) =
      Fpath.(compare (j1 // r1) (j2 // r2))
  end)

type job_run_meta = {
  job_info : Builder.job;
  uuid : Uuidm.t;
  start : Ptime.t;
  finish : Ptime.t;
  result : Builder.execution_result;
}

type job_run_info = {
  meta : job_run_meta;
  out : (int * string) list;
  data : (Fpath.t * string) list
}

type digest = {
  sha256 : Cstruct.t;
}

type t = {
  dir : Fpath.t;
  mutable meta_cache : job_run_meta RunMap.t;
  mutable digest_cache : (Fpath.t * digest) list RunMap.t
}

let init dir = { dir; meta_cache = RunMap.empty; digest_cache = RunMap.empty; }

type job = {
  path : Fpath.t;
  runs : job_run_meta list;
}

let job_name { path; _ } = Fpath.to_string path

let read_full t path run =
  let f = Fpath.(t.dir // path // run / "full") in
  let* ic = Lwt_io.open_file ~mode:Lwt_io.Input (Fpath.to_string f) in
  let+ s = Lwt_io.read ic in
  let open Rresult.R.Infix in
  Builder.Asn.exec_of_cs (Cstruct.of_string s)
  >>| fun (job_info, uuid, out, start, finish, result, data) ->
  let meta = { job_info; uuid; start; finish; result } in
  t.meta_cache <- RunMap.add (path, run) meta t.meta_cache;
  { meta; out; data }

let digest (path, data) =
  let module H = Mirage_crypto.Hash in
  let data = Cstruct.of_string data in
  (path, {
      sha256 = H.SHA256.digest data;
    })

let read_full_with_digests t path run =
  read_full t path run >|= fun ({ data; _ } as full) ->
  match RunMap.find_opt (path, run) t.digest_cache with
  | Some digests -> full, digests
  | None ->
    let digests = List.map digest data in
    t.digest_cache <- RunMap.add (path, run) digests t.digest_cache;
    full, digests

let read_full_meta t path run =
  match RunMap.find_opt (path, run) t.meta_cache with
  | Some meta ->
    Lwt_result.lift (Bos.OS.File.exists Fpath.(t.dir // path // run / "full")) >>= fun exists ->
    if exists
    then Lwt_result.return meta
    else
      (t.meta_cache <- RunMap.remove (path, run) t.meta_cache;
       Lwt_result.fail (`Msg "no such file"))
  | None ->
    read_full t path run >|= fun { meta; out = _; data = _ }  ->
    meta

let job t job =
  let path = Fpath.(t.dir // job) in
  let open Lwt_result.Infix in
  Lwt_result.lift (Bos.OS.Dir.contents ~rel:true path) >>= fun runs ->
  let+ runs =
    Lwt_list.filter_map_s (fun run ->
        let+ meta = read_full_meta t job run in
        match meta with
        | Error (`Msg e) ->
          Log.warn (fun m -> m "error reading job run file %a: %s"
                       Fpath.pp Fpath.(path // run) e);
          None
        | Ok meta -> Some meta)
      runs
  in
  Ok { path = job; runs }

let jobs t =
  let r =
    let open Rresult.R.Infix in
    Bos.OS.Dir.contents ~rel:true t.dir >>|
    List.filter (fun f -> not (Fpath.equal (Fpath.v "state") f)) >>|
    List.filter_map (fun f ->
        match Bos.OS.Dir.exists Fpath.(t.dir // f) with
        | Ok true -> Some f
        | Ok false ->
          Log.warn (fun m -> m "dir %a doesn't exist" Fpath.pp
                       Fpath.(t.dir // f));
          None
        | Error (`Msg e) ->
          Log.warn (fun m -> m "error reading job dir %a: %s" Fpath.pp
                       Fpath.(t.dir // f) e);
          None)
  in Lwt.return r
