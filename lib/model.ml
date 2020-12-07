let src = Logs.Src.create "builder-web.model" ~doc:"Builder_web model"
module Log = (val Logs.src_log  src : Logs.LOG)

open Rresult.R.Infix

type t = {
  dir : Fpath.t;
}

type job = {
  path : Fpath.t;
  runs : Fpath.t list;
}

let job_name { path; _ } = Fpath.to_string path

type job_run_info = {
  job_info : Builder.job;
  uuid : Uuidm.t;
  out : (int * string) list;
  start : Ptime.t;
  finish : Ptime.t;
  result : Builder.execution_result;
  data : (Fpath.t * string) list
}

let read_full t path run =
  let f = Fpath.(t.dir // path // run / "full") in
  Bos.OS.File.read f >>= fun s ->
  Builder.Asn.exec_of_cs (Cstruct.of_string s)
  >>| fun (job_info, uuid, out, start, finish, result, data) ->
  { job_info; uuid; out; start; finish; result; data }

let job t job =
  Bos.OS.Dir.contents ~rel:true Fpath.(t.dir // job) >>= fun job_runs ->
  Ok { path = job; runs = job_runs }

let jobs t =
  Bos.OS.Dir.contents ~rel:true t.dir >>|
  List.filter (fun f -> not (Fpath.equal (Fpath.v "state") f)) >>|
  List.filter_map (fun f ->
      match job t f with
      | Ok job -> Some job
      | Error (`Msg e) ->
        Log.warn (fun m -> m "Error reading job run dir %a: %s" Fpath.pp
                     Fpath.(t.dir // f) e);
        None)
