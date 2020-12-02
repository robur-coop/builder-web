let src = Logs.Src.create "builder-web.model" ~doc:"Builder_web model"
module Log = (val Logs.src_log  src : Logs.LOG)

open Rresult.R.Infix

type t = {
  dir : Fpath.t;
}

type job_run = Fpath.t

type job = {
  name : Fpath.t;
  runs : job_run list;
}

let job_name { name; _ } = Fpath.to_string name
(* TODO: ensure invariant: jobs are always valid UUIDs *)
let job_run_uuid f = Option.get (Uuidm.of_string (Fpath.to_string f))

type job_run_info = {
  job_info : Builder.job;
  uuid : Uuidm.t;
  out : (int * string) list;
  start : Ptime.t;
  finish : Ptime.t;
  result : Builder.execution_result;
  data : (Fpath.t * string) list
}

let read_full t job run =
  let f = Fpath.(t.dir // job.name // run / "full") in
  Bos.OS.File.read f >>= fun s ->
  Builder.Asn.exec_of_cs (Cstruct.of_string s)
  >>| fun (job_info, uuid, out, start, finish, result, data) ->
  { job_info; uuid; out; start; finish; result; data }

let job_runs t job =
  Bos.OS.Dir.contents ~rel:true Fpath.(t.dir // job) >>= fun job_runs ->
  Ok { name = job; runs = job_runs }

let jobs t =
  Bos.OS.Dir.contents ~rel:true t.dir >>|
  List.filter (fun f -> not (Fpath.equal (Fpath.v "state") f)) >>|
  List.filter_map (fun job ->
      match job_runs t job with
      | Ok job -> Some job
      | Error (`Msg e) ->
        Log.warn (fun f -> f "Error reading job run dir %a: %s" Fpath.pp
                     Fpath.(t.dir // job) e);
        None)
