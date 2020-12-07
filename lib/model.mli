type t

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

type job = {
  path : Fpath.t;
  runs : job_run_meta list;
}

val init : Fpath.t -> t

val job_name : job -> string

val read_full : t -> Fpath.t -> Fpath.t -> (job_run_info, [> `Msg of string ]) result

val job : t -> Fpath.t -> (job, [> `Msg of string]) result
val jobs : t -> (job list, [> `Msg of string ]) result
