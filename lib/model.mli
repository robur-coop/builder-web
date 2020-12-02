type t = {
  dir : Fpath.t
}

type job

type job_run

type job_run_info = {
  job_info : Builder.job;
  uuid : Uuidm.t;
  out : (int * string) list;
  start : Ptime.t;
  finish : Ptime.t;
  result : Builder.execution_result;
  data : (Fpath.t * string) list
}

val job_name : job -> string
val job_run_uuid : job_run -> Uuidm.t

val read_full : t -> job -> job_run -> (job_run_info, [> `Msg of string ]) result

val jobs : t -> (job list, [> `Msg of string ]) result
