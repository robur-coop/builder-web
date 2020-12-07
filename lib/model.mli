type t = {
  dir : Fpath.t
}

type job = {
  path : Fpath.t;
  runs : Fpath.t list;
}


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

val read_full : t -> Fpath.t -> Fpath.t -> (job_run_info, [> `Msg of string ]) result

val job : t -> Fpath.t -> (job, [> `Msg of string]) result
val jobs : t -> (job list, [> `Msg of string ]) result
