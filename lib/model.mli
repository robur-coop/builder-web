type error = [ Caqti_error.call_or_retrieve | `Not_found | `File_error of Fpath.t ]

val pp_error : Format.formatter -> error -> unit

val build_artifact : Uuidm.t -> Fpath.t -> Caqti_lwt.connection ->
  (string * Cstruct.t, [> error ]) result Lwt.t

val build_artifacts : Builder_db.id -> Caqti_lwt.connection ->
  (Builder_db.file list, [> error ]) result Lwt.t

val build : Uuidm.t -> Caqti_lwt.connection ->
  (Builder_db.id * Builder_db.Build.t, [> error ]) result Lwt.t

val job : string -> Caqti_lwt.connection ->
  ((Builder_db.id * Builder_db.Build.Meta.t) list, [> error ]) result Lwt.t

val jobs : Caqti_lwt.connection ->
  (string list, [> error ]) result Lwt.t
