type error = [ Caqti_error.call_or_retrieve | `Not_found | `File_error of Fpath.t | `Msg of string ]

val pp_error : Format.formatter -> error -> unit

val staging : Fpath.t -> Fpath.t

val cleanup_staging : Fpath.t -> Caqti_lwt.connection ->
  (unit, [> error ]) result Lwt.t

val build_artifact : Uuidm.t -> Fpath.t -> Caqti_lwt.connection ->
  (string * Cstruct.t, [> error ]) result Lwt.t

val build_artifacts : Builder_db.id -> Caqti_lwt.connection ->
  (Builder_db.file list, [> error ]) result Lwt.t

val build : Uuidm.t -> Caqti_lwt.connection ->
  (Builder_db.id * Builder_db.Build.t, [> error ]) result Lwt.t

val build_meta : Builder_db.id -> Caqti_lwt.connection ->
  ((Builder_db.Build.Meta.t * Builder_db.file option) option, [> error ]) result Lwt.t

val build_hash : Cstruct.t -> Caqti_lwt.connection ->
  ((string * Builder_db.Build.t) option, [> error ]) result Lwt.t

val build_exists : Uuidm.t -> Caqti_lwt.connection ->
  (bool, [> error ]) result Lwt.t

val main_binary : Builder_db.id -> Fpath.t option -> Caqti_lwt.connection ->
  (Builder_db.file option, [> error ]) result Lwt.t

val job : string -> Caqti_lwt.connection ->
  ((Builder_db.Build.Meta.t * Builder_db.file option) list, [> error ]) result Lwt.t

val jobs : Caqti_lwt.connection ->
  ((Builder_db.id * string) list, [> error ]) result Lwt.t

val user : string -> Caqti_lwt.connection ->
  (Builder_web_auth.scrypt Builder_web_auth.user_info option, [> error ]) result Lwt.t


val add_build :
  Fpath.t ->
  (Builder.job * Uuidm.t * (int * string) list * Ptime.t * Ptime.t *
   Builder.execution_result * (Fpath.t * string) list) ->
  Caqti_lwt.connection ->
  (unit, [> error ]) result Lwt.t
