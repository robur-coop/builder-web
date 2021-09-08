type error = [ Caqti_error.call_or_retrieve | `Not_found | `File_error of Fpath.t | `Msg of string ]

val pp_error : Format.formatter -> error -> unit

val not_found : 'a option -> ('a, [> error ]) result Lwt.t

val staging : Fpath.t -> Fpath.t

val cleanup_staging : Fpath.t -> Caqti_lwt.connection ->
  (unit, [> `Msg of string ]) result Lwt.t

val build_artifact : Uuidm.t -> Fpath.t -> Caqti_lwt.connection ->
  (Builder_db.file, [> error ]) result Lwt.t

val build_artifact_by_id : [`build_artifact] Builder_db.id -> Caqti_lwt.connection ->
  (Builder_db.file, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val build_artifact_data : Fpath.t -> Builder_db.file ->
  (string, [> error ]) result Lwt.t

val build_artifacts : [`build] Builder_db.id -> Caqti_lwt.connection ->
  (Builder_db.file list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val build : Uuidm.t -> Caqti_lwt.connection ->
  ([`build] Builder_db.id * Builder_db.Build.t, [> error ]) result Lwt.t

val build_meta : [`job] Builder_db.id -> Caqti_lwt.connection ->
  ((Builder_db.Build.t * Builder_db.file option) option, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val build_hash : Cstruct.t -> Caqti_lwt.connection ->
  ((string * Builder_db.Build.t) option, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val build_exists : Uuidm.t -> Caqti_lwt.connection ->
  (bool, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val latest_build_uuid : [`job] Builder_db.id -> Caqti_lwt.connection ->
  (Uuidm.t, [> error ]) result Lwt.t

val latest_successful_build_uuid : [`job] Builder_db.id -> Caqti_lwt.connection ->
  (Uuidm.t option, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val previous_successful_build : [`build] Builder_db.id -> Caqti_lwt.connection ->
  (Builder_db.Build.t option, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val builds_with_different_input_and_same_main_binary : [`build] Builder_db.id -> Caqti_lwt.connection ->
  (Builder_db.Build.t list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val builds_with_same_input_and_same_main_binary : [`build] Builder_db.id -> Caqti_lwt.connection ->
  (Builder_db.Build.t list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val builds_with_same_input_and_different_main_binary : [`build] Builder_db.id -> Caqti_lwt.connection ->
  (Builder_db.Build.t list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val readme : string -> Caqti_lwt.connection ->
  (string option, [> error ]) result Lwt.t

val job_and_readme : string -> Caqti_lwt.connection ->
  (string option * (Builder_db.Build.t * Builder_db.file option) list, [> error ]) result Lwt.t

val job_id : string -> Caqti_lwt.connection ->
  ([`job] Builder_db.id option, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val jobs : Caqti_lwt.connection ->
  (([`job] Builder_db.id * string) list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val jobs_with_section_synopsis : Caqti_lwt.connection ->
  (([`job] Builder_db.id * string * string option * string option) list, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val job_name : [`job] Builder_db.id -> Caqti_lwt.connection ->
  (string, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val user : string -> Caqti_lwt.connection ->
  (([`user] Builder_db.id * Builder_web_auth.scrypt Builder_web_auth.user_info) option, [> Caqti_error.call_or_retrieve ]) result Lwt.t

val authorized : [`user] Builder_db.id -> string -> Caqti_lwt.connection -> (unit, [> Caqti_error.call_or_retrieve | `Msg of string ]) result Lwt.t

val add_build :
  Fpath.t ->
  [`user] Builder_db.id ->
  (Builder.script_job * Uuidm.t * (int * string) list * Ptime.t * Ptime.t *
   Builder.execution_result * (Fpath.t * string) list) ->
  Caqti_lwt.connection ->
  (unit, [> Caqti_error.call_or_retrieve | `Msg of string ]) result Lwt.t
