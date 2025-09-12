val pp_error : [< `Not_found | `File_error of Fpath.t | `Msg of string | Caqti_error.t ] Fmt.t

val user : string -> Caqti_miou.connection ->
  (([`user] Builder_db.id * Builder_web_auth.scrypt Builder_web_auth.user_info) option,
   [> Caqti_error.call_or_retrieve ]) result

val build_with_main_binary : [`job] Builder_db.id -> string -> Caqti_miou.connection ->
  ((Builder_db.Build.t * Builder_db.file) option,
   [> Caqti_error.call_or_retrieve ]) result

val platforms_of_job : [ `job ] Builder_db.id -> Caqti_miou.connection ->
  (string list, [> Caqti_error.call_or_retrieve ]) result

val jobs_with_section_synopsis : Caqti_miou.connection ->
  (([ `job ] Builder_db.id * string * string option * string option) list,
   [> Caqti_error.call_or_retrieve ]) result

val failed_builds : start:int -> count: int -> string option -> Caqti_miou.connection ->
  ((string * Builder_db.Build.t) list,
   [> Caqti_error.call_or_retrieve ]) result

val job_and_readme : string -> Caqti_miou.connection ->
  (([`job] Builder_db.id * string option),
   [> Caqti_error.call_or_retrieve | `Not_found ]) result

val builds_grouped_by_output : [`job] Builder_db.id -> string option -> Caqti_miou.connection ->
  ((Builder_db.Build.t * Builder_db.file option) list,
   [> Caqti_error.call_or_retrieve ]) result

val builds_grouped_by_output_with_failed : [`job] Builder_db.id -> string option ->
  Caqti_miou.connection ->
  ((Builder_db.Build.t * Builder_db.file option) list,
   [> Caqti_error.call_or_retrieve ]) result

val job_id : string -> Caqti_miou.connection ->
  ([`job] Builder_db.id option, [> Caqti_error.call_or_retrieve ]) result

val not_found : 'a option -> ('a, [> `Not_found ]) result

val latest_successful_build_uuid : [`job] Builder_db.id -> string option ->
  Caqti_miou.connection ->
  (Uuidm.t option, [> Caqti_error.call_or_retrieve ]) result

val build : Uuidm.t -> Caqti_miou.connection ->
  ([`build] Builder_db.id * Builder_db.Build.t,
   [> Caqti_error.call_or_retrieve | `Not_found ]) result

val build_artifact_by_id : [`build_artifact] Builder_db.id -> Caqti_miou.connection ->
  (Builder_db.file, [> Caqti_error.call_or_retrieve ]) result

val build_artifacts : [`build] Builder_db.id -> Caqti_miou.connection ->
  (Builder_db.file list, [> Caqti_error.call_or_retrieve ]) result

val builds_with_same_input_and_same_main_binary : [`build] Builder_db.id ->
  Caqti_miou.connection ->
  (Builder_db.Build.t list, [> Caqti_error.call_or_retrieve ]) result

val builds_with_different_input_and_same_main_binary : [`build] Builder_db.id ->
  Caqti_miou.connection ->
  (Builder_db.Build.t list, [> Caqti_error.call_or_retrieve ]) result

val builds_with_same_input_and_different_main_binary : [`build] Builder_db.id ->
  Caqti_miou.connection ->
  (Builder_db.Build.t list, [> Caqti_error.call_or_retrieve ]) result

val latest_successful_build : [`job] Builder_db.id -> string option ->
  Caqti_miou.connection ->
  (Builder_db.Build.t option, [> Caqti_error.call_or_retrieve ]) result

val next_successful_build_different_output : [`build] Builder_db.id ->
  Caqti_miou.connection ->
  (Builder_db.Build.t option, [> Caqti_error.call_or_retrieve ]) result

val previous_successful_build_different_output : [`build] Builder_db.id ->
  Caqti_miou.connection ->
  (Builder_db.Build.t option, [> Caqti_error.call_or_retrieve ]) result

val solo5_manifest : Fpath.t -> Builder_db.file ->
  Solo5_elftool.mft option

val build_artifact : Uuidm.t -> Fpath.t -> Caqti_miou.connection ->
  (Builder_db.file, [> Caqti_error.call_or_retrieve | `Not_found ]) result

val artifact_path : Builder_db.file -> Fpath.t

val build_console_by_uuid : Fpath.t -> Uuidm.t -> Caqti_miou.connection ->
  (Fpath.t, [> Caqti_error.call_or_retrieve | `Not_found ]) result

val build_script_by_uuid : Fpath.t -> Uuidm.t -> Caqti_miou.connection ->
  (Fpath.t, [> Caqti_error.call_or_retrieve | `Not_found ]) result

module Viz : sig
  val try_load_cached_visualization : datadir:Fpath.t -> uuid:Uuidm.t ->
    [< `Dependencies | `Treemap ] -> Caqti_miou.connection ->
    (Fpath.t, [> Caqti_error.call_or_retrieve | `Not_found | `Msg of string ]) result
end

val build_artifact_data : Fpath.t -> Uuidm.t -> Fpath.t -> Caqti_miou.connection ->
  (string, [> Caqti_error.call_or_retrieve | `Not_found | `File_error of Fpath.t ]) result

val exec_of_build : Fpath.t -> Uuidm.t -> Caqti_miou.connection ->
  (string, [> Caqti_error.call_or_retrieve | `Not_found | `File_error of Fpath.t ]) result

val job_name : [`job] Builder_db.id -> Caqti_miou.connection ->
  (string, [> Caqti_error.call_or_retrieve ]) result

val build_hash : string -> Caqti_miou.connection ->
  ((string * Builder_db.Build.t) option, [> Caqti_error.call_or_retrieve ]) result

val authorized : [`user] Builder_db.id -> Builder_web_auth.scrypt Builder_web_auth.user_info ->
  string -> Caqti_miou.connection ->
  (bool, [> Caqti_error.call_or_retrieve ]) result

val build_exists : Uuidm.t -> Caqti_miou.connection ->
  (bool, [> Caqti_error.call_or_retrieve ]) result

val add_build : datadir:Fpath.t -> configdir:Fpath.t -> [`user] Builder_db.id ->
  (Builder.script_job * Uuidm.t * (int * string) list * Ptime.t * Ptime.t *
    Builder.execution_result * Builder.data) ->
  Caqti_miou.connection ->
  (unit,
   [> Caqti_error.call_or_retrieve | `Not_found | `File_error of Fpath.t | `Msg of string ]) result
