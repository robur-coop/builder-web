module Rep : sig
  type untyped_id
  type 'a id
  type file = {
    filepath : Fpath.t;
    sha256 : string;
    size : int;
  }

  val id_to_int64 : 'a id -> int64
  val untyped_id : untyped_id Caqti_type.t
  val id : 'a -> 'a id Caqti_type.t
  val uuid : Uuidm.t Caqti_type.t
  val ptime : Ptime.t Caqti_type.t
  val fpath : Fpath.t Caqti_type.t
  val file : file Caqti_type.t
  val execution_result : Builder.execution_result Caqti_type.t
  val console : (int * string) list Caqti_type.t
end
type 'a id = 'a Rep.id

type file = Rep.file = {
  filepath : Fpath.t;
  sha256 : string;
  size : int;
}

val application_id : int32

val current_version : int64

val get_application_id :
  (unit, int32, [ `One ]) Caqti_request.t

val set_application_id :
  (unit, unit, [ `Zero ]) Caqti_request.t

val get_version :
  (unit, int64, [ `One ]) Caqti_request.t

val set_current_version :
  (unit, unit, [ `Zero ]) Caqti_request.t

val last_insert_rowid :
  (unit, 'a id, [ `One ]) Caqti_request.t

module Job : sig
  val get :
    ([`job] id, string, [ `One ])
      Caqti_request.t
  val get_id_by_name :
    (string, [`job] id, [ `One | `Zero ]) Caqti_request.t
  val get_all_with_section_synopsis :
    (unit, [`job] id * string * string option * string option, [ `Many | `One | `Zero ]) Caqti_request.t
  val try_add :
    (string, unit, [ `Zero ]) Caqti_request.t
  val remove :
    ([`job] id, unit, [ `Zero ]) Caqti_request.t
end

module Tag : sig
  val get_id_by_name :
    (string, [`tag] id, [ `One ]) Caqti_request.t
  val try_add :
    (string, unit, [ `Zero ]) Caqti_request.t
end

module Job_tag : sig
  val add :
    ([`tag] id * string * [`job] id, unit, [ `Zero ]) Caqti_request.t
  val update :
    ([`tag] id * string * [`job] id, unit, [ `Zero ]) Caqti_request.t
  val get_value :
    ([`tag] id * [`job] id, string, [ `One | `Zero ]) Caqti_request.t
  val remove_by_job :
    ([`job] id, unit, [ `Zero ]) Caqti_request.t
end

module Build_artifact : sig
  val get : ([`build_artifact] id, file, [ `One]) Caqti_request.t
  val get_by_build_uuid :
    (Uuidm.t * Fpath.t, [`build_artifact] id * file,
     [ `One | `Zero ])
      Caqti_request.t
  val get_all_by_build :
    ([`build] id, [`build_artifact] id * file, [ `Many | `One | `Zero ]) Caqti_request.t
  val exists : (string, bool, [ `One ]) Caqti_request.t
  val add :
    (file * [`build] id, unit, [ `Zero ]) Caqti_request.t
  val remove_by_build :
    ([`build] id, unit, [ `Zero ]) Caqti_request.t
  val remove :
    ([`build_artifact] id, unit, [ `Zero ]) Caqti_request.t
end

module Build :
sig
  type t = {
    uuid : Uuidm.t;
    start : Ptime.t;
    finish : Ptime.t;
    result : Builder.execution_result;
    console : Fpath.t;
    script : Fpath.t;
    platform : string;
    main_binary : [`build_artifact] id option;
    input_id : string option;
    user_id : [`user] id;
    job_id : [`job] id;
  }

  val pp : t Fmt.t

  val get_by_uuid :
    (Uuidm.t, [`build] id * t, [ `One | `Zero ])
      Caqti_request.t
  val get_all :
    ([`job] id, [`build] id * t, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_all_failed :
    (int * int * string option, string * t, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_all_artifact_sha :
    ([`job] id * string option, string, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_latest_successful_with_binary :
    ([`job] id * string, [`build] id * t * file, [ `One | `Zero ])
      Caqti_request.t
  val get_failed_builds :
    ([`job] id * string option, t, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_latest_successful :
    ([`job] id * string option, t, [ `One | `Zero ])
      Caqti_request.t
  val get_builds_older_than :
    ([`job] id * string option * Ptime.t, [`build] id * t, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_builds_excluding_latest_n :
    ([`job] id * string option * int, [`build] id * t, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_nth_latest_successful :
    ([`job] id * string option * int, [`build] id * t, [ `One | `Zero ]) Caqti_request.t
  val get_previous_successful_different_output :
    ([`build] id, t, [ `One | `Zero ])
      Caqti_request.t
  val get_next_successful_different_output :
    ([`build] id, t, [ `One | `Zero ])
      Caqti_request.t
  val get_same_input_same_output_builds :
    ([`build] id, t, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_same_input_different_output_hashes :
    ([`build] id, string, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_different_input_same_output_input_ids :
    ([`build] id, string, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_one_by_input_id :
    (string, t, [ `One ]) Caqti_request.t
  val get_platforms_for_job :
    ([`job] id, string, [ `Many | `One | `Zero ]) Caqti_request.t
  val add : (t, unit, [ `Zero ]) Caqti_request.t
  val get_by_hash :
    (string, t, [ `One]) Caqti_request.t
  val get_with_main_binary_by_hash :
    (string, t * file option, [ `One]) Caqti_request.t
  val get_with_jobname_by_hash :
    (string, string * t, [ `One | `Zero]) Caqti_request.t
  val set_main_binary : ([`build] id * [`build_artifact] id, unit, [ `Zero ]) Caqti_request.t
  val remove : ([`build] id, unit, [ `Zero]) Caqti_request.t
end

module User : sig
  val get_user :
    (string, [`user] id * Builder_web_auth.scrypt Builder_web_auth.user_info,
     [ `One | `Zero ])
    Caqti_request.t
  val get_all :
    (unit, string, [ `Many | `One | `Zero ]) Caqti_request.t
  val add :
    (Builder_web_auth.scrypt Builder_web_auth.user_info, unit, [ `Zero ])
    Caqti_request.t
  val remove_user :
    (string, unit, [ `Zero ]) Caqti_request.t
  val update_user :
    (Builder_web_auth.scrypt Builder_web_auth.user_info, unit, [ `Zero ])
    Caqti_request.t
end

module Access_list : sig
  val get :
    ([`user] id * [`job] id, [`access_list] id, [ `One ]) Caqti_request.t
  val add :
    ([`user] id * [`job] id, unit, [ `Zero ]) Caqti_request.t
  val remove :
    ([`user] id * [`job] id, unit, [ `Zero ]) Caqti_request.t
  val remove_by_job :
    ([`job] id, unit, [ `Zero ]) Caqti_request.t
  val remove_all_by_username :
    (string, unit, [ `Zero ]) Caqti_request.t
end

val migrate :
  (unit, unit, [ `Zero ]) Caqti_request.t list
val rollback :
  (unit, unit, [ `Zero ]) Caqti_request.t list
