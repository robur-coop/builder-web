module Rep : sig
  type untyped_id
  type 'a id
  type file = {
    filepath : Fpath.t;
    localpath : Fpath.t;
    sha256 : Cstruct.t;
    size : int;
  }

  val untyped_id : untyped_id Caqti_type.t
  val id : 'a -> 'a id Caqti_type.t
  val uuid : Uuidm.t Caqti_type.t
  val ptime : Ptime.t Caqti_type.t
  val fpath : Fpath.t Caqti_type.t
  val cstruct : Cstruct.t Caqti_type.t
  val file : file Caqti_type.t
  val execution_result : Builder.execution_result Caqti_type.t
  val console : (int * string) list Caqti_type.t
end
type 'a id = 'a Rep.id

type file = Rep.file = {
  filepath : Fpath.t;
  localpath : Fpath.t;
  sha256 : Cstruct.t;
  size : int;
}

val application_id : int32

val current_version : int64

val get_application_id :
  (unit, int32, [< `Many | `One | `Zero > `One ]) Caqti_request.t

val set_application_id :
  (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t

val get_version :
  (unit, int64, [< `Many | `One | `Zero > `One ]) Caqti_request.t

val set_current_version :
  (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t

val last_insert_rowid :
  (unit, 'a id, [< `Many | `One | `Zero > `One ]) Caqti_request.t

module Job : sig
  val migrate :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val rollback :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t

  val get :
    ([`job] id, string, [< `Many | `One | `Zero > `One ])
      Caqti_request.t
  val get_id_by_name :
    (string, [`job] id, [< `Many | `One | `Zero > `One `Zero ]) Caqti_request.t
  val get_all :
    (unit, [`job] id * string, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_all_with_section_synopsis :
    (unit, [`job] id * string * string option * string option, [ `Many | `One | `Zero ]) Caqti_request.t
  val try_add :
    (string, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val remove :
    ([`job] id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
end

module Tag : sig
  val migrate :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val rollback :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val get :
    ([`tag] id, string, [< `Many | `One | `Zero > `One ]) Caqti_request.t
  val get_id_by_name :
    (string, [`tag] id, [< `Many | `One | `Zero > `One ]) Caqti_request.t
  val try_add :
    (string, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
end

module Job_tag : sig
  val migrate :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val rollback :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val add :
    ([`tag] id * string * [`job] id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val update :
    ([`tag] id * string * [`job] id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val get_value :
    ([`tag] id * [`job] id, string, [< `Many | `One | `Zero > `Zero `One ]) Caqti_request.t
end

module Build_artifact : sig
  val migrate :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val rollback :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t

  val get : ([`build_artifact] id, file, [< `Many | `One | `Zero > `One]) Caqti_request.t
  val get_by_build :
    ([`build] id * Fpath.t, [`build_artifact] id * file,
     [< `Many | `One | `Zero > `One ]) Caqti_request.t

  val get_by_build_uuid :
    (Uuidm.t * Fpath.t, [`build_artifact] id * file,
     [< `Many | `One | `Zero > `One `Zero ])
      Caqti_request.t
  val get_all_by_build :
    ([`build] id, [`build_artifact] id * file, [ `Many | `One | `Zero ]) Caqti_request.t
  val add :
    (file * [`build] id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val remove_by_build :
    ([`build] id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val remove :
    ([`build_artifact] id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
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
    input_id : Cstruct.t option;
    user_id : [`user] id;
    job_id : [`job] id;
  }

  val migrate :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val rollback :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t

  val get_opt :
    ([`build] id, t, [< `Many | `One | `Zero > `One `Zero ]) Caqti_request.t
  val get_by_uuid :
    (Uuidm.t, [`build] id * t, [< `Many | `One | `Zero > `One `Zero ])
      Caqti_request.t
  val get_all :
    ([`job] id, [`build] id * t, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_all_with_main_binary :
    ([`job] id, [`build] id * t * file option, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_all_artifact_sha :
    ([`job] id, Cstruct.t, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_latest :
    ([`job] id, [`build] id * t * file option, [< `Many | `One | `Zero > `One `Zero ])
      Caqti_request.t
  val get_latest_failed :
    ([`job] id, t, [< `Many | `One | `Zero > `One `Zero ]) Caqti_request.t
  val get_latest_uuid :
    ([`job] id, [`build] id * Uuidm.t, [< `Many | `One | `Zero > `One `Zero ])
      Caqti_request.t
  val get_latest_successful_uuid :
    ([`job] id, Uuidm.t, [< `Many | `One | `Zero > `One `Zero ])
      Caqti_request.t
  val get_previous_successful :
    ([`build] id, [`build] id * t, [< `Many | `One | `Zero > `One `Zero ])
      Caqti_request.t
  val get_same_input_same_output_builds :
    ([`build] id, t, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_same_input_different_output_hashes :
    ([`build] id, Cstruct.t, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_different_input_same_output_input_ids :
    ([`build] id, Cstruct.t, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_one_by_input_id :
    (Cstruct.t, t, [< `Many | `One | `Zero > `One ]) Caqti_request.t
  val add : (t, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val get_by_hash :
    (Cstruct.t, t, [< `Many | `One | `Zero > `One]) Caqti_request.t
  val get_with_main_binary_by_hash :
    (Cstruct.t, t * file option, [< `Many | `One | `Zero > `One]) Caqti_request.t
  val get_with_jobname_by_hash :
    (Cstruct.t, string * t, [< `Many | `One | `Zero > `One `Zero]) Caqti_request.t
  val set_main_binary : ([`build] id * [`build_artifact] id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val remove : ([`build] id, unit, [< `Many | `One | `Zero > `Zero]) Caqti_request.t
end

module User : sig
  val migrate :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val rollback :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val get_user :
    (string, [`user] id * Builder_web_auth.scrypt Builder_web_auth.user_info,
     [< `Many | `One | `Zero > `One `Zero ])
    Caqti_request.t
  val get_all :
    (unit, string, [ `Many | `One | `Zero ]) Caqti_request.t
  val add :
    (Builder_web_auth.scrypt Builder_web_auth.user_info, unit, [< `Many | `One | `Zero > `Zero ])
    Caqti_request.t
  val remove : ([`user] id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val remove_user :
    (string, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val update_user :
    (Builder_web_auth.scrypt Builder_web_auth.user_info, unit, [< `Many | `One | `Zero > `Zero ])
    Caqti_request.t
end

module Access_list : sig
  val migrate :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val rollback :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val get :
    ([`user] id * [`job] id, [`access_list] id, [< `Many | `One | `Zero > `One ]) Caqti_request.t
  val add :
    ([`user] id * [`job] id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val remove :
    ([`user] id * [`job] id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val remove_all_by_username :
    (string, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
end

val migrate :
  (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t list
val rollback :
  (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t list
