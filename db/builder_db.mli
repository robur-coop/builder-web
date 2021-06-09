module Rep : sig
  type id
  type file = {
    filepath : Fpath.t;
    localpath : Fpath.t;
    sha256 : Cstruct.t;
    size : int;
  }

  val id : id Caqti_type.t
  val uuid : Uuidm.t Caqti_type.t
  val ptime : Ptime.t Caqti_type.t
  val fpath : Fpath.t Caqti_type.t
  val cstruct : Cstruct.t Caqti_type.t
  val file : file Caqti_type.t
  val execution_result : Builder.execution_result Caqti_type.t
  val console : (int * string) list Caqti_type.t
end
type id = Rep.id

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
  (unit, id, [< `Many | `One | `Zero > `One ]) Caqti_request.t

module Job : sig
  val migrate :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val rollback :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t

  val get :
    (id, string, [< `Many | `One | `Zero > `One ])
      Caqti_request.t
  val get_id_by_name :
    (string, id, [< `Many | `One | `Zero > `One `Zero ]) Caqti_request.t
  val get_all :
    (unit, id * string, [ `Many | `One | `Zero ]) Caqti_request.t
  val try_add :
    (string, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val remove :
    (id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
end

module Build_artifact : sig
  val migrate :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val rollback :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t

  val get_by_build :
    (id * Fpath.t, id * file,
     [< `Many | `One | `Zero > `One ]) Caqti_request.t

  val get_by_build_uuid :
    (Uuidm.t * Fpath.t, id * file,
     [< `Many | `One | `Zero > `One `Zero ])
      Caqti_request.t
  val get_all_by_build :
    (id, id * file, [ `Many | `One | `Zero ]) Caqti_request.t
  val add :
    (file * id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val remove_by_build :
    (id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
end

module Build_file : sig
  val migrate :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val rollback :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t

  val get_by_build_uuid :
    (Uuidm.t * Fpath.t, id * file,
     [< `Many | `One | `Zero > `One `Zero ])
      Caqti_request.t
  val get_all_by_build :
    (id, id * file, [ `Many | `One | `Zero ]) Caqti_request.t
  val add :
    (file * id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val remove_by_build :
    (id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
end

module Build :
sig
  type t = {
    uuid : Uuidm.t;
    start : Ptime.t;
    finish : Ptime.t;
    result : Builder.execution_result;
    console : (int * string) list;
    script : string;
    main_binary : id option;
    job_id : id;
  }
  module Meta :
  sig
    type t = {
      uuid : Uuidm.t;
      start : Ptime.t;
      finish : Ptime.t;
      result : Builder.execution_result;
      main_binary : id option;
      job_id : id;
    }
  end

  val migrate :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val rollback :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t

  val get_opt :
    (id, t, [< `Many | `One | `Zero > `One `Zero ]) Caqti_request.t
  val get_by_uuid :
    (Uuidm.t, id * t, [< `Many | `One | `Zero > `One `Zero ])
      Caqti_request.t
  val get_all :
    (id, id * t, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_all_meta :
    (id, id * Meta.t * file option, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_latest :
    (id, id * Meta.t * file option, [< `Many | `One | `Zero > `One `Zero ])
      Caqti_request.t
  val get_latest_uuid :
    (id, id * Uuidm.t, [< `Many | `One | `Zero > `One `Zero ])
      Caqti_request.t
  val get_latest_successful_uuid :
    (id, Uuidm.t, [< `Many | `One | `Zero > `One `Zero ])
      Caqti_request.t
  val get_previous_successful :
    (id, id * Meta.t, [< `Many | `One | `Zero > `One `Zero ])
      Caqti_request.t
  val add : (t, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val get_by_hash :
    (Cstruct.t, string * t, [< `Many | `One | `Zero > `One `Zero]) Caqti_request.t
  val set_main_binary : (id * id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val remove : (id, unit, [< `Many | `One | `Zero > `Zero]) Caqti_request.t
end

module User : sig
  val migrate :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val rollback :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val get_user :
    (string, id * Builder_web_auth.scrypt Builder_web_auth.user_info,
     [< `Many | `One | `Zero > `One `Zero ])
    Caqti_request.t
  val get_all :
    (unit, string, [ `Many | `One | `Zero ]) Caqti_request.t
  val add :
    (Builder_web_auth.scrypt Builder_web_auth.user_info, unit, [< `Many | `One | `Zero > `Zero ])
    Caqti_request.t
  val remove : (id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
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
    (id * id, id, [< `Many | `One | `Zero > `One ]) Caqti_request.t
  val add :
    (id * id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val remove :
    (id * id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val remove_all_by_username :
    (string, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
end

val migrate :
  (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t list
val rollback :
  (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t list
