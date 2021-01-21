type id

type file = {
  filepath : Fpath.t;
  localpath : Fpath.t;
  sha256 : Cstruct.t;
}
val file : file Caqti_type.t

val last_insert_rowid :
  (unit, id, [< `Many | `One | `Zero > `One ]) Caqti_request.t

module Job : sig
  val migrate :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val rollback :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t

  val get :
    (id, string, [< `Many | `One | `Zero > `One `Zero ])
      Caqti_request.t
  val get_id_by_name :
    (string, id, [< `Many | `One | `Zero > `One ]) Caqti_request.t
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

  val get_by_build_uuid :
    (Uuidm.t * Fpath.t, Fpath.t * Cstruct.t,
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
    (Uuidm.t * Fpath.t, Fpath.t * Cstruct.t,
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
    job_id : id;
  }
  module Meta :
  sig
    type t = {
      uuid : Uuidm.t;
      start : Ptime.t;
      finish : Ptime.t;
      result : Builder.execution_result;
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
    (id, id * Meta.t, [ `Many | `One | `Zero ]) Caqti_request.t
  val get_all_meta_by_name :
    (string, id * Meta.t, [ `Many | `One | `Zero ]) Caqti_request.t
  val add : (t, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
end

module User : sig
  val migrate :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val rollback :
    (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val get_user :
    (string, id * Builder_web_auth.user_info,
     [< `Many | `One | `Zero > `One `Zero ])
    Caqti_request.t
  val get_all :
    (unit, string, [ `Many | `One | `Zero ]) Caqti_request.t
  val add :
    (Builder_web_auth.user_info, unit, [< `Many | `One | `Zero > `Zero ])
    Caqti_request.t
  val remove : (id, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val remove_user :
    (string, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t
  val update_user :
    (Builder_web_auth.user_info, unit, [< `Many | `One | `Zero > `Zero ])
    Caqti_request.t
end

val migrate :
  (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t list
val rollback :
  (unit, unit, [< `Many | `One | `Zero > `Zero ]) Caqti_request.t list
