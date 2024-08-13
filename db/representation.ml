module Asn = struct
  let decode_strict codec cs =
    match Asn.decode codec cs with
    | Ok (a, cs) ->
      if Cstruct.length cs = 0
      then Ok a
      else Error "trailing bytes"
    | Error (`Parse msg) -> Error ("parse error: " ^ msg)

  let projections_of asn =
    let c = Asn.codec Asn.der asn in
    (decode_strict c, Asn.encode c)

  let console =
    Asn.S.(sequence_of
             (sequence2
                (required ~label:"delta" int)
                (required ~label:"data" utf8_string)))

  let console_of_cs, console_to_cs = projections_of console
end

type untyped_id = int64
let untyped_id = Caqti_type.int64
type 'a id = untyped_id
let id (_ : 'a) : 'a id Caqti_type.t = untyped_id
let any_id : 'a id Caqti_type.t = untyped_id

let id_to_int64 (id : 'a id) : int64 = id

type file = {
  filepath : Fpath.t;
  sha256 : Cstruct.t;
  size : int;
}

let uuid =
  let encode uuid = Ok (Uuidm.to_string uuid) in
  let decode s =
    Uuidm.of_string s
    |> Option.to_result ~none:"failed to decode uuid"
  in
  Caqti_type.custom ~encode ~decode Caqti_type.string


let ptime =
  let encode t = Ok (Ptime.Span.to_d_ps (Ptime.to_span t)) in
  let decode (d, ps) = Ok (Ptime.v (d, ps))
  in
  let rep = Caqti_type.(t2 int int64) in
  Caqti_type.custom ~encode ~decode rep

let fpath =
  let encode t = Ok (Fpath.to_string t) in
  let decode s = Fpath.of_string s
                 |> Result.map_error (fun (`Msg s) -> s) in
  Caqti_type.custom ~encode ~decode Caqti_type.string

let cstruct =
  let encode t = Ok (Cstruct.to_string t) in
  let decode s = Ok (Cstruct.of_string s) in
  Caqti_type.custom ~encode ~decode Caqti_type.octets

let file =
  let encode { filepath; sha256; size } =
    Ok (filepath, sha256, size) in
  let decode (filepath, sha256, size) =
    Ok { filepath; sha256; size } in
  Caqti_type.custom ~encode ~decode Caqti_type.(t3 fpath cstruct int)

let file_opt =
  let rep = Caqti_type.(t3 (option fpath) (option cstruct) (option int)) in
  let encode = function
    | Some { filepath; sha256; size } ->
      Ok (Some filepath, Some sha256, Some size)
    | None ->
      Ok (None, None, None)
  in
  let decode = function
    | (Some filepath, Some sha256, Some size) ->
      Ok (Some { filepath; sha256; size })
    | (None, None, None) ->
      Ok None
    | _ ->
      (* This should not happen if the database is well-formed *)
      Error "Some but not all fields NULL"
  in
  Caqti_type.custom ~encode ~decode rep

let execution_result =
  let encode = function
    | Builder.Exited v -> Ok (v, None)
    | Builder.Signalled v -> Ok (v lsl 8, None)
    | Builder.Stopped v -> Ok (v lsl 16, None)
    | Builder.Msg msg -> Ok (65536, Some msg)
  in
  let decode (code, msg) =
    if code <= 0xFF then
       Ok (Builder.Exited code)
    else if code <= 0xFFFF then
       Ok (Builder.Signalled (code lsr 8))
    else if code <= 0xFFFFFF then
       Ok (Builder.Stopped (code lsr 16))
    else if code = 65536 then
       match msg with
       | None -> Error "bad encoding"
       | Some m -> Ok (Builder.Msg m)
    else
      Error "bad encoding (unknown number)"
  in
  let rep = Caqti_type.(t2 int (option string)) in
  Caqti_type.custom ~encode ~decode rep

let console =
  let encode console = Ok (Asn.console_to_cs console) in
  let decode data = Asn.console_of_cs data in
  Caqti_type.custom ~encode ~decode cstruct

let user_info =
  let rep = Caqti_type.(t7 string cstruct cstruct int int int bool) in
  let encode { Builder_web_auth.username;
               password_hash = `Scrypt (password_hash, password_salt, {
                   Builder_web_auth.scrypt_n; scrypt_r; scrypt_p
               });
               restricted; }
    =
    Ok (username, password_hash, password_salt, scrypt_n, scrypt_r, scrypt_p, restricted)
  in
  let decode (username, password_hash, password_salt, scrypt_n, scrypt_r, scrypt_p, restricted) =
    Ok { Builder_web_auth.username;
         password_hash =
           `Scrypt (password_hash, password_salt,
                    { Builder_web_auth.scrypt_n;
                      scrypt_r; scrypt_p });
         restricted; }  in
  Caqti_type.custom ~encode ~decode rep

(* this doesn't really belong in this module, but we need access to the type of [id] *)
let last_insert_rowid =
  let open Caqti_request.Infix in
  Caqti_type.unit ->! any_id @@
    "SELECT last_insert_rowid()"
