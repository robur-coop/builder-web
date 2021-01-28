module Asn = struct
  let decode_strict codec cs =
    match Asn.decode codec cs with
    | Ok (a, cs) ->
      if Cstruct.len cs = 0
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

type id = int64
let id = Caqti_type.int64

type file = {
  filepath : Fpath.t;
  localpath : Fpath.t;
  sha256 : Cstruct.t;
}

let uuid =
  let encode uuid = Ok (Uuidm.to_bytes uuid) in
  let decode s =
    Uuidm.of_bytes s
    |> Option.to_result ~none:"failed to decode uuid"
  in
  Caqti_type.custom ~encode ~decode Caqti_type.string


let ptime =
  let encode t = Ok (Ptime.Span.to_d_ps (Ptime.to_span t)) in
  let decode (d, ps) = Ok (Ptime.v (d, ps))
  in
  let rep = Caqti_type.(tup2 int int64) in
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
  let encode { filepath; localpath; sha256 } =
    Ok (filepath, localpath, sha256) in
  let decode (filepath, localpath, sha256) =
    Ok { filepath; localpath; sha256 } in
  Caqti_type.custom ~encode ~decode Caqti_type.(tup3 fpath fpath cstruct)

let file_opt =
  let rep = Caqti_type.(tup3 (option fpath) (option fpath) (option cstruct)) in
  let encode = function
    | Some { filepath; localpath; sha256 } ->
      Ok (Some filepath, Some localpath, Some sha256)
    | None ->
      Ok (None, None, None)
  in
  let decode = function
    | (Some filepath, Some localpath, Some sha256) ->
      Ok (Some { filepath; localpath; sha256 })
    | (None, None, None) ->
      Ok None
    | _ ->
      (* This should not happen if the database is well-formed *)
      Error "Some but not all fields NULL"
  in
  Caqti_type.custom ~encode ~decode rep

let execution_result =
  let encode = function
    | Builder.Exited v -> Ok (0, Some v, None)
    | Builder.Signalled v -> Ok (1, Some v, None)
    | Builder.Stopped v -> Ok (2, Some v, None)
    | Builder.Msg msg -> Ok (3, None, Some msg)
  in
  let decode (kind, code, msg) =
    match kind, code, msg with
    | 0, Some v, None -> Ok (Builder.Exited v)
    | 1, Some v, None -> Ok (Builder.Signalled v)
    | 2, Some v, None -> Ok (Builder.Stopped v)
    | 3, None, Some msg -> Ok (Builder.Msg msg)
    | _ -> Error "bad encoding"
  in
  let rep = Caqti_type.(tup3 int (option int) (option string)) in
  Caqti_type.custom ~encode ~decode rep

let console =
  let encode console = Ok (Asn.console_to_cs console) in
  let decode data = Asn.console_of_cs data in
  Caqti_type.custom ~encode ~decode cstruct

let user_info =
  let rep = Caqti_type.(tup4 string cstruct cstruct int) in
  let encode { Builder_web_auth.username; password_hash;
               password_salt; password_iter } =
    Ok (username, password_hash, password_salt, password_iter)
  in
  let decode (username, password_hash, password_salt, password_iter) =
    Ok { Builder_web_auth.username; password_hash; password_salt; password_iter }
  in
  Caqti_type.custom ~encode ~decode rep
