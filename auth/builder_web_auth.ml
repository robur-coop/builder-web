let prf : Mirage_crypto.Hash.hash = `SHA256
let default_count = 160_000
let dk_len = 32l

type user_info = {
  username : string;
  password_hash : Cstruct.t;
  password_salt : Cstruct.t;
  password_iter : int;
}

module SMap = Map.Make(String)

type t = user_info SMap.t

let user_info_to_sexp { username; password_hash; password_salt; password_iter } =
  Sexplib.Sexp.(List [
      Atom "user_info";
      Atom username;
      Atom (Cstruct.to_string password_hash);
      Atom (Cstruct.to_string password_salt);
      Sexplib.Conv.sexp_of_int password_iter;
    ])

let user_info_of_sexp =
  let open Sexplib.Sexp in
  function
  | List [ Atom "user_info";
           Atom username;
           Atom password_hash;
           Atom password_salt;
           (Atom _ ) as password_iter; ] ->
    { username;
      password_hash = Cstruct.of_string password_hash;
      password_salt = Cstruct.of_string password_salt;
      password_iter = Sexplib.Conv.int_of_sexp password_iter; }
  | sexp ->
    Sexplib.Conv.of_sexp_error "Auth_store.user_info_of_sexp: bad sexp" sexp

let h count salt password =
  Pbkdf.pbkdf2 ~prf ~count ~dk_len ~salt ~password:(Cstruct.of_string password)

let hash ~username ~password =
  let salt = Mirage_crypto_rng.generate 16 in
  let password_iter = default_count in
  let password_hash = h password_iter salt password in
  { username; password_hash; password_salt = salt; password_iter }

let verify_password password user_info =
  Cstruct.equal
    (h user_info.password_iter user_info.password_salt password)
    user_info.password_hash
