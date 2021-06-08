type pbkdf2_sha256_params = {
  pbkdf2_sha256_iter : int;
}

type scrypt_params = {
  scrypt_n : int;
  scrypt_r : int;
  scrypt_p : int;
}

let scrypt_params ?(scrypt_n = 16384) ?(scrypt_r = 8) ?(scrypt_p = 1) () =
  { scrypt_n; scrypt_r; scrypt_p }

type pbkdf2_sha256 =
  [ `Pbkdf2_sha256 of Cstruct.t * Cstruct.t * pbkdf2_sha256_params ]

type scrypt = [ `Scrypt of Cstruct.t * Cstruct.t * scrypt_params ]

type password_hash = [ pbkdf2_sha256 | scrypt ]

type 'a user_info = {
  username : string;
  password_hash : [< password_hash ] as 'a;
  restricted : bool;
}

let pbkdf2_sha256 ~params:{ pbkdf2_sha256_iter = count } ~salt ~password =
  Pbkdf.pbkdf2 ~prf:`SHA256 ~count ~dk_len:32l ~salt ~password:(Cstruct.of_string password)

let scrypt ~params:{ scrypt_n = n; scrypt_r = r; scrypt_p = p } ~salt ~password =
  Scrypt_kdf.scrypt_kdf ~n ~r ~p ~dk_len:32l ~salt ~password:(Cstruct.of_string password)

let hash ?(scrypt_params=scrypt_params ())
    ~username ~password ~restricted () =
  let salt = Mirage_crypto_rng.generate 16 in
  let password_hash = scrypt ~params:scrypt_params ~salt ~password in
  {
    username;
    password_hash = `Scrypt (password_hash, salt, scrypt_params);
    restricted;
  }

let verify_password password user_info =
  match user_info.password_hash with
  | `Pbkdf2_sha256 (password_hash, salt, params) ->
    Cstruct.equal
      (pbkdf2_sha256 ~params ~salt ~password)
      password_hash
  | `Scrypt (password_hash, salt, params) ->
    Cstruct.equal
      (scrypt ~params ~salt ~password)
      password_hash
