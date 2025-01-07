type opam_diff = {
  pkg : OpamPackage.t ;
  effectively_equal : bool ;
  diff : string ;
}

type version_diff = {
  name : OpamPackage.Name.t;
  version_left : OpamPackage.Version.t;
  version_right : OpamPackage.Version.t;
}

type duniverse_diff = {
  name : string ;
  urls : string * string option ;
  hash : (OpamHash.kind * string option * string option) list ;
}

val pp_opampackage : Format.formatter -> OpamPackage.t -> unit

val pp_version_diff : Format.formatter -> version_diff -> unit

val pp_duniverse_diff : Format.formatter -> duniverse_diff -> unit

val pp_duniverse_dir : Format.formatter -> string * string -> unit

val pp_opam_diff : Format.formatter -> opam_diff -> unit

val commands_to_strings : OpamTypes.command list * OpamTypes.command list -> string list * string list

val opt_url_to_string : OpamFile.URL.t option * OpamFile.URL.t option -> string * string


val compare : OpamFile.SwitchExport.t ->
  OpamFile.SwitchExport.t ->
  opam_diff list * version_diff list * OpamPackage.Set.t * OpamPackage.Set.t * ((string * string) list * (string * string) list * duniverse_diff list, [> `Msg of string ]) result


val compare_to_json : opam_diff list * version_diff list * OpamPackage.Set.t * OpamPackage.Set.t *
((string * string) list * (string * string) list * duniverse_diff list, [< `Msg of string ]) result -> Yojson.Basic.t
