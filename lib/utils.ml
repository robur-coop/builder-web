
module String_map = struct
  include Map.Make(String)

  let add_or_create key v t=
    update key (function None -> Some [ v ] | Some xs -> Some (v :: xs)) t
end

