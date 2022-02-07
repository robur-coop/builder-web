
module String_map = struct
  include Map.Make(String)

  let add_or_create key v t=
    update key (function None -> Some [ v ] | Some xs -> Some (v :: xs)) t
end

let diff_map a b =
  let diff a b =
    String_map.fold (fun k v acc ->
      if not (String_map.mem k b) then (k, v) :: acc else acc)
      a [] |> List.rev
  in
  let added = diff b a
  and removed = diff a b
  and changed =
    String_map.fold (fun k v acc ->
      match String_map.find_opt k b with
      | None -> acc
      | Some v' -> if String.equal v v' then acc else (k, v, v') :: acc)
    a [] |> List.rev
  in
  (added, removed, changed)

let compare_env env1 env2 =
  let parse_env e =
    List.fold_left (fun m s ->
      match Astring.String.cut ~sep:"=" s with
      | Some (key, value) -> String_map.add key value m
      | None -> String_map.add s "" m)
    String_map.empty (Astring.String.cuts ~sep:"\n" e)
  in
  diff_map (parse_env env1) (parse_env env2)

let compare_pkgs p1 p2 =
  let parse_pkgs p =
    List.fold_left (fun m s ->
      match Astring.String.cut ~sep:"=" s with
      | Some (name, version) -> String_map.add name version m
      | None -> match Astring.String.cut ~sep:"-" s with
        | Some (name, version) -> String_map.add name version m
        | None -> String_map.add s "" m)
    String_map.empty (Astring.String.cuts ~sep:"\n" p)
  in
  diff_map (parse_pkgs p1) (parse_pkgs p2)

module Omd = struct

  let make_safe omd =
    let rec safe_block = function
      | Omd.Paragraph (attr, inline) ->
        safe_inline inline
        |> Option.map (fun inline -> Omd.Paragraph (attr, inline))
      | Omd.List (attr, typ, spacing, blocks) ->
        let blocks = List.filter_map (fun b ->
          let b = List.filter_map safe_block b in
          if b = [] then None else Some b)
          blocks
        in
        if blocks = [] then None else
          Some (Omd.List (attr, typ, spacing, blocks))
      | Omd.Blockquote (attr, blocks) ->
        let blocks = List.filter_map safe_block blocks in
        if blocks = [] then None else
          Some (Omd.Blockquote (attr, blocks))
      | Omd.Heading (attr, level, inline) ->
        safe_inline inline
        |> Option.map (fun inline -> Omd.Heading (attr, level, inline))
      | Omd.Html_block _ -> None
      | Omd.Definition_list (attr, def_elts) ->
        let def_elts = List.filter_map safe_def_elts def_elts in
        if def_elts = [] then None else
          Some (Omd.Definition_list (attr, def_elts))
      | Omd.Code_block _
      | Omd.Thematic_break _ as v -> Some v
    and safe_def_elts { term ; defs } =
      let defs = List.filter_map safe_inline defs in
      safe_inline term
      |> Option.map (fun term -> { Omd.term ; defs })
    and safe_inline = function
      | Concat (attr, inline) ->
        Some (Concat (attr, List.filter_map safe_inline inline))
      | Emph (attr, inline) ->
        safe_inline inline
        |> Option.map (fun inline -> Omd.Emph (attr, inline))
      | Strong (attr, inline) ->
        safe_inline inline
        |> Option.map (fun inline -> Omd.Strong (attr, inline))
      | Link (attr, link) ->
        begin match safe_link link with
          | `No_label | `Relative -> safe_inline link.Omd.label
          | `Link l -> Some (Omd.Link (attr, l))
        end
      | Image (attr, link) ->
        begin match safe_link link with
          | `No_label | `Relative -> None
          | `Link l -> Some (Omd.Image (attr, l))
        end
      | Html _ -> None
      | Text _
      | Code _
      | Hard_break _
      | Soft_break _ as v -> Some v
    and safe_link ({ label ; destination ; _ } as l) =
      let absolute_link =
        String.(length destination >= 2 && equal (sub destination 0 2) "//") ||
        String.(length destination >= 7 && equal (sub destination 0 7) "http://") ||
        String.(length destination >= 8 && equal (sub destination 0 8) "https://")
      in
      if absolute_link then
        match safe_inline label with
        | None -> `No_label
        | Some label -> `Link { l with label }
      else
        `Relative
    in
    List.filter_map safe_block omd

  let html_of_string markdown =
    markdown
    |> Omd.of_string 
    |> make_safe 
    |> Omd.to_html 

end
