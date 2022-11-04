module Set = OpamPackage.Set

type package = OpamPackage.t

let packages (switch : OpamFile.SwitchExport.t) =
  assert (Set.cardinal switch.selections.sel_pinned = 0);
  assert (Set.cardinal switch.selections.sel_compiler = 0);
  assert (Set.subset switch.selections.sel_roots switch.selections.sel_installed);
  switch.selections.sel_installed

let duniverse_dir = "x-opam-monorepo-duniverse-dirs"

module M = Map.Make(String)

let duniverse_dirs_data =
  (* the representation in the file is [ URL DIR [ HASH* ] ] *)
  let open OpamParserTypes.FullPos in
  let ( let* ) = Result.bind in
  let string ~ctx = function
    | { pelem = String s ; _ } -> Ok s
    | _ -> Error (`Msg ("couldn't find a string " ^ ctx))
  in
  let extract_data = function
    | { pelem = List { pelem = [ url ; dir ; hashes ] ; _ } ; _ } ->
      let* url = string ~ctx:"url" url in
      let* hashes =
        match hashes with
        | { pelem = List { pelem = hashes ; _ } ; _ } ->
          List.fold_left (fun acc hash ->
              let* acc = acc in
              let* hash = string ~ctx:"hash" hash in
              let* h = match OpamHash.of_string_opt hash with
                | Some h -> Ok OpamHash.(kind h, contents h)
                | None -> Error (`Msg ("couldn't decode opam hash in " ^ hash))
              in
              Ok (h :: acc))
            (Ok []) hashes
        | _ -> Error (`Msg "couldn't decode hashes")
      in
      let* dir = string ~ctx:"directory" dir in
      Ok (url, dir, List.rev hashes)
    | _ -> Error (`Msg "expected a string or identifier")
  in
  function
  | { pelem = List { pelem = lbody ; _ } ; _ } ->
    List.fold_left (fun acc v ->
        let* acc = acc in
        let* (url, dir, hashes) = extract_data v in
        Ok (M.add dir (url, hashes) acc))
      (Ok M.empty) lbody
  | _ -> Error (`Msg "expected a list or a nested list")

let duniverse (switch : OpamFile.SwitchExport.t) =
  let root = switch.OpamFile.SwitchExport.selections.OpamTypes.sel_roots in
  if OpamPackage.Set.cardinal root = 1 then
    let root = OpamPackage.Set.choose root in
    Option.bind
      OpamPackage.(Name.Map.find_opt root.name switch.OpamFile.SwitchExport.overlays)
      (fun opam ->
         match OpamFile.OPAM.extended opam duniverse_dir duniverse_dirs_data with
         | None -> None
         | Some Error _ -> None
         | Some Ok v -> Some v)
  else
    None

type duniverse_diff = {
  name : string ;
  urls : string * string option ;
  hash : (OpamHash.kind * string option * string option) list ;
}

let pp_duniverse_diff ppf { name ; urls ; hash } =
  let opt_hash = Option.value ~default:"NONE" in
  Format.fprintf ppf "%s (%s%s) %s"
    name
    (fst urls)
    (Option.fold ~none:"" ~some:(fun url -> "->" ^ url) (snd urls))
    (String.concat ", " (List.map (fun (h, l, r) ->
         OpamHash.string_of_kind h ^ " " ^ opt_hash l ^ "->" ^ opt_hash r) hash))

let pp_duniverse_pkg ppf (dir, url) =
  Format.fprintf ppf "%s (%s)" dir url

let duniverse_diff l r =
  let l = Option.value l ~default:M.empty
  and r = Option.value r ~default:M.empty
  in
  let keys_l_only = ref [] and keys_r_only = ref [] and diff = ref [] in
  let equal_hashes l r =
    (* l and r are lists of pairs, with the hash kind and its value *)
    List.for_all (fun (h, v) ->
        match List.assoc_opt h r with
        | None -> true
        | Some v' -> String.equal v v')
      l
  in
  let _ =
    M.merge (fun key l r ->
        match l, r with
        | None, Some _ -> keys_r_only := key :: !keys_r_only; None
        | Some _, None -> keys_l_only := key :: !keys_l_only; None
        | None, None -> None
        | Some (_, l), Some (_, r) when equal_hashes l r -> None
        | Some l, Some r -> diff := (key, l, r) :: !diff; None)
      l r
  in
  let pkg_only keys map =
    let only =
      M.filter (fun k _ -> List.mem k keys) map |> M.bindings
    in
    List.map (fun (key, (url, _)) -> key, url) only
  in
  let l_only = pkg_only !keys_l_only l
  and r_only = pkg_only !keys_r_only r
  and diff =
    List.map (fun (name, (url_l, hashes_l), (url_r, hashes_r)) ->
        let urls =
          if String.equal url_l url_r then url_l, None else url_l, Some url_r
        in
        let hash =
          List.fold_left (fun acc (h, v) ->
              match List.assoc_opt h hashes_r with
              | None -> (h, Some v, None) :: acc
              | Some v' ->
                if String.equal v v' then
                  acc
                else
                  (h, Some v, Some v') :: acc)
            [] hashes_l
        in
        let hash = List.fold_left (fun acc (h', v') ->
            match List.assoc_opt h' hashes_l with
            | None -> (h', None, Some v') :: acc
            | Some _ -> acc)
            hash hashes_r
        in
        { name ; urls ; hash })
      !diff
  in
  l_only, r_only, diff

type version_diff = {
  name : OpamPackage.Name.t;
  version_left : OpamPackage.Version.t;
  version_right : OpamPackage.Version.t;
}

let pp_opampackage ppf p =
  Format.fprintf ppf "%s" (OpamPackage.to_string p)

let pp_version_diff ppf { name; version_left; version_right } =
  Format.fprintf ppf "%s.%s->%s"
    (OpamPackage.Name.to_string name)
    (OpamPackage.Version.to_string version_left)
    (OpamPackage.Version.to_string version_right)

type opam_diff = {
  pkg : OpamPackage.t ;
  build : (OpamTypes.command list * OpamTypes.command list) option ;
  install : (OpamTypes.command list * OpamTypes.command list) option ;
  url : (OpamFile.URL.t option * OpamFile.URL.t option) option ;
  otherwise_equal : bool ;
}

let commands_to_strings (l, r) =
    let v a =
      OpamPrinter.FullPos.value (OpamPp.print OpamFormat.V.command a)
    in
    List.map v l, List.map v r

let opt_url_to_string (l, r) =
  let url_to_s = function
    | None -> "" | Some u -> OpamFile.URL.write_to_string u
  in
  url_to_s l, url_to_s r

let pp_opam_diff ppf { pkg ; otherwise_equal ; _ } =
  Format.fprintf ppf "%a%s"
    pp_opampackage pkg
    (if otherwise_equal then "" else " (and additional changes)")

let rec strip_common_prefix a b =
  match a, b with
  | hd::tl, hd'::tl' ->
    if hd = hd' then
      strip_common_prefix tl tl'
    else
      a, b
  | a, b -> a, b

let detailed_opam_diff pkg l r =
  let no_build_install_url p =
    OpamFile.OPAM.with_url_opt None
      (OpamFile.OPAM.with_install []
         (OpamFile.OPAM.with_build [] p))
  in
  let otherwise_equal =
    OpamFile.OPAM.effectively_equal
      (no_build_install_url l) (no_build_install_url r)
  and build =
    if OpamFile.OPAM.build l = OpamFile.OPAM.build r then
      None
    else
      Some (strip_common_prefix (OpamFile.OPAM.build l) (OpamFile.OPAM.build r))
  and install =
    if OpamFile.OPAM.install l = OpamFile.OPAM.install r then
      None
    else
      Some (strip_common_prefix (OpamFile.OPAM.install l) (OpamFile.OPAM.install r))
  and url =
    if OpamFile.OPAM.url l = OpamFile.OPAM.url r then
      None
    else
      Some (OpamFile.OPAM.url l, OpamFile.OPAM.url r)
  in
  { pkg ; build ; install ; url ; otherwise_equal }

let detailed_opam_diffs left right pkgs =
  OpamPackage.Set.fold (fun p acc ->
      let find = OpamPackage.Name.Map.find p.name in
      let opam_left = find left.OpamFile.SwitchExport.overlays
      and opam_right = find right.OpamFile.SwitchExport.overlays in
      (detailed_opam_diff p opam_left opam_right) :: acc)
    pkgs []

let compare left right =
  let packages_left = packages left and packages_right = packages right in
  let module Set = OpamPackage.Set in
  let equal_name p1 p2 = OpamPackage.Name.equal p1.OpamPackage.name p2.OpamPackage.name in
  let diff l r =
    Set.filter (fun p1 ->
        not (Set.exists (equal_name p1) r))
      l
  in
  let same_version = Set.inter packages_left packages_right in
  let opam_diff =
    Set.filter
      (fun p ->
         let find = OpamPackage.Name.Map.find p.name in
         let opam_left = find left.overlays and opam_right = find right.overlays in
         not (OpamFile.OPAM.effectively_equal opam_left opam_right))
      same_version
  and version_diff =
    List.filter_map (fun p1 ->
        match Set.find_opt (equal_name p1) packages_right with
        | Some p2 ->
          if OpamPackage.Version.equal p1.version p2.version
          then None
          else
            Some { name = p1.OpamPackage.name;
                   version_left = p1.OpamPackage.version;
                   version_right = p2.OpamPackage.version }
        | None ->
          None)
      (Set.elements packages_left)
  and left_pkgs = diff packages_left packages_right
  and right_pkgs = diff packages_right packages_left
  in
  let opam_diff = detailed_opam_diffs left right opam_diff in
  let left_duniverse, right_duniverse, duniverse_diff =
    duniverse_diff (duniverse left) (duniverse right)
  in
  (opam_diff, version_diff, left_pkgs, right_pkgs,
   duniverse_diff, left_duniverse, right_duniverse)
