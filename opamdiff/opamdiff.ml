module Set = OpamPackage.Set

type package = OpamPackage.t

let packages (switch : OpamFile.SwitchExport.t) =
  assert (Set.cardinal switch.selections.sel_pinned = 0);
  assert (Set.cardinal switch.selections.sel_compiler = 0);
  assert (Set.subset switch.selections.sel_roots switch.selections.sel_installed);
  switch.selections.sel_installed

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
  (opam_diff, version_diff, left_pkgs, right_pkgs)
