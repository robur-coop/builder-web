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
  let (same, opam_diff) =
    Set.partition
      (fun p ->
         let find = OpamPackage.Name.Map.find p.name in
         let opam_left = find left.overlays and opam_right = find right.overlays in
         OpamFile.OPAM.effectively_equal opam_left opam_right)
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
  and left = diff packages_left packages_right
  and right = diff packages_right packages_left
  in
  (same, opam_diff, version_diff, left, right)
