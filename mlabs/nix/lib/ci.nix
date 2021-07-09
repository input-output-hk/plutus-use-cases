{ pkgs }:

let
  # Generic nixpkgs, use *only* for lib functions that are stable across versions
  lib = pkgs.lib;
in
rec {

  dimension = name: attrs: f:
    builtins.mapAttrs
      (k: v:
        let o = f k v;
        in o // { recurseForDerivations = o.recurseForDerivations or true; }
      )
      attrs
    // { meta.dimension.name = name; };

  /*
    Takes an attribute set and returns all the paths to derivations within it, i.e.
    derivationPaths { a = { b = <drv>; }; c = <drv>; } == [ "a.b" "c" ]
    This can be used with 'attrByPath' or the 'constitutents' of an aggregate Hydra job.
  */
  derivationPaths =
    let
      names = x: lib.filter (n: n != "recurseForDerivations" && n != "meta") (builtins.attrNames x);
      go = nameSections: attrs:
        builtins.concatMap
          (n:
            let
              v = builtins.getAttr n attrs;
              newNameSections = nameSections ++ [ n ];
            in
            if pkgs.lib.isDerivation v
            then [ (builtins.concatStringsSep "." newNameSections) ]
            else if builtins.isAttrs v
            then go newNameSections v
            else [ ]
          )
          (names attrs);
    in
    go [ ];

  # Creates an aggregate job with the given name from every derivation in the attribute set.
  derivationAggregate = name: attrs: pkgs.releaseTools.aggregate {
    inherit name;
    constituents = derivationPaths attrs;
  };

  # A filter for removing packages that aren't supported on the current platform
  # according to 'meta.platforms'.
  platformFilterGeneric = pkgs: system:
    # This needs to use the correct nixpkgs version so all the systems line up
    let
      lib = pkgs.lib;
      platform = lib.systems.elaborate { inherit system; };
      # Can't just default to [] for platforms, since no meta.platforms
      # means "all platforms" not "no platforms"
    in
    drv:
    if drv ? meta && drv.meta ? platforms then
      lib.any (lib.meta.platformMatch platform) drv.meta.platforms
    else true;

  # Hydra doesn't like these attributes hanging around in "jobsets": it thinks they're jobs!
  stripAttrsForHydra = filterAttrsOnlyRecursive (n: _: n != "recurseForDerivations" && n != "dimension");

  # Keep derivations and attrsets with 'recurseForDerivations'. This ensures that we match the
  # derivations that Hercules will see, and prevents Hydra from trying to pick up all sorts of bad stuff
  # (like attrsets that contain themselves!).
  filterDerivations = filterAttrsOnlyRecursive (n: attrs: lib.isDerivation attrs
                                                          || attrs.recurseForDerivations or false);

  # A version of 'filterAttrsRecursive' that doesn't recurse into
  # derivations. This prevents us from going into an infinite loop with the
  # 'out' attribute on derivations.  TODO: Surely this shouldn't be necessary. I
  # think normal 'filterAttrsRecursive' will effectively cause infinite loops if
  # you keep derivations and your predicate forces the value of the attribute,
  # as this then triggers a loop on the 'out' attribute. Weird.
  filterAttrsOnlyRecursive = pred: set:
    lib.listToAttrs (
      lib.concatMap
        (name:
          let v = set.${name}; in
          if pred name v then [
            (lib.nameValuePair name (
              if builtins.isAttrs v && !lib.isDerivation v then filterAttrsOnlyRecursive pred v
              else v
            ))
          ] else [ ]
        )
        (builtins.attrNames set)
    );

  # Takes an array of systems and returns a `name: system` AttrSet
  # filterSystems :: [ string ] -> AttrSet
  filterSystems = systems: lib.filterAttrs (_: v: builtins.elem v systems) {
    linux = "x86_64-linux";
    darwin = "x86_64-darwin";
  };
}
