{ lib, gitignore-nix, haskell-nix, sources, compiler-nix-name }:
let
  # The Hackage index-state from cabal.project
  index-state =
    let
      parseIndexState = rawCabalProject:
        let
          indexState = lib.lists.concatLists (lib.lists.filter (l: l != null)
            (map (l: builtins.match "^index-state: *(.*)" l)
              (lib.splitString "\n" rawCabalProject)));
        in
        lib.lists.head (indexState ++ [ null ]);
    in
    parseIndexState (builtins.readFile ../../../cabal.project);

  project = import ./haskell.nix {
    inherit haskell-nix compiler-nix-name gitignore-nix;
  };

  packages = project.hsPkgs;

  projectPackages = haskell-nix.haskellLib.selectProjectPackages packages;

in
rec { inherit project projectPackages packages; }
