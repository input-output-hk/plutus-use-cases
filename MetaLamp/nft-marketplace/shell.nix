let
  packages = import ./.;
  inherit (packages) pkgs plutus-starter;
  inherit (plutus-starter) haskell;

in
  haskell.project.shellFor {
    withHoogle = false;

    nativeBuildInputs = with plutus-starter; [
      hlint
      cabal-install
      nodejs
      purs
      spago
      purty
      fix-purty
      fix-stylish-haskell
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ];
  }
