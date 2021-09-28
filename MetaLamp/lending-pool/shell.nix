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
      haskell-language-server
      stylish-haskell
      fix-stylish-haskell
      pkgs.niv
      cardano-repo-tool
    ];
  }
