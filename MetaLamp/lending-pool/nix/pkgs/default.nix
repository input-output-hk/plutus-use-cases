{ pkgs
, sources
, plutus
, haskell-nix
}:
let
  gitignore-nix = pkgs.callPackage plutus."gitignore.nix" { };

  compiler-nix-name = plutus.plutus.haskell.compiler-nix-name;

  haskell = pkgs.callPackage ./haskell {
    inherit gitignore-nix sources haskell-nix;
    inherit compiler-nix-name; # Use the same GHC version as plutus
    inherit (pkgs) libsodium-vrf;
  };

  hlint = plutus.plutus.hlint;

  cabal-install = plutus.plutus.cabal-install;

  nodejs = plutus.pkgs.nodejs;

  purs = plutus.plutus.purs;

  spago = plutus.plutus.spago;

  purty = plutus.plutus.purty;

  fix-purty = plutus.plutus.fixPurty;

  fix-stylish-haskell = plutus.plutus.fixStylishHaskell;

  stylish-haskell = plutus.plutus.stylish-haskell;

  haskell-language-server = plutus.plutus.haskell-language-server;

  cardano-repo-tool = plutus.plutus.cardano-repo-tool;
in
{
  inherit nodejs purs spago purty fix-purty;
  inherit haskell hlint cabal-install stylish-haskell fix-stylish-haskell haskell-language-server;
  inherit cardano-repo-tool;
}