{ pkgs, sources, plutus, haskell-nix, pre-commit-hooks-nix }:
let
  gitignore-nix = pkgs.callPackage plutus."gitignore.nix" { };

  compiler-nix-name = plutus.plutus.haskell.compiler-nix-name;

  haskell = pkgs.callPackage ./haskell {
    inherit gitignore-nix sources haskell-nix;
    inherit compiler-nix-name;
  };

  hlint = plutus.plutus.hlint;

  cabal-install = plutus.plutus.cabal-install;

  stylish-haskell = plutus.plutus.stylish-haskell;

  haskell-language-server = plutus.plutus.haskell-language-server;

  purty-pre-commit = plutus.plutus.purty-pre-commit;

in
{
  inherit haskell hlint cabal-install stylish-haskell;
  inherit haskell-language-server pre-commit-hooks-nix purty-pre-commit;
}
