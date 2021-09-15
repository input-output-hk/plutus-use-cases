let
  sources = import ./sources.nix { inherit pkgs; };

  plutus = import sources.plutus { };

  easy-purescript-nix = import sources.easy-purescript-nix { inherit pkgs; };

  pre-commit-hooks-nix = import sources.pre-commit-hooks-nix;

  pkgs = plutus.pkgs;

  haskell-nix = pkgs.haskell-nix;

  uniswap = import ./pkgs {
    inherit pkgs haskell-nix sources plutus pre-commit-hooks-nix;
  };

in
{
  inherit pkgs;

  inherit plutus uniswap easy-purescript-nix;
}
