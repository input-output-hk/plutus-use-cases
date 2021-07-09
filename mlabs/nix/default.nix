let
  haskell-nix = pkgs.haskell-nix;
  mlabs-plutus-use-cases = import ./pkgs {
    inherit pkgs haskell-nix sources plutus;
  };
  sources = import ./sources.nix { inherit pkgs; };
  plutus = import sources.plutus { };
  pkgs = plutus.pkgs;
  pab = import ./pab.nix { inherit plutus; };
in
{
  inherit pkgs mlabs-plutus-use-cases;
}
