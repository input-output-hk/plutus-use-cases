let
  # Here a some of the various attributes for the variable 'packages':
  #
  # { pkgs
  #   mlabs-plutus-use-cases: {
  #     haskell: {
  #       project # The Haskell project created by haskell-nix.project
  #       packages # All the packages defined by our project, including dependencies
  #       projectPackages # Just the packages in the project
  #     }
  #     hlint
  #     cabal-install
  #     stylish-haskell
  #     haskell-language-server
  #   }
  # }
  packages = import ./nix;

  inherit (packages) pkgs mlabs-plutus-use-cases;
  project = mlabs-plutus-use-cases.haskell.project;
in
{
  inherit pkgs mlabs-plutus-use-cases;

  inherit project;
}
