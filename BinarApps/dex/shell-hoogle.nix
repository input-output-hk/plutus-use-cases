let
  packages = import ./.;
  inherit (packages) uniswap;
  inherit (uniswap) haskell;

in
haskell.project.shellFor {
  packages = ps: [ ps.dex ];
  withHoogle = true;
}
