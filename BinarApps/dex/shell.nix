let
  packages = import ./.;
  inherit (packages) pkgs uniswap easy-purescript-nix;
  inherit (pkgs) lib stdenv utillinux python3 nixpkgs-fmt;
  inherit (uniswap)
    haskell pre-commit-hooks-nix stylish-haskell purty-pre-commit;
  inherit (easy-purescript-nix) purs spago zephyr purty;

  # Configure project pre-commit hooks
  pre-commit-check = pre-commit-hooks-nix.run {
    src = ./.;
    tools = {
      stylish-haskell = stylish-haskell;
      nixpkgs-fmt = nixpkgs-fmt;
      shellcheck = pkgs.shellcheck;
      purty = purty-pre-commit;
    };
    hooks = {
      purty.enable = true;
      stylish-haskell.enable = true;
      hlint.enable = true;
      nixpkgs-fmt = {
        enable = true;
        # While nixpkgs-fmt does exclude patterns specified in `.ignore` this
        # does not appear to work inside the hook. For now we have to thus
        # maintain excludes here *and* in `./.ignore` and *keep them in sync*.
        excludes = [ ".*nix/sources.nix$" ".*/packages.nix$" ];
      };
      shellcheck.enable = true;
      png-optimization = {
        enable = true;
        name = "png-optimization";
        description = "Ensure that PNG files are optimized";
        entry = "${pkgs.optipng}/bin/optipng";
        files = "\\.png$";
      };
    };
  };

  nixpkgsInputs = (with pkgs;
    [ libsodium-vrf xz ghcid niv nixpkgs-fmt nodejs-14_x z3 zlib nodePackages.parcel-bundler ]
    ++ lib.optionals stdenv.isDarwin [ clang ]);

  localInputs = (with uniswap; [
    hlint
    cabal-install
    haskell-language-server
    stylish-haskell
    purs
    spago
    zephyr
    purty
  ]);

  devInputs = (with pkgs; [ haskellPackages.hasktags haskellPackages.hpc ]);

in
haskell.project.shellFor {
  withHoogle = false;

  shellHook = ''
    ${pre-commit-check.shellHook}
  ''
  # Work around https://github.com/NixOS/nix/issues/3345, which makes
  # tests etc. run single-threaded in a nix-shell.
  # Sets the affinity to cores 0-1000 for $$ (current PID in bash)
  # Only necessary for linux - darwin doesn't even expose thread
  # affinity APIs!
  + lib.optionalString stdenv.isLinux ''
    ${utillinux}/bin/taskset -pc 0-1000 $$
  '' + ''
    export PATH=$(pwd)/.ghcup/bin:$PATH

  '';

  nativeBuildInputs = nixpkgsInputs ++ localInputs ++ devInputs;
}
