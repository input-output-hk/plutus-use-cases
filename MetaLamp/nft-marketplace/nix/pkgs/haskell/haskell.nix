############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ haskell-nix
, gitignore-nix
, compiler-nix-name
, lib
, libsodium-vrf
}:

let
  project = haskell-nix.project {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = haskell-nix.haskellLib.cleanGit {
      name = "plutus-starter";
      src = ../../../.;
    };

    inherit compiler-nix-name;

    sha256map = {
      "https://github.com/olgaklimenko/plutus-apps.git"."e8c5f532e5563c6638c612f6d8dfba9580813c94" = "0kd37x4wwif67pd0sgmrwda7l6kmffff0lpk8yq040wxjki0f8ff";
      "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "1lrzknw765pz2j97nvv9ip3l1mcpf2zr4n56hwlz0rk7wq7ls4cm";
      "https://github.com/input-output-hk/purescript-bridge.git"."366fc70b341e2633f3ad0158a577d52e1cd2b138" = "18j0rysfccbmfpbw2d1rsjkpd5h84alpsn6b5rwzdxw9h5vqi9m5";
      "https://github.com/input-output-hk/servant-purescript.git"."ebea59c7bdfc0338d83fca772b9a57e28560bcde" = "0gjcq4y61kwb4w70pnswn5dp23wd13dac8d9hz84j374cm1kshsn";
      "https://github.com/input-output-hk/cardano-base"."654f5b7c76f7cc57900b4ddc664a82fc3b925fb0" = "0j4x9zbx5dkww82sqi086h39p456iq5xr476ylmrnpwcpfb4xai4";
      "https://github.com/input-output-hk/cardano-crypto.git"."f73079303f663e028288f9f4a9e08bcca39a923e" = "1n87i15x54s0cjkh3nsxs4r1x016cdw1fypwmr68936n3xxsjn6q";
      "https://github.com/input-output-hk/cardano-ledger.git"."bf008ce028751cae9fb0b53c3bef20f07c06e333" = "0my3801w1vinc0kf5yh9lxl6saqxgwm6ccg0vvzi104pafcwwcqx";
      "https://github.com/input-output-hk/cardano-prelude"."bb4ed71ba8e587f672d06edf9d2e376f4b055555" = "00h10l5mmiza9819p9v5q5749nb9pzgi20vpzpy1d34zmh6gf1cj";
      "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
      "https://github.com/input-output-hk/iohk-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c" = "1il8fx3misp3650ryj368b3x95ksz01zz3x0z9k00807j93d0ka0";
      "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" = "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";
      "https://github.com/input-output-hk/ouroboros-network"."d613de3d872ec8b4a5da0c98afb443f322dc4dab" = "0lfbipfdrzay8v1pcazx0qgkda3d1j0505yig9jrml9j7991rmhl";
      "https://github.com/input-output-hk/cardano-node.git"."4f65fb9a27aa7e3a1873ab4211e412af780a3648" = "00k9fqrm0gphjji23x0nc9z6bqh8bqrncgivn3mi3csacjzicrrx";
      "https://github.com/input-output-hk/cardano-config.git"."e9de7a2cf70796f6ff26eac9f9540184ded0e4e6" = "1wm1c99r5zvz22pdl8nhkp13falvqmj8dgkm8fxskwa9ydqz01ld";
      "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
      "https://github.com/input-output-hk/hedgehog-extras"."edf6945007177a638fbeb8802397f3a6f4e47c14" = "0wc7qzkc7j4ns2rz562h6qrx2f8xyq7yjcb7zidnj7f6j0pcd0i9";
      "https://github.com/input-output-hk/cardano-addresses"."d2f86caa085402a953920c6714a0de6a50b655ec" = "0p6jbnd7ky2yf7bwb1350k8880py8dgqg39k49q02a6ij4ld01ay";
      "https://github.com/input-output-hk/plutus"."2721c59fd2302b75c4138456c29fd5b509e8340a" = "02g8pzldyfl4pm8sy22yd3l2fr3zpyhwkvv9x3h9lsf6lfx5wi7k";
      "https://github.com/input-output-hk/cardano-wallet"."760140e238a5fbca61d1b286d7a80ece058dc729" = "014njpddrlqm9bbab636h2gf58zkm0bx04i1jsn07vh5j3k0gri6";
    };

    modules = [
      {
        packages = {
          # Broken due to haddock errors. Refer to https://github.com/input-output-hk/plutus/blob/master/nix/pkgs/haskell/haskell.nix
          plutus-ledger.doHaddock = false;
          plutus-use-cases.doHaddock = false;

          # See https://github.com/input-output-hk/iohk-nix/pull/488
          cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
          cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
        };
      }
    ];
  };
in
  project