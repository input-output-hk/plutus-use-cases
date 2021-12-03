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
      "https://github.com/input-output-hk/plutus-apps.git"."v2021-11-22" = "10m0gmnakjnpnzyvjs3ksfpxgjcq3pdphr4gf8v7fjhr9fjbc45n";
      "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "1lrzknw765pz2j97nvv9ip3l1mcpf2zr4n56hwlz0rk7wq7ls4cm";
      "https://github.com/input-output-hk/purescript-bridge.git"."366fc70b341e2633f3ad0158a577d52e1cd2b138" = "18j0rysfccbmfpbw2d1rsjkpd5h84alpsn6b5rwzdxw9h5vqi9m5";
      "https://github.com/input-output-hk/servant-purescript.git"."ebea59c7bdfc0338d83fca772b9a57e28560bcde" = "0gjcq4y61kwb4w70pnswn5dp23wd13dac8d9hz84j374cm1kshsn";
      "https://github.com/input-output-hk/cardano-base"."4ea7e2d927c9a7f78ddc69738409a5827ab66b98" = "0n0hxbr0l95cdc25jmmgs7apmmw17i91chhj5rzzv1k7f3iymf6d";
      "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "06sdx5ndn2g722jhpicmg96vsrys89fl81k8290b3lr6b1b0w4m3";
      "https://github.com/input-output-hk/cardano-ledger-specs"."bf008ce028751cae9fb0b53c3bef20f07c06e333" = "0my3801w1vinc0kf5yh9lxl6saqxgwm6ccg0vvzi104pafcwwcqx";
      "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" = "02jddik1yw0222wd6q0vv10f7y8rdgrlqaiy83ph002f9kjx7mh6";
      "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
      "https://github.com/input-output-hk/iohk-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c" = "1il8fx3misp3650ryj368b3x95ksz01zz3x0z9k00807j93d0ka0";
      "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" = "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";
      "https://github.com/input-output-hk/ouroboros-network"."1f4973f36f689d6da75b5d351fb124d66ef1057d" = "186056rvzdzy4jhvamjjbcmjyr94hs5hcyr8x6a0ch21hv5f014p";
      "https://github.com/input-output-hk/cardano-node.git"."b6ca519f97a0e795611a63174687e6bb70c9f752" = "0z5lpmqc98fwg3xzpzxkfslbxdjwfyyw8bn8yq0574sf4942vqdn";
      "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
      "https://github.com/input-output-hk/hedgehog-extras"."edf6945007177a638fbeb8802397f3a6f4e47c14" = "0wc7qzkc7j4ns2rz562h6qrx2f8xyq7yjcb7zidnj7f6j0pcd0i9";
      "https://github.com/input-output-hk/cardano-addresses"."d2f86caa085402a953920c6714a0de6a50b655ec" = "0p6jbnd7ky2yf7bwb1350k8880py8dgqg39k49q02a6ij4ld01ay";
      "https://github.com/input-output-hk/plutus"."3f089ccf0ca746b399c99afe51e063b0640af547" = "1nx8xmdgwmnsla4qg4k67f5md8vm3p1p9i25ndalrqdg40z90486";
      "https://github.com/j-mueller/cardano-wallet"."6be73ab852c0592713dfe78218856d4a8a0ee69e" = "0rx5hvmbdv5dwb4qq39vyhisj0v75j21jbiivn3s3q9za6m6x1p4";
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