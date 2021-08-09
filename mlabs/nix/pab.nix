{ plutus, pkgs ? plutus.pkgs }: rec {
  # PAB setup
  plutus_pab_exes = plutus.plutus-pab.pab-exes;
  plutus_pab_client = plutus.plutus-pab.client;

  plutus_pab_db_path = "/tmp";
  plutus_pab_confs = import ./pab_conf.nix {
    db-path = plutus_pab_db_path;
    client = plutus_pab_client;
  };

  # Annoyingly, the mkConf from Pab has a fixed name...
  # The plutus build by default misses this
  plutus_pab_conf_dir = with plutus_pab_confs;
    pkgs.linkFarm "plutus_pab_envs" [
      {
        inherit (pab_env1) name;
        path = plutus.plutus-pab.mkConf pab_env1;
      }

      {
        inherit (pab_env2) name;
        path = plutus.plutus-pab.mkConf pab_env2;
      }
    ];

  plutus_ledger_with_docs =
    plutus.plutus.haskell.packages.plutus-ledger.components.library.override {
      doHaddock = true;
      configureFlags = [ "-f defer-plugin-errors" ];
    };

  env_variables = {
    PAB_CONFIG_PATH = plutus_pab_conf_dir;
    PAB_CLIENT_PATH = plutus_pab_client;
    PAB_DB1_PATH = plutus_pab_confs.pab_env1.db-file;
    PAB_DB2_PATH = plutus_pab_confs.pab_env2.db-file;
  };

}
