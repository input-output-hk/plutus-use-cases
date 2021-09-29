# This set is fed in as arguments to a derivation which
# generates a config file.
{ nodeserver-port ? "9082", client, db-path ? "./.tmp" }: {
  pab_env1 = {
    inherit client nodeserver-port;
    name = "pab_env1.yaml";

    # DB
    db-file = "${db-path}/pab_env1.db";

    # Ports
    webserver-port = "9080";
    walletserver-port = "9081";
    chain-index-port = "9083";
    signing-process-port = "9084";
    metadata-server-port = "9085";

    # Wallet 1
    wallet = "1";
  };

  pab_env2 = {
    inherit client nodeserver-port;
    name = "pab_env2.yaml";

    # DB
    db-file = "${db-path}/pab_env2.db";

    webserver-port = "9090";
    walletserver-port = "9091";
    chain-index-port = "9093";
    signing-process-port = "9094";
    metadata-server-port = "9095";

    wallet = "2";
  };
}
