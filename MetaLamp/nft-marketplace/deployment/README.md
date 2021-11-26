## Testnet deployment steps:

### Set up local environment

- Cd to `./env` and create a file with local env variables `env.local.sh` from the `env.local.sh`.

- export your environment variables:

```
source env/env.local.sh
```

### Run cardano-node and cardano-wallet

If it is your first start, you haven't download testnet configs, run: 

```
sh first-start.sh $NODE_TAG $NODE_PATH
```

For the next time starts run:

```
sh start.sh $NODE_TAG $NODE_PATH
```

You can enter to wallet and node containers using following commands:

```
docker  exec -ti cardano-wallet bash

docker  exec -ti cardano-node bash
```

### Start chain-index

Copy `./config/chain-index-config.template.json` to the `./config/chain-index-config.local.json`, and change <NODE_PATH> to your node path directory.

Go to the `plutus-apps` project from new terminal window and run chain-index:

```
cd plutus-apps

nix build -f default.nix plutus-apps.haskell.packages.plutus-chain-index.components.exes.plutus-chain-index

./result/bin/plutus-chain-index --config ../plutus-use-cases/MetaLamp/nft-marketplace/config/chain-index-config.local.json start-index
```

### Set up the PAB

Copy `./config/plutus-pab.template.yaml` to the `./config/plutus-pab.local.yaml`, and change <NODE_PATH> to your node path directory.

### Create a wallet

Step 2 and step 3 from [document](https://gist.github.com/mikekeke/883d56c38e0237444ac98ae5257e174f).
Save a wallet passphrase, it is required for the next step.

### Run Dapp

Go to the `plutus-use-cases/MetaLamp/nft-marketplace`, enter the nix shell and run migrations to create pab database, then run the PAB application itself:

```
cabal exec pab-app -- --config ./config/plutus-pab.local.yaml migrate 

cabal exec pab-app -- --config ./config/plutus-pab.local.yaml --passphrase "WALLET_PASSPHRASE" webserver  
```

Created on the basis of [instruction](https://gist.github.com/mikekeke/883d56c38e0237444ac98ae5257e174f).
