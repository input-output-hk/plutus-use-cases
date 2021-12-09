# Testnet demo


## Prerequsites

### install wget
### cd to the working directory 
`cd ./plutus-use-cases/eleks/oracle` 
next commands run in the current directory

## Setup cardano
Run cardano node
### Install cardano node
`bash demo/node/install-cardano.sh`
### Run cardano node
`bash demo/node/run.sh`
In new terminal wait unitl synced via query 
`
export CARDANO_NODE_SOCKET_PATH=demo/node/db/node.socket
cardano-cli query tip --testnet-magic 1097911063
`
it should return 100%

## Build oracle script

In new window run `nix shell`
### Build project 
in nix terminal
`cabal build all`

### Generate script files
in nix terminal
`cabal exec -- gs 1500000 1000000 "demo/keys/oracle/payment.vkey"`

## Run oracle demo
In new window
`bash demo/oracle-demo.sh`


## Query utxo


cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address addr_test1wrpqw0xzgw0t237z6ut8kpcgep5jzqpvgky0z0slzgfmeqs7xyrc4

curl -vk -H "Content-Type: application/json"-XPOST http://localhost:9083/utxo-at-address/ -d '"addr_test1wrpqw0xzgw0t237z6ut8kpcgep5jzqpvgky0z0slzgfmeqs7xyrc4"'
curl http://localhost:9083/diagnostics

curl -XPOST http://localhost:9083/from-hash/datum/validator/addr_test1wrpqw0xzgw0t237z6ut8kpcgep5jzqpvgky0z0slzgfmeqs7xyrc4


curl -vk -H "Content-Type: application/json" -XPOST http://localhost:9083/from-hash/datum -d '"38de6c1dfd313615897ba388e4cd502e85f71c610ef43b3fd3677a49"'