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
