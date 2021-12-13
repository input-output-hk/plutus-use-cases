# Testnet demo


## Prerequsites

### install wget

## Starting nix shell
if not cloned - clone https://github.com/input-output-hk/plutus-apps
cd to project root directory
run `nix shell`
go to working demo directory `cd ./plutus-use-cases/eleks/oracle/cli-demo` 
### build sources 
in nix terminal
`cabal update`
`cabal build all`
## Setup cardano
Run cardano node
### Install cardano node
`bash node/install-cardano.sh`
### Run cardano node
`bash node/start-node.sh`
In new terminal wait unitl synced via query 
`
export CARDANO_NODE_SOCKET_PATH=demo/node/db/node.socket
cardano-cli query tip --testnet-magic 1097911063
`
it should return 100%

## Start chainindex

in new windows run Starting nix shell step

### run step
`bash chainindex/start-chainindex.sh`

## Run oracle demo
In new window
`bash demo/oracle-demo.sh`
