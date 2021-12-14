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
### run step
In new nix shell
`bash chainindex/start-chainindex.sh`

## Create addresses 
`bash address-gen.sh`
it will create keys set oracle and client in keys folder

### Send money to the new adddresses 
go to 
https://testnets.cardano.org/en/testnets/cardano/tools/faucet/
and use payment.addr to top up the balance
## Run oracle demo
In new nix shell
`bash demo/oracle-demo.sh`
