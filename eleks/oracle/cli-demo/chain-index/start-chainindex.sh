#!/bin/bash
set -e

SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

echo uraa $SCRIPTPATH
cd $SCRIPTPATH/../../../../../plutus-apps/plutus-pab
pwd
set -x
cabal exec -- plutus-chain-index --config $SCRIPTPATH/chain-index-config.json --db-path $SCRIPTPATH/chain-index.db --socket-path /tmp/node.socket start-index
