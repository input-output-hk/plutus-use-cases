#!/bin/bash
set -e

SCRIPTPATH="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"

cd $SCRIPTPATH/../../../../../plutus-apps/plutus-pab
set -x
cabal exec -- plutus-chain-index --config $SCRIPTPATH/chain-index-config.json --db-path $SCRIPTPATH/chain-index.db --socket-path /tmp/node.socket start-index
