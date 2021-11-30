#!/usr/bin/env bash

# ./buildUniswapTokenAddress [UNISWAP PLUTUS SCRIPT]

set -euox pipefail

#build script address and send uniswap token to it

uniswapPlutusScript=$1

CARDANO_NODE_SOCKET_PATH=./state-node-testnet/node.socket \
cardano-cli/bin/cardano-cli address build --testnet-magic 1097911063 --payment-script-file \
$uniswapPlutusScript >> uniswapPlutusScript.address
