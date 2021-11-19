#!/usr/bin/env bash
set -euo pipefail

# Note: call this script from within ./dep/cardano-node/result/alonzo-purple/

# ./buildCollateral.sh [UTXO] [UTXO INDEX] [TO ADDRESS] [CHANGE ADDRESS] [COLLATERAL AMOUNT] [PAYMENT KEY LOCATION]

export CARDANO_NODE_SOCKET_PATH=./state-node-alonzo-purple/node.socket 

utxo=$1
idx=$2
toAddress=$3
changeAddress=$4
collateralAmount=$5
paymentskey=$6

txcollateralraw=$(pwd)/txcollateral.raw
txcollateralsign=$(pwd)/txcollateral.sign
function mkcollateraltx {
  echo " -> Building collateral"
  ./cardano-cli/bin/cardano-cli transaction build \
    --alonzo-era \
    --tx-in "${utxo}#${idx}" \
    --tx-out $toAddress+$collateralAmount \
    --change-address $changeAddress \
    --testnet-magic 1097911063 \
    --out-file $txcollateralraw
  cat $txcollateralraw

  echo " -> Signing collateral"
  ./cardano-cli/bin/cardano-cli transaction sign --tx-body-file $txcollateralraw --signing-key-file $paymentskey --out-file $txcollateralsign
  echo " -> Submit collateral"
  ./cardano-cli/bin/cardano-cli transaction submit --testnet-magic 1097911063 --tx-file $txcollateralsign
} 

mkcollateraltx
