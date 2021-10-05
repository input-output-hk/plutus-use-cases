#!/usr/bin/env bash
set -euo pipefail

# ./buildCollateral.sh [UTXO] [UTXO INDEX] [TO ADDRESS] [CHANGE ADDRESS] [COLLATERAL AMOUNT]

export CARDANO_NODE_SOCKET_PATH=./state-node-alonzo-purple/node.socket 

utxo=$1
idx=$2
toAddress=$3
changeAddress=$4
collateralAmount=$5

txcollateralraw=$(pwd)/txcollateral.raw
txcollateralsign=$(pwd)/txcollateral.sign
function mkcollateraltx {
  echo " -> Building collateral"
  ./cardano-cli/bin/cardano-cli transaction build \
    --alonzo-era \
    --tx-in $utxo"#"$idx \ #"51f73e6c2fd2d6bca9498c2a7f680c304d8ac2e556f2e8107919516b60e81b30#0" \
    --tx-out $toAddress+$collateralAmount \ #"addr_test1qrlt4547kcveetpcrqfnwy2m6twsh2lwncsyt60c4aeflwljl2wj5av3e50fr80j5qa8gg7v07caf0s7c8xwp7we6rks5lmf57"+5000000 \
    --change-address $changeAddress \ #"addr_test1qrlt4547kcveetpcrqfnwy2m6twsh2lwncsyt60c4aeflwljl2wj5av3e50fr80j5qa8gg7v07caf0s7c8xwp7we6rks5lmf57" \
    --testnet-magic 8 \
    --out-file $txcollateralraw
  cat $txcollateralraw

  paymentskey=$(pwd)/payment.skey
  echo " -> Signing collateral"
  ./cardano-cli/bin/cardano-cli transaction sign --tx-body-file $txcollateralraw --signing-key-file $paymentskey --out-file $txcollateralsign
  echo " -> Submit collateral"
  ./cardano-cli/bin/cardano-cli transaction submit --testnet-magic 8 --tx-file $txcollateralsign
} 

mkcollateraltx
