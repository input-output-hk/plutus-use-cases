#!/usr/bin/env bash

#./sendUniswapTokenToUniswapScript [TOKEN ADDRESS] [UTXO] [COLLATERAL UTXO] [PAYMENT SKEY] [SCRIPT ADDRESS FILE] [DATUM HASH FILE] [CHANGE ADDRESS] [UNISWAP CURRENCY SYMBOL]

set -euox pipefail

scriptAddr=$(cat $5)
currencySymbol=$8
datumHash=$(cat $6)
value="1689618 lovelace + 1 $currencySymbol.Uniswap"
bodyFile=sendUniswapToken-tx-body
outFile=sendUniswapToken-tx-signed
protocolparams="./sendToken-protocolparams"

./cardano-cli/bin/cardano-cli query protocol-parameters --testnet-magic 1097911063 > $protocolparams

echo "queried and set protocolparams $protocolparams"

./cardano-cli/bin/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --tx-in $1 \
    --tx-in $2 \
    --tx-in-collateral $2 \
    --tx-out "$scriptAddr + $value" \
    --tx-out-datum-hash $datumHash \
    --change-address $7 \
    --protocol-params-file $protocolparams \
    --out-file $bodyFile

echo "saved transaction to $bodyFile"

./cardano-cli/bin/cardano-cli transaction sign \
    --tx-body-file $bodyFile \
    --signing-key-file $4 \
    --testnet-magic 1097911063 \
    --out-file $outFile

echo "signed transaction and saved as $outFile"

./cardano-cli/bin/cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file $outFile

echo "submitted transaction"

