#!/usr/bin/env bash

# ./buildPool.sh [TXHASH#TXIX] [TOKEN A TXHASH#TXIX] [TOKEN B TXHASH#TXIX] [SCRIPT FILE] [UNISWAP SCRIPT ADDRESS] [UNISWAP TOKEN CURRENCY SYMBOL] [POOL TOKEN CURRENCY SYMBOL] [FACTORY DATUM HASH] [LIQUIDITY POOL DATUM HASH] [CHANGE ADDRESS]

set -euo pipefail
uniswapToken=1 + $6
poolToken=1 + $7
scriptFile=$4
scriptAddr=$5
factoryDatumHash=$8
lpDatumHash=$9
changeAddress=$10
protocolparams="./buildPool-protocolparams"
bodyFile=buildPool-tx-body

./cardano-cli/bin/cardano-cli query protocol-parameters --testnet-magic 8 > $protocolparams

echo "queried and set protocolparams $protocolparams"

./cardano-cli/bin/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 8 \
    --tx-in $1 \ #Utxo with enough funds
    --tx-in $2 \ #PikaCoin Utxo
    --tx-in $3 \ #Uniswap Utxo
    --tx-in-script-file $scriptFile \
    --tx-in-datum-file [] \ # file Factory []
    --tx-in-redeemer-file [] \ # Uniswap Action
    --tx-in-collateral $1 \
    --tx-out "$scriptAddr + 2034438 lovelace + $uniswapToken" \ #Factory
    --tx-out-datum-hash $factoryDatumHash \ #Factory
    --tx-out "$scriptAddr + 2034438 lovelace + $poolToken" \ #Liquidity Pool
    --tx-out-datum-hash $lpDatumHash \ # Liquidity Pool
    --mint "$poolStateValue + $liquidityCoinValue" \
    --mint-script-file $otherPolicyFile \ #mintingPolicy for this
    --mint-redeemer-value [] \
    --change-address $changeAddress \ #utxo
    --protocol-params-file $protocolparams \
    --out-file $bodyFile
