#!/usr/bin/env bash

# ./buildPool.sh [TXHASH#TXIX] [TOKEN B TXHASH#TXIX] [UNISWAP TOKEN TXHASH#TXIX] [SCRIPT FILE] [UNISWAP SCRIPT ADDRESS] [UNISWAP TOKEN CURRENCY SYMBOL] [POOL TOKEN CURRENCY SYMBOL] [FACTORY DATUM EMBED FILE] [LIQUIDITY POOL DATUM EMBED FILE] [CHANGE ADDRESS] [LIQUIDITY CURRENCY POLICY] [UNIPOOL DATUM HASH] [UNISWAP ACTION REDEEMER FILE] [SKEY FILE] [LIQUIDITY TOKEN CURRENCY SYMBOL] [TOKEN B CURRENCY SYMBOL]

set -euox pipefail

uniswapToken=$6
poolToken=$7
scriptFile=$4
scriptAddr=$5
factoryDatumEmbed=$8
lpDatumEmbed=$9
changeAddress=${10}
protocolparams="./buildPool-protocolparams"
bodyFile=buildPool-tx-body
liquidityCurrencyPolicy=${11}
uniPoolDatumFile=${12}
redeemerUniswapAction=${13}
#liquidityCoinValue="207875f104148d5f3bacb2601ce9ee519defbd276be5fe241d84107a.PikaCoinPikaCoin"
liquidityCoinValue=${14}
outFile="./buildPool-tx-signed"
#tokenAValue="e41bbd4c8c419c825945d05499ba41cc53181b44b8ac056d24dbdb42.PikaCoin"
tokenAValue=${15}
./cardano-cli/bin/cardano-cli query protocol-parameters --testnet-magic 8 > $protocolparams

echo "queried and set protocolparams $protocolparams"

./cardano-cli/bin/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 8 \
    --tx-in $1 \
    --tx-in $2 \
    --tx-in $3 \
    --tx-in-script-file $scriptFile \
    --tx-in-datum-file $uniPoolDatumFile \
    --tx-in-redeemer-file $redeemerUniswapAction \
    --tx-in-collateral $1 \
    --tx-out "$scriptAddr + 1689618 lovelace + 1 $uniswapToken" \
    --tx-out-datum-embed-file $factoryDatumEmbed \
    --tx-out "$scriptAddr + 1 $poolToken + 1930992 lovelace + 100000 $tokenAValue" \
    --tx-out-datum-embed-file $lpDatumEmbed \
    --tx-out "$changeAddress + 1641143 + 439431 $liquidityCoinValue" \
    --mint "1 $poolToken + 439431 $liquidityCoinValue" \
    --mint-script-file $liquidityCurrencyPolicy \
    --mint-redeemer-file ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/unipool-redeemer \
    --change-address $changeAddress \
    --protocol-params-file $protocolparams \
    --out-file $bodyFile

./cardano-cli/bin/cardano-cli transaction sign \
    --tx-body-file $bodyFile \
    --signing-key-file $skey \
    --testnet-magic 8 \
    --out-file $outFile

echo "signed transaction and saved as $outFile"

./cardano-cli/bin/cardano-cli transaction submit \
    --testnet-magic 8 \
    --tx-file $outFile

echo "submitted transaction"

