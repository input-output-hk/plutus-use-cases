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
liquidityCoinValue="75a70a2a2e897886839a7a30935c2aa36a27406c5e67d21d8f749df5.PikaCoinPikaCoin"
skey=${14}
outFile="./buildPool-tx-signed"
tokenAValue="17e86dfec8981df58e070430ce87fc7f51d5be7137cc0203ebac00c8.PikaCoin"
# tokenAValue=${15}
./cardano-cli/bin/cardano-cli query protocol-parameters --testnet-magic 1097911063 > $protocolparams

echo "queried and set protocolparams $protocolparams"

./cardano-cli/bin/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --tx-in "8c01c5cb6c0a211f17a54e9e10655d4e2a77d5a1e9e2693973096452fb649de2#0" \
    --tx-in $1 \
    --tx-in $2 \
    --tx-in $3 \
    --tx-in-script-file $scriptFile \
    --tx-in-datum-file $uniPoolDatumFile \
    --tx-in-redeemer-file $redeemerUniswapAction \
    --tx-in-collateral $1 \
    --tx-out "$scriptAddr + 1689618 lovelace + 1 $uniswapToken" \
    --tx-out-datum-embed-file $factoryDatumEmbed \
    --tx-out "$scriptAddr + 1 $poolToken + 5000000 lovelace + 1000 $tokenAValue" \
    --tx-out-datum-embed-file $lpDatumEmbed \
    --tx-out "$changeAddress 957353706 lovelace + 70711 $liquidityCoinValue + 999000 17e86dfec8981df58e070430ce87fc7f51d5be7137cc0203ebac00c8.PikaCoin + 8944272 a4139e51066d2f0bcc852befdc106e469b9fa31ca607e81bf93ff21e.PikaCoinPikaCoin" \
    --mint "1 $poolToken + 70711 $liquidityCoinValue" \
    --mint-script-file $liquidityCurrencyPolicy \
    --mint-redeemer-file ~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/unipool-redeemer \
    --change-address $changeAddress \
    --protocol-params-file $protocolparams \
    --out-file $bodyFile

echo "transaction successfully built"

./cardano-cli/bin/cardano-cli transaction sign \
    --tx-body-file $bodyFile \
    --signing-key-file $skey \
    --testnet-magic 1097911063 \
    --out-file $outFile

echo "signed transaction and saved as $outFile"

./cardano-cli/bin/cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file $outFile

echo "submitted transaction"

