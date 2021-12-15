#!/usr/bin/env bash

# ./buildPool.sh [TXHASH#TXIX] [TOKEN B TXHASH#TXIX] [UNISWAP TOKEN TXHASH#TXIX] [SCRIPT FILE] [UNISWAP SCRIPT ADDRESS] [UNISWAP TOKEN CURRENCY SYMBOL] [POOL TOKEN CURRENCY SYMBOL] [FACTORY DATUM EMBED FILE] [LIQUIDITY POOL DATUM EMBED FILE] [CHANGE ADDRESS] [LIQUIDITY CURRENCY POLICY] [EMPTY UNIPOOL DATUM FILE PATH] [UNISWAP ACTION REDEEMER FILE(unipool-redeemer)] [SKEY FILE] [FACTORY DATUM HASH FILE(factoryDatum.hash)] [POOL DATUM HASH FILE(poolDatum.hash)] [TOKEN B CURRENCYSYMBOL.TOKENNAME] [LIQUIDITY TOKEN CURRENCYSYMBOL.TOKENNAME] [TOKEN A AMOUNT] [TOKEN B AMOUNT] [LIQUIDITY POOL AMOUNT]

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
liquidityCoinValue=${18}
skey=${14}
outFile="./buildPool-tx-signed"
tokenAValue=${17}
adaPoolAmount=${19}
tokenAPoolAmount=${20}
liquidityAmount=${21}
./cardano-cli/bin/cardano-cli query protocol-parameters --testnet-magic 1097911063 > $protocolparams

echo "queried and set protocolparams $protocolparams"

./cardano-cli/bin/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --tx-in $1 \
    --tx-in $2 \
    --tx-in $3 \
    --tx-in-script-file $scriptFile \
    --tx-in-datum-file $uniPoolDatumFile \
    --tx-in-redeemer-file $redeemerUniswapAction \
    --tx-in-collateral $1 \
    --tx-out "$scriptAddr + 1 $uniswapToken" \
    --tx-out-datum-embed-file $factoryDatumEmbed \
    --tx-out "$scriptAddr + 1 $poolToken + $adaPoolAmount lovelace + $tokenAPoolAmount $tokenAValue" \
    --tx-out-datum-embed-file $lpDatumEmbed \
    --tx-out "$changeAddress + $liquidityAmount $liquidityCoinValue" \
    --mint "1 $poolToken + $liquidityAmount $liquidityCoinValue" \
    --mint-script-file $liquidityCurrencyPolicy \
    --mint-redeemer-file ${13} \
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

