#!/usr/bin/env bash

# ./handleSwap.sh [TXHASH#TXIX] [UNISWAP SCRIPT FILE] [UNIPOOL DATUM FILE] [REDEEMER FILE] [COLLATERAL ADDRESS] [UNISWAP SCRIPT ADDRESS] [COIN TO BE SWAPPED - CURRENCYSYMBOL.TOKENNAME] [SKEY FILE] [ADDITIONAL FUNDS - TXHASH#TXIX] [POOL UTXO - TXHASH#TXIX] [POOL STATE CURRENCYSYMBOL.TOKENNAME]

set -euox pipefail

scriptFile=$2
uniPoolDatumFile=$3
redeemerUniswapAction=$4
collateral=$5
scriptAddr=$6
swappedCoin=$7
protocolparams="./build-raw-swap-protocolparams"
bodyFile=build-raw-swap-tx-body
changeAddress=$8
skey=$9
outFile="./build-raw-swap-tx"
additionalFunds=${10}
poolUtxo=${11}
poolStateCurrencySymbol=${12}

./cardano-cli/bin/cardano-cli query protocol-parameters --testnet-magic 1097911063 > $protocolparams

./cardano-cli/bin/cardano-cli transaction build-raw \
    --alonzo-era \
    --tx-in "1d7a5f1a4e1b47fb336a7907d7d94c1e252f0e6334697e046c0113653302ff76#0" \
    --tx-in $1 \
    --tx-in $additionalFunds \
    --tx-in $poolUtxo \
    --tx-in-execution-units "(1,1)" \
    --tx-in-script-file $scriptFile \
    --tx-in-datum-file $uniPoolDatumFile \
    --tx-in-redeemer-file $redeemerUniswapAction \
    --tx-in-collateral $collateral \
    --tx-out "$scriptAddr + 7000000 lovelace + 1 $poolStateCurrencySymbol  + 716 17e86dfec8981df58e070430ce87fc7f51d5be7137cc0203ebac00c8.PikaCoin" \
    --tx-out-datum-embed-file $uniPoolDatumFile \
    --tx-out "$changeAddress + 958811813 lovelace + 999118 17e86dfec8981df58e070430ce87fc7f51d5be7137cc0203ebac00c8.PikaCoin + 70711 75a70a2a2e897886839a7a30935c2aa36a27406c5e67d21d8f749df5.PikaCoinPikaCoin + 8944272 a4139e51066d2f0bcc852befdc106e469b9fa31ca607e81bf93ff21e.PikaCoinPikaCoin" \
    --fee 10 \
    --protocol-params-file $protocolparams \
    --out-file $bodyFile

echo 'Swap transaction built'

./cardano-cli/bin/cardano-cli transaction sign \
    --tx-body-file $bodyFile \
    --testnet-magic 1097911063 \
    --out-file $outFile

echo "signed transaction and saved as $outFile"
#
#./cardano-cli/bin/cardano-cli transaction submit \
#    --testnet-magic 1097911063 \
#    --tx-file $outFile
#
#echo "submitted transaction"
#
