#!/usr/bin/env bash

# ./handleSwap.sh [TXHASH#TXIX] [UNISWAP SCRIPT FILE] [UNIPOOL DATUM FILE] [REDEEMER FILE] [COLLATERAL ADDRESS] [UNISWAP SCRIPT ADDRESS] [COIN TO BE SWAPPED - CURRENCYSYMBOL.TOKENNAME] [SKEY FILE] [ADDITIONAL FUNDS - TXHASH#TXIX] [POOL UTXO - TXHASH#TXIX] [POOL STATE CURRENCYSYMBOL.TOKENNAME]

set -euox pipefail

scriptFile=$2
uniPoolDatumFile=$3
redeemerUniswapAction=$4
collateral=$5
scriptAddr=$6
swappedCoin=$7
protocolparams="./rawSwap-protocolparams"
bodyFile=rawSwap-tx-body
changeAddress=$8
skey=$9
outFile="./rawSwap-tx-signed"
additionalFunds=${10}
poolUtxo=${11}
poolStateCurrencySymbol=${12}

./cardano-cli/bin/cardano-cli query protocol-parameters --testnet-magic 8 > $protocolparams

./cardano-cli/bin/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 8 \
    --tx-in $1 \
    --tx-in $additionalFunds \
    --tx-in $poolUtxo \
    --tx-in-script-file $scriptFile \
    --tx-in-datum-file $uniPoolDatumFile \
    --tx-in-redeemer-file $redeemerUniswapAction \
    --tx-in-collateral $collateral \
    --tx-out "$scriptAddr + 1930992 lovelace + 1 $poolStateCurrencySymbol + 100100 $swappedCoin" \
    --tx-out-datum-embed-file $uniPoolDatumFile \
    --tx-out "$changeAddress + 1373477 lovelace + 399900 $swappedCoin" \
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

