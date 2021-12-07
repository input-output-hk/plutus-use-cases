#!/usr/bin/env bash

# ./handleSwap.sh [TXHASH#TXIX] [UNISWAP SCRIPT FILE] [UNIPOOL DATUM FILE] [REDEEMER FILE] [COLLATERAL ADDRESS] [UNISWAP SCRIPT ADDRESS] [COIN TO BE SWAPPED - CURRENCYSYMBOL.TOKENNAME] [SKEY FILE] [ADDITIONAL FUNDS - TXHASH#TXIX] [POOL UTXO - TXHASH#TXIX] [POOL STATE CURRENCYSYMBOL.TOKENNAME]

set -euox pipefail

scriptFile=$2
uniPoolDatumFile=$3
redeemerUniswapAction=$4
collateral=$5
scriptAddr=$6
swappedCoin=$7
protocolparams="./obelisk-rawSwap-protocolparams"
bodyFile=rawSwap-tx-body
changeAddress=$8
skey=$9
outFile="./obelisk-rawSwap-tx-signed"
additionalFunds=${10}
poolUtxo=${11}
poolStateCurrencySymbol=${12}

/home/zigpolymath/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/cardano-node/result/alonzo-testnet/cardano-cli/bin/cardano-cli query protocol-parameters --testnet-magic 1097911063 > $protocolparams

/home/zigpolymath/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/cardano-node/result/alonzo-testnet/cardano-cli/bin/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --tx-in $1 \
    --tx-in $additionalFunds \
    --tx-in $poolUtxo \
    --tx-in-script-file $scriptFile \
    --tx-in-datum-file $uniPoolDatumFile \
    --tx-in-redeemer-file $redeemerUniswapAction \
    --tx-in-collateral "555d92eb7e540ffc4d9b6a80becce5429737a5787c54d0f28c8ecb5f9d174774#0" \
    --tx-out "$scriptAddr + 7000000 lovelace + 1 $poolStateCurrencySymbol  + 716 17e86dfec8981df58e070430ce87fc7f51d5be7137cc0203ebac00c8.PikaCoin" \
    --tx-out-datum-embed-file $uniPoolDatumFile \
    --tx-out "$changeAddress + 1344798 lovelace + 118 17e86dfec8981df58e070430ce87fc7f51d5be7137cc0203ebac00c8.PikaCoin" \
    --change-address $changeAddress \
    --protocol-params-file $protocolparams \
    --out-file $bodyFile

echo 'Swap transaction built'

/home/zigpolymath/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/cardano-node/result/alonzo-testnet/cardano-cli/bin/cardano-cli transaction sign \
    --tx-body-file $bodyFile \
    --signing-key-file $skey \
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
