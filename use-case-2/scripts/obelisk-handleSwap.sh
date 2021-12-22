#!/usr/bin/env bash

# ./handleSwap.sh [TXHASH#TXIX] [UNISWAP SCRIPT FILE] [UNIPOOL DATUM FILE] [REDEEMER FILE] [COLLATERAL ADDRESS] [UNISWAP SCRIPT ADDRESS] [COIN TO BE SWAPPED - CURRENCYSYMBOL.TOKENNAME] [SKEY FILE] [ADDITIONAL FUNDS - TXHASH#TXIX] [POOL UTXO - TXHASH#TXIX] [POOL STATE CURRENCYSYMBOL.TOKENNAME] [NAMI WALLET CLIENT'S EXPECTED TXOUT] [UNISWAP EXPECTED TXOUT] [NAMI Wallet Address]

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
additionalFunds=${10}
poolUtxo=${11}
poolStateCurrencySymbol=${12}
clientTxOutBalance=${13}
uniswapTxOutBalance=${14}
requesterAddress=${15}
outFile="./$requesterAddress-obelisk-rawSwap-tx-signed"

/home/zigpolymath/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/cardano-node/result/alonzo-testnet/cardano-cli/bin/cardano-cli query protocol-parameters --testnet-magic 1097911063 > $protocolparams

/home/zigpolymath/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/cardano-node/result/alonzo-testnet/cardano-cli/bin/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --tx-in $1 \
    --tx-in $poolUtxo \
    --tx-in-script-file $scriptFile \
    --tx-in-datum-file $uniPoolDatumFile \
    --tx-in-redeemer-file $redeemerUniswapAction \
    --tx-in-collateral $collateral \
    --tx-out "$scriptAddr + 1 $poolStateCurrencySymbol + $uniswapTxOutBalance" \
    --tx-out-datum-embed-file $uniPoolDatumFile \
    --tx-out "$changeAddress $clientTxOutBalance" \
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
