#!/usr/bin/env bash

# ./buildPool.sh [TXHASH#TXIX] [TOKEN A TXHASH#TXIX] [UNISWAP TOKEN TXHASH#TXIX] [SCRIPT FILE] [UNISWAP SCRIPT ADDRESS] [UNISWAP TOKEN CURRENCY SYMBOL] [POOL TOKEN CURRENCY SYMBOL] [FACTORY DATUM HASH] [LIQUIDITY POOL DATUM HASH] [CHANGE ADDRESS] [LIQUIDITY CURRENCY POLICY] [UNIPOOL DATUM FILE] [UNISWAP ACTION REDEEMER FILE] [SKEY FILE]

set -euox pipefail

uniswapToken="1 $6"
poolToken="1 $7"
scriptFile=$4
scriptAddr=$5
factoryDatumHash=$8
lpDatumHash=$9
changeAddress=${10}
protocolparams="./buildPool-protocolparams"
bodyFile=buildPool-tx-body
liquidityCurrencyPolicy=${11}
uniPoolDatumFile=${12}
redeemerUniswapAction=${13}
skey=${14}
outFile="./buildPool-tx-signed"
poolStateValue="1 f3a215c69aa5f38c84d90022d43d4294ed854185416bfbfc72d258d6.PoolState"
liquidityCoinValue="707107 f3a215c69aa5f38c84d90022d43d4294ed854185416bfbfc72d258d6.PikaCoinPikaCoin"
changeValue="5000000 lovelace + 500000 e41bbd4c8c419c825945d05499ba41cc53181b44b8ac056d24dbdb42.PikaCoin + 707107 f3a215c69aa5f38c84d90022d43d4294ed854185416bfbfc72d258d6.PikaCoinPikaCoin"

./cardano-cli/bin/cardano-cli query protocol-parameters --testnet-magic 8 > $protocolparams

echo "queried and set protocolparams $protocolparams"

./cardano-cli/bin/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 8 \
    --tx-in "9e5fbac1f9fd998f31e023643d886679261f8f5c38208a07d6551c1dc5b85998#0" \
    --tx-in $1 \
    --tx-in $2 \
    --tx-in $3 \
    --tx-in-script-file $scriptFile \
    --tx-in-datum-file $uniPoolDatumFile \
    --tx-in-redeemer-file $redeemerUniswapAction \
    --tx-in-collateral $1 \
    --tx-out "$scriptAddr + 2034438 lovelace + $uniswapToken" \
    --tx-out-datum-hash $(cat $factoryDatumHash) \
    --tx-out "$scriptAddr + 2034438 lovelace + $poolToken" \
    --tx-out-datum-hash $(cat $lpDatumHash) \
    --tx-out "addr_test1qrlt4547kcveetpcrqfnwy2m6twsh2lwncsyt60c4aeflwljl2wj5av3e50fr80j5qa8gg7v07caf0s7c8xwp7we6rks5lmf57 + $changeValue" \
    --mint "$poolStateValue + $liquidityCoinValue" \
    --mint-script-file $liquidityCurrencyPolicy \
    --mint-redeemer-value [] \
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

