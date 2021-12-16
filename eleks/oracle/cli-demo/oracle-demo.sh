#!/bin/bash
dir=$(dirname "$0")
set -e
source $dir/base.sh

function main() {
    rm $dir/oracle.plutus || true
    rm $dir/requesttoken.plutus || true
    rm -rf $dir/datum || true
    mkdir $dir/datum
    cabal build gs
    cabal build encode-oracle-request 
    cabal build verify-datum
    #set env after build
    setEnv

    WINNER_ID=20
    CLEINT_KEY_PATH="$dir/keys/client"
    CLIENT_ADDRESS=$(cat $CLEINT_KEY_PATH/payment.addr)
    CLIENT_SIGN_KEY="$CLEINT_KEY_PATH/payment.skey"
    CLIENT_VER_KEY="$CLEINT_KEY_PATH/payment.vkey"
    ORACLE_OWNER_KEY_PATH="$dir/keys/oracle"
    ORACLE_OWNER_ADDRESS=$(cat $ORACLE_OWNER_KEY_PATH/payment.addr)
    ORACLE_SIGN_KEY="$ORACLE_OWNER_KEY_PATH/payment.skey"
    ORACLE_VER_KEY="$ORACLE_OWNER_KEY_PATH/payment.vkey"

    GAME_ID=13
    cabal exec -- gs $ORACLE_FEE $ORACLE_COLLATERAL $ORACLE_VER_KEY
    makeScriptAddress

    echo '***** request oracle token'
    requestDatumFile="$dir/datum/oracle-request.json"
    cabal exec -- encode-oracle-request $GAME_ID "$CLIENT_VER_KEY" | jq 'fromjson' > $requestDatumFile
    mintRequestToken $CLIENT_ADDRESS "$CLIENT_SIGN_KEY" $ORACLE_OWNER_ADDRESS $requestDatumFile
    waitUtxoForDatum $requestDatumFile
   
    echo '***** request game state to live'
    updateLiveDatumFile="$dir/datum/oracle-update-live.json"
    cabal exec -- encode-oracle-request $GAME_ID "$CLIENT_VER_KEY" $ORACLE_SIGN_KEY 0 "\"LIVE\"" | jq 'fromjson' > $updateLiveDatumFile
    oracleUpdate $ORACLE_OWNER_ADDRESS $ORACLE_SIGN_KEY $requestDatumFile $updateLiveDatumFile
    waitUtxoForDatum $updateLiveDatumFile

    echo '***** request game state to finished'
    updateFinishGameDatumFile="$dir/datum/oracle-update-finishgame.json"
    cabal exec -- encode-oracle-request $GAME_ID $CLIENT_VER_KEY $ORACLE_SIGN_KEY 55 "\"FT\"" | jq 'fromjson' > $updateFinishGameDatumFile
    oracleUpdate $ORACLE_OWNER_ADDRESS $ORACLE_SIGN_KEY $updateLiveDatumFile $updateFinishGameDatumFile
    waitUtxoForDatum $updateFinishGameDatumFile

    echo '***** redeem the oracle token'
    redeemRequestToken $CLIENT_ADDRESS $CLIENT_SIGN_KEY $updateFinishGameDatumFile

    echo '***** verify oracle token onchain'
    queryDatum $updateFinishGameDatumFile
}

function queryDatum() {
    echo 'get datum'
    datumFile=$1
    datumHash=$(cardano-cli transaction hash-script-data --script-data-file $datumFile)
    datum=$(curl --location --request POST 'localhost:9083/from-hash/datum' \
        -H 'Content-Type: application/json' \
        -d "\"$datumHash\"")
    echo $datum
    cabal exec -- verify-datum $datum $ORACLE_VER_KEY
}
function mintRequestToken() {
    paymentAddress=$1
    paymentKey=$2
    oracleOwnerAddress=$3
    datumFile=$4

    updatePayUtxo $paymentAddress

    datumHash=$(cardano-cli transaction hash-script-data --script-data-file $datumFile)
    cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic $TESTNET_MAGIC \
    --tx-in $PAY_TX_HASH#$PAY_TX_IX \
    --change-address $paymentAddress \
    --mint-script-file $MINT_SCRIPT_FILE \
    --mint="1 $POLICY_ID.$ORACLE_TOKEN_NAME" \
    --mint-redeemer-file "$dir/redeemers/oracle-mint-request.json" \
    --out-file $TX_RAW_PATH \
    --protocol-params-file $PROTOCOL_PATH \
    --tx-in-collateral $PAY_TX_HASH#$PAY_TX_IX \
    --tx-out $oracleOwnerAddress+$ORACLE_FEE\
    --tx-out $ORACLE_SCRIPT_ADDRESS+"1 $POLICY_ID.$ORACLE_TOKEN_NAME"+$ORACLE_COLLATERAL \
    --tx-out-datum-embed-file $datumFile \

    sendTransaction $paymentKey
}



function oracleUpdate() {
    oracleOwnerAddress=$1
    oracleOwnerKey=$2
    inDatumFile=$3
    outDatumFile=$4
    updatePayUtxo $oracleOwnerAddress
    outDatumHash=$(cardano-cli transaction hash-script-data --script-data-file $outDatumFile)

    requestUtxo=$(findUtxoByDatum $inDatumFile)

    echo "requestUtxo: $requestUtxo"

    cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic $TESTNET_MAGIC \
    --tx-in $PAY_TX_HASH#$PAY_TX_IX \
    --tx-in $requestUtxo \
    --tx-in-script-file $ORACLE_SCRIPT_FILE \
    --tx-in-redeemer-file "$dir/redeemers/oracle-update.json" \
    --tx-in-datum-file "$inDatumFile" \
    --change-address $oracleOwnerAddress \
    --out-file $TX_RAW_PATH \
    --protocol-params-file $PROTOCOL_PATH \
    --tx-in-collateral $PAY_TX_HASH#$PAY_TX_IX \
    --required-signer $oracleOwnerKey\
    --tx-out $ORACLE_SCRIPT_ADDRESS+"1 $POLICY_ID.$ORACLE_TOKEN_NAME"+$ORACLE_COLLATERAL \
    --tx-out-datum-embed-file $outDatumFile
 set +x
    sendTransaction $oracleOwnerKey
}

sendTransaction() {
    signKey=$1
    cardano-cli transaction sign --tx-body-file $TX_RAW_PATH --signing-key-file $signKey --testnet-magic $TESTNET_MAGIC --out-file $TX_SIGN_PATH
    cardano-cli transaction submit --testnet-magic $TESTNET_MAGIC --tx-file $TX_SIGN_PATH
}

function redeemRequestToken() {
    requestOwnerAddress=$1
    requestOwnerKey=$2
    inDatumFile=$3
    updatePayUtxo $requestOwnerAddress
    echo uraaaa
    requestUtxo=$(findUtxoByDatum $inDatumFile)

    cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic $TESTNET_MAGIC \
    --tx-in $PAY_TX_HASH#$PAY_TX_IX \
    --tx-in $requestUtxo \
    --tx-in-script-file $ORACLE_SCRIPT_FILE \
    --tx-in-redeemer-file "$dir/redeemers/oracle-redeem.json" \
    --tx-in-datum-file "$inDatumFile"  \
    --change-address $requestOwnerAddress \
    --mint-script-file $MINT_SCRIPT_FILE \
    --mint="-1 $POLICY_ID.$ORACLE_TOKEN_NAME" \
    --mint-redeemer-file "$dir/redeemers/oracle-mint-redeem.json" \
    --out-file $TX_RAW_PATH \
    --protocol-params-file $PROTOCOL_PATH \
    --tx-in-collateral $PAY_TX_HASH#$PAY_TX_IX \
    --required-signer $requestOwnerKey \
    --tx-out $requestOwnerAddress+$ORACLE_COLLATERAL
 
    sendTransaction $requestOwnerKey
}

main