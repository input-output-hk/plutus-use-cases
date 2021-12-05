#!/bin/bash
dir=$(dirname "$0")
set -e
source $dir/base.sh
echo $dir uraa
function main() {
    rm $dir/oracle.plutus || true
    rm $dir/requesttoken.plutus || true
    cabal build gs
    cabal build encode-oracle-request 
    #set env after build
    setEnv
    GAME_ID=1
    WINNER_ID=0
    CLEINT_KEY_PATH="$dir/keys/client"
    CLIENT_ADDRESS=$(cat $CLEINT_KEY_PATH/payment.addr)
    CLIENT_SIGN_KEY="$CLEINT_KEY_PATH/payment.skey"
    CLIENT_VER_KEY="$CLEINT_KEY_PATH/payment.vkey"
    ORACLE_OWNER_KEY_PATH="$dir/keys/oracle"
    ORACLE_OWNER_ADDRESS=$(cat $ORACLE_OWNER_KEY_PATH/payment.addr)
    ORACLE_SIGN_KEY="$ORACLE_OWNER_KEY_PATH/payment.skey"
    ORACLE_VER_KEY="$ORACLE_OWNER_KEY_PATH/payment.vkey"
    cabal exec -- gs $ORACLE_FEE $ORACLE_COLLATERAL $ORACLE_VER_KEY
    makeScriptAddress
    echo 'mint token'


    #cabal exec -- encode-oracle-request 1 $ORACLE_VER_KEY $CLIENT_SIGN_KEY 0 "\"LIVE\"" >  $dir/datum/oracle-update-live111.json
    #cabal exec -- encode-oracle-request $GAME_ID "$CLIENT_VER_KEY" > $dir/datum/oracle-request1.json
    mintRequestToken $CLIENT_ADDRESS "$CLIENT_SIGN_KEY" $ORACLE_OWNER_ADDRESS "$dir/datum/oracle-request.json"
    #sleep 10
    #cabal exec -- encode-oracle-request 1 $CLIENT_VER_KEY $ORACLE_SIGN_KEY 0 "\"LIVE\"" >  $dir/datum/oracle-update-live.json
    #oracleUpdate $ORACLE_OWNER_ADDRESS $ORACLE_SIGN_KEY "$dir/datum/oracle-request.json" "$dir/datum/oracle-update-live.json"

    #cabal exec -- encode-oracle-request 1 $CLIENT_VER_KEY $ORACLE_SIGN_KEY 55 "\"FT\"" > $dir/datum/oracle-update-finishgame.json
    #oracleUpdate $ORACLE_OWNER_ADDRESS $ORACLE_SIGN_KEY "$dir/datum/oracle-update-live.json" "$dir/datum/oracle-update-finishgame.json"
   

    #oracleUpdate $ORACLE_OWNER_ADDRESS $ORACLE_SIGN_KEY "$dir/datum/oracle-update-finishgame.json" "$dir/datum/oracle-update-finishgame.json"
   
   
   
    #redeemRequestToken $CLIENT_ADDRESS $CLIENT_SIGN_KEY "$dir/datum/oracle-update-finishgame.json"
    #redeemRequestToken $CLIENT_ADDRESS $CLIENT_SIGN_KEY "$dir/datum/oracle-update-live.json"
}

function mintRequestToken() {
    paymentAddress=$1
    paymentKey=$2
    oracleOwnerAddress=$3
    datumFile=$4

    updatePayUtxo $paymentAddress
    cabal exec -- encode-oracle-request 1 "$dir/keys/client/payment.vkey" > $dir/test.txt

    datumHash=$(cardano-cli transaction hash-script-data --script-data-file $datumFile)
    set -x
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

getRequestUtxo() {
    datumFile=$1
    expectedHash=$(cardano-cli transaction hash-script-data --script-data-file $datumFile)
    set -x
    oracleRequestUtxo=$(cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address "$ORACLE_SCRIPT_ADDRESS" | grep $expectedHash || true } )
    set +x
    if [ -z "$oracleRequestUtxo" ]; then
    set -x 
        echo "oracle request utxo not found"
        set +x
        exit 1
    fi
    utxoHash=$(echo $oracleRequestUtxo | awk '{print $1}')
    utxoIx=$(echo $oracleRequestUtxo | awk '{print $2}')
    echo "$utxoHash#$utxoIx"
}

function oracleUpdate() {
    oracleOwnerAddress=$1
    oracleOwnerKey=$2
    inDatumFile=$3
    outDatumFile=$4
    updatePayUtxo $oracleOwnerAddress
    outDatumHash=$(cardano-cli transaction hash-script-data --script-data-file $outDatumFile)
echo ura

    requestUtxo=$(getRequestUtxo $inDatumFile)

    echo "requestUtxo: $requestUtxo"

set -x
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
    requestUtxo=$(getRequestUtxo $inDatumFile)

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