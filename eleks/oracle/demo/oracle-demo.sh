#!/bin/bash
dir=$(dirname "$0")
source $dir/base.sh

function main() {
    setEnv

    PAYMENT_ADDRESS=$(cat $dir/keys/client/payment.addr)
    PAYMENT_SIGN_KEY="$dir/keys/client/payment.skey"
    ORACLE_OWNER=$(cat $dir/keys/oracle/payment.addr)

    echo 'mint token'
    mintRequestToken $PAYMENT_ADDRESS $PAYMENT_SIGN_KEY $ORACLE_OWNER

    #todo bette way to wait for transaction completion
    sleep 15
    echo 'redeem'
    redeemRequestToken $PAYMENT_ADDRESS $PAYMENT_SIGN_KEY
}

function mintRequestToken() {
    paymentAddress=$1
    paymentKey=$2
    oracleOwnerAddress=$3

    updatePayUtxo $paymentAddress
    datumHash=$(cardano-cli transaction hash-script-data --script-data-file $dir/datum/oracle-request.json)
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
    --tx-out $oracleOwnerAddress+$ORACLE_FEE+$ORACLE_COLLATERAL \
    --tx-out $ORACLE_SCRIPT_ADDRESS+$MIN_UTXO+"1 $POLICY_ID.$ORACLE_TOKEN_NAME" \
    --tx-out-datum-hash $datumHash

    sendTransaction $paymentKey
}

function redeemRequestToken() {
    paymentAddress=$1
    paymentKey=$2
    updatePayUtxo $paymentAddress
    expectedHash=$(cardano-cli transaction hash-script-data --script-data-file $dir/datum/oracle-request.json)
    set -x
    oracleUtxo=$(cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address "$ORACLE_SCRIPT_ADDRESS" | grep $expectedHash )
    set +x
    if [ -z "$oracleUtxo" ]; then
        echo "oracle utxo not found"
        exit 0
    fi
    utxoHash=$(echo $oracleUtxo | awk '{print $1}')
    utxoIx=$(echo $oracleUtxo | awk '{print $2}')
    echo utxoHash $utxoHash
    echo utxoIx $utxoIx
    cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic $TESTNET_MAGIC \
    --tx-in $PAY_TX_HASH#$PAY_TX_IX \
    --tx-in $utxoHash#$utxoIx \
    --tx-in-script-file $ORACLE_SCRIPT_FILE \
    --tx-in-redeemer-file "$dir/redeemers/oracle-redeem.json" \
    --tx-in-datum-file "$dir/datum/oracle-request.json" \
    --change-address $paymentAddress \
    --mint-script-file $MINT_SCRIPT_FILE \
    --mint="-1 $POLICY_ID.$ORACLE_TOKEN_NAME" \
    --mint-redeemer-file "$dir/redeemers/oracle-mint-redeem.json" \
    --out-file $TX_RAW_PATH \
    --protocol-params-file $PROTOCOL_PATH \
    --tx-in-collateral $PAY_TX_HASH#$PAY_TX_IX
 
    sendTransaction $paymentKey
}

sendTransaction() {
    signKey=$1
    cardano-cli transaction sign --tx-body-file $TX_RAW_PATH --signing-key-file $signKey --testnet-magic $TESTNET_MAGIC --out-file $TX_SIGN_PATH
    cardano-cli transaction submit --testnet-magic $TESTNET_MAGIC --tx-file $TX_SIGN_PATH
}

main
