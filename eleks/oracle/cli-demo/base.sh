#!/bin/bash
dir=$(dirname "$0")


function setEnv() {
  export TESTNET_MAGIC=1097911063
  export ORACLE_FEE=1500000
  export ORACLE_COLLATERAL=1000000
  export MIN_UTXO=2000000
  export ORACLE_TOKEN_NAME="oracleRequestTokenName"
  export CARDANO_NODE_SOCKET_PATH=$dir/node/db/node.socket
  export PROTOCOL_PATH=$dir/protocol.json
  export TX_RAW_PATH=$dir/tx.raw
  export TX_SIGN_PATH=$dir/tx.sign
  cardano-cli query protocol-parameters --testnet-magic $TESTNET_MAGIC --out-file $PROTOCOL_PATH

  makeScriptAddress
} 

function makeDatumBytes() {
    export DATUM_VALUE=$1 \
    && export DATUM_JSON=$(printf "{\"bytes\":\"%s\"}" $(echo -n $DATUM_VALUE | xxd -ps)) \
    && echo "$DATUM_JSON" > script-datum.json \
    && echo $DATUM_JSON \
    && DATUM_HASH="$(cardano-cli transaction hash-script-data --script-data-file script-datum.json)" \
    && export DATUM_HASH=${DATUM_HASH:0:64} \
    && echo "DATUM_HASH=\"$DATUM_HASH\""
}

function updatePayUtxo() {
  paymentAddress=$1
  echo "Payment Info******"
  payUtxo=$(cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address $paymentAddress )

  #cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address $paymentAddress 
  payUtxoIndex=$(echo "$payUtxo" | awk -v max=0 '{if(NR >= 2 && $3>max){rowNum=NR; max=$3}}END{print rowNum}')
  ##
  export PAY_TX_HASH=$(echo "$payUtxo" | awk -v nr=$payUtxoIndex "NR==nr" | awk '{print $1}')
  echo "PAY_TX_HASH $PAY_TX_HASH"
  export PAY_TX_IX=$(echo "$payUtxo" | awk -v nr=$payUtxoIndex "NR==nr" | awk '{print $2}')
  echo "PAY_TX_IX $PAY_TX_IX"
}

function makeScriptAddress() {
  SCRIPT_FILE_BASE=$dir/..
  export MINT_SCRIPT_FILE=$SCRIPT_FILE_BASE/requesttoken.plutus
  export ORACLE_SCRIPT_FILE=$SCRIPT_FILE_BASE/oracle.plutus
  export MINT_SCRIPT_ADDRESS=$(cardano-cli address build --payment-script-file $MINT_SCRIPT_FILE --testnet-magic $TESTNET_MAGIC)
  echo "MINT_SCRIPT_ADDRESS $MINT_SCRIPT_ADDRESS"

  export ORACLE_SCRIPT_ADDRESS=$(cardano-cli address build --payment-script-file $ORACLE_SCRIPT_FILE --testnet-magic $TESTNET_MAGIC)
  echo "ORACLE_SCRIPT_ADDRESS $ORACLE_SCRIPT_ADDRESS"

  export POLICY_ID=$(cardano-cli transaction policyid --script-file $MINT_SCRIPT_FILE)
  echo "POLICY_ID $POLICY_ID"
}