#!/bin/bash
dir=$(dirname "$0")


function setEnv() {
  export TESTNET_MAGIC=1097911063
  export ORACLE_FEE=2000000
  export ORACLE_COLLATERAL=3500000
  export MIN_UTXO=2000000 
  export ORACLE_TOKEN_NAME="6f72746b" #ortk in base16
  #export CARDANO_NODE_SOCKET_PATH=$dir/node/db/node.socket
  export CARDANO_NODE_SOCKET_PATH=/tmp/node.socket
  #export CARDANO_NODE_SOCKET_PATH=~/projects/plutus/plutus-apps/plutus-pab/test-node/testnet/node.sock

  export PROTOCOL_PATH=$dir/protocol.json
  export TX_RAW_PATH=$dir/tx.raw
  export TX_SIGN_PATH=$dir/tx.sign
  cardano-cli query protocol-parameters --testnet-magic $TESTNET_MAGIC --out-file $PROTOCOL_PATH
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
  export PAY_TX_IX=$(echo "$payUtxo" | awk -v nr=$payUtxoIndex "NR==nr" | awk '{print $2}')
}

function makeScriptAddress() {
  export MINT_SCRIPT_FILE=$dir/requesttoken.plutus
  export ORACLE_SCRIPT_FILE=$dir/oracle.plutus
  export MINT_SCRIPT_ADDRESS=$(cardano-cli address build --payment-script-file $MINT_SCRIPT_FILE --testnet-magic $TESTNET_MAGIC)
  echo "MINT_SCRIPT_ADDRESS $MINT_SCRIPT_ADDRESS"

  export ORACLE_SCRIPT_ADDRESS=$(cardano-cli address build --payment-script-file $ORACLE_SCRIPT_FILE --testnet-magic $TESTNET_MAGIC)
  echo "ORACLE_SCRIPT_ADDRESS $ORACLE_SCRIPT_ADDRESS"

  export POLICY_ID=$(cardano-cli transaction policyid --script-file $MINT_SCRIPT_FILE)
  echo "POLICY_ID $POLICY_ID"
}

findUtxoByDatum() {
    datumFile=$1
    expectedHash=$(cardano-cli transaction hash-script-data --script-data-file $datumFile)
    oracleRequestUtxo=$(cardano-cli query utxo --testnet-magic $TESTNET_MAGIC --address "$ORACLE_SCRIPT_ADDRESS" | grep $expectedHash || true } )
    if [ "$oracleRequestUtxo" ]; then
        utxoHash=$(echo $oracleRequestUtxo | awk '{print $1}')
        utxoIx=$(echo $oracleRequestUtxo | awk '{print $2}')
        echo "$utxoHash#$utxoIx"
    else
        echo ""
    fi 
}

waitUtxoForDatum() {
  echo 'wait for utxo'
  utxo=""
  while [ -z "$utxo" ];
  do 
    utxo=$(findUtxoByDatum $1);
    echo 'no utxo yet, sleep 5 seconds'
    sleep 10
  done
  echo "utxo found: $utxo __________"
}