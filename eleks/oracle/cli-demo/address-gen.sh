#!/bin/bash
set -e

source base.sh
dir=$(dirname "$0")
baseFolder=$dir/keys

function main() {
    setEnv
    generateAddress oracle

    generateAddress client
}

function generateAddress() {

    folder=$baseFolder/$1
    mkdir -p $folder
    cardano-cli address key-gen \
     --extended-key \
    --verification-key-file $folder/payment.vkey \
    --signing-key-file $folder/payment.skey

    cardano-cli stake-address key-gen \
    --verification-key-file $folder/stake.vkey \
    --signing-key-file $folder/stake.skey

    cardano-cli address build \
    --payment-verification-key-file $folder/payment.vkey \
    --stake-verification-key-file $folder/stake.vkey \
    --out-file $folder/payment.addr \
    --testnet-magic $TESTNET_MAGIC

    cat $folder/payment.addr
}

main


#    cardano-cli address key-gen \
#      --extended-key \
#     --verification-key-file payment.vkey \
#     --signing-key-file payment.skey