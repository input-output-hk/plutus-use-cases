#!/usr/bin/env bash

# ./mintTokenScript.sh [PAYMENT ADDRESS] [PAYMENT VKEY] [PAYMENT SKEY] [UTXO HASH] [UTXO HASH INDEX] [PATH TO SCRIPT] [TOKEN NAME] [TOKEN AMOUNT]

set -euo pipefail

export PATH=$(pwd)/cardano-cli/bin:$PATH
magic="--testnet-magic=1097911063"

workdir=$(pwd)/dumpdir
walletdir=$(pwd)

script=$6
tokenName=$7
tokenAmount=$8
scriptaddr=$workdir/issue.addr

mintingpolicyjson=$script 
paymentaddr=$1 #"addr_test1qrlt4547kcveetpcrqfnwy2m6twsh2lwncsyt60c4aeflwljl2wj5av3e50fr80j5qa8gg7v07caf0s7c8xwp7we6rks5lmf57"
paymentvkey=$2 # $walletdir/payment.vkey
paymentskey=$3 # $walletdir/payment.skey
redeemerFile=$(pwd)/redeemerScript.0

# set txhash & txix from utxo available @ $paymentaddr
txhash=$4 #bd59d826d94d3099ef61362cf974b1ab1226744bb476eceb4a78b8c510fe5843
txix=$5 #0
txmint=$workdir/txmint

echo "UTXOs @ $paymentaddr"
cardano-cli query utxo --address $paymentaddr $magic
cardano-cli address build --payment-script-file $script --out-file $scriptaddr $magic

echo "Script addr: $scriptaddr"
echo -e '\n\n'

# cat >$mintingpolicyjson <<EOL
# {
#   "keyHash": "$(cardano-cli address key-hash --payment-verification-key-file $paymentvkey)",
#   "type": "sig"
# }
# EOL

currencysymbol=$(cardano-cli transaction policyid --script-file $mintingpolicyjson)

amount=$tokenAmount
tokenList=""
tokens=($tokenName)
for i in "${!tokens[@]}"; do
  tokenList="${tokenList}"+"${amount} ${currencysymbol}.${tokens[i]}"
done
tokenList="${tokenList#+}"

echo "     Token List: $tokenList"
echo "Currency Symbol: $currencysymbol"
echo " Minting Policy: $(cat $mintingpolicyjson)"

protocolparams=$workdir/protocolparams
cardano-cli query protocol-parameters $magic > $protocolparams


cardano-cli transaction build \
    --alonzo-era \
		--protocol-params-file $protocolparams\
    --tx-in $txhash#$txix \
		--tx-in-collateral $txhash#$txix\
    --tx-out $paymentaddr+$tokenAmount+"$tokenList" \
    --mint "$tokenList" \
    --mint-script-file $mintingpolicyjson \
    --mint-redeemer-file $redeemerFile \
    --change-address $paymentaddr \
    $magic \
    --out-file $txmint.raw

cardano-cli transaction sign \
    --tx-body-file $txmint.raw \
    --signing-key-file $paymentskey \
    $magic \
    --out-file $txmint.sign

cat $txmint.raw
cat $txmint.sign
cardano-cli transaction submit $magic --tx-file $txmint.sign
