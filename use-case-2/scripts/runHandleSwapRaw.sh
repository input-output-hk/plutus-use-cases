#!/usr/bin/env bash

set -euox pipefail

../../../../scripts/handleSwapRaw.sh "a323bab8336a5ae89109efff139e6237fd3df36b2eb73ce64c4198d386e4b94b#0" \
~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/uniswapPlutusScript.plutus \
~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/firstRawSwap/poolDatum.plutus \
~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/rawSwap/rawSwap-redeemer \
"a323bab8336a5ae89109efff139e6237fd3df36b2eb73ce64c4198d386e4b94b#3" \
"addr_test1wqwtftz4r7ly8e8qkkt9h4dhjmhe5zpq4cmx4tdepsqx7psx88h7z" \
"lovelace" \
"addr_test1qqpmfwqxj8w0x2skzlklgpfn27p0mqeasr022edvhgzntpqw3y7rfu37ta7m6uds7yhp8n94dgkfc035wm6tgmpgtszsmqe955" \
./keys/payment.skey \
"a323bab8336a5ae89109efff139e6237fd3df36b2eb73ce64c4198d386e4b94b#3" \
"3fe5e587c36a29b6151154fd6cf3b0226d172984196c9bf8350d8962e7aa5710#1" \
"75a70a2a2e897886839a7a30935c2aa36a27406c5e67d21d8f749df5.PoolState" \

