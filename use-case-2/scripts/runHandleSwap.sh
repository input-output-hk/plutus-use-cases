#!/usr/bin/env bash

set -euox pipefail

../../../../scripts/handleSwap.sh "215e22a938f5853ecfc4ab3cb49d25dedb2b0c1dc952dff283a9ad47519e8e01#1" \
~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/uniswapPlutusScript.plutus \
~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/firstRawSwap/poolDatum.plutus \
~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/rawSwap/rawSwap-redeemer \
"215e22a938f5853ecfc4ab3cb49d25dedb2b0c1dc952dff283a9ad47519e8e01#0" \
"addr_test1wqwtftz4r7ly8e8qkkt9h4dhjmhe5zpq4cmx4tdepsqx7psx88h7z" \
"lovelace" \
"addr_test1qq9cvk55uqlxtv5wnyyum7luvr0p549g3mu3eqtj6ah7xvlh6gjurme664fs433l4egfnlplfq4edwkfnlsl3hr8c9fs7g2n38" \
./keys/payment.skey \
"555d92eb7e540ffc4d9b6a80becce5429737a5787c54d0f28c8ecb5f9d174774#1" \
"3fe5e587c36a29b6151154fd6cf3b0226d172984196c9bf8350d8962e7aa5710#1" \
"75a70a2a2e897886839a7a30935c2aa36a27406c5e67d21d8f749df5.PoolState" \

