#!/usr/bin/env bash

set -euox pipefail

../../../../scripts/handleSwap.sh "320f9525cee2d8e489e8063128cddac8d1b4fafe88a5cfe9beae3cb34d943228#2" \
~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/uniswapPlutusScript.plutus \
~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/rawSwap/poolDatum.plutus \
~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/rawSwap/rawSwap-redeemer \
"320f9525cee2d8e489e8063128cddac8d1b4fafe88a5cfe9beae3cb34d943228#0" \
"addr_test1wpr9r6r6q0wgydckagwh52kvw2fxwq3mc3mpcym7l6l9lzqgvz4f8" \
"e41bbd4c8c419c825945d05499ba41cc53181b44b8ac056d24dbdb42.PikaCoin" \
"addr_test1qrlt4547kcveetpcrqfnwy2m6twsh2lwncsyt60c4aeflwljl2wj5av3e50fr80j5qa8gg7v07caf0s7c8xwp7we6rks5lmf57" \
~/Documents/Obsidian/IOHK/cardano-node/result/alonzo-purple/payment.skey \

