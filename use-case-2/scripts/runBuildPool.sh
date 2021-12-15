#!/usr/bin/env bash

set -euox pipefail

../../../../scripts/buildPool.sh "d6e4512bfbf398fe6f06f7d1935c01a09554a6537a647479e9d2f4329b70f055#0" \
  "d6e4512bfbf398fe6f06f7d1935c01a09554a6537a647479e9d2f4329b70f055#1" \
  "1d7b50a9eeeb035ab3be1d956ffdd0a588043999c2d665214fd602595d78629c#1" \
  ~/Documents/Obsidian/bobTheBuilder9/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/uniswapPlutusScript.plutus \
  "addr_test1wpnjsv28crcw4908vm9ueeag0lczjdgdadmzz292yyym4esvgefz7" \
  "51be6a676f95a36abd0259f947c350761c8a022ef0f640d7b7ed110e.Uniswap" \
  "759f497b1fefcb4683bf7e6389d687aeee5dbcfadd2529143fd6d016.PoolState" \
  ~/Documents/Obsidian/bobTheBuilder9/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/factoryDatum.plutus \
  ~/Documents/Obsidian/bobTheBuilder9/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/poolDatum.plutus \
  "addr_test1vqkvkzx4ey6lalfvdrd7xe36w6fa7hhwvtgge0367392aqse0hc38" \
  ~/Documents/Obsidian/bobTheBuilder9/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/liquidityCurrencyPolicy.plutus \
  ~/Documents/Obsidian/bobTheBuilder9/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/poolDatum.empty.plutus \
  ~/Documents/Obsidian/bobTheBuilder9/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/unipool-redeemer \
  ~/Documents/Obsidian/bobTheBuilder9/plutus-use-cases/use-case-2/dep/cardano-node/result/alonzo-testnet/keys/payment.skey \
  ~/Documents/Obsidian/bobTheBuilder9/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/factoryDatum.hash \
  ~/Documents/Obsidian/bobTheBuilder9/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/poolDatum.hash \
  "bfd1fcd53fb65accd03becbd05526583b522de43471ae8258a08b394.OBSIDIAN" \
  "759f497b1fefcb4683bf7e6389d687aeee5dbcfadd2529143fd6d016.OBSIDIANOBSIDIAN" \
  900000000 \
  314150 \
  16814726 \

