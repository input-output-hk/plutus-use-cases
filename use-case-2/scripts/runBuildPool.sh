#!/usr/bin/env bash

set -euox pipefail

../../../../scripts/buildPool.sh "f9ed5a110984ef91a499021d4ecf9f52ed39f424b3795439820374c6af9cb311#0" \
  "5017f03797fbedfbe648325bc000aea08477b2b3afe6027eb76ab8140f928770#3" \
  "f9ed5a110984ef91a499021d4ecf9f52ed39f424b3795439820374c6af9cb311#1" \
  ~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/uniswapPlutusScript.plutus \
  "addr_test1wqwtftz4r7ly8e8qkkt9h4dhjmhe5zpq4cmx4tdepsqx7psx88h7z" \
  "d6298a9d6b5f7b66258377577927cd61a97a0439bc41079bee6d7b20.Uniswap" \
  "75a70a2a2e897886839a7a30935c2aa36a27406c5e67d21d8f749df5.PoolState" \
  ~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/factoryDatum.plutus \
  ~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/poolDatum.plutus \
  "addr_test1vqkvkzx4ey6lalfvdrd7xe36w6fa7hhwvtgge0367392aqse0hc38" \
  ~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/liquidityCurrencyPolicy.plutus \
  ~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/poolDatum.empty.plutus \
  ~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/unipool-redeemer \
  ~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/cardano-node/result/alonzo-testnet/keys/payment.skey \
  ~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/factoryDatum.hash \
  ~/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/poolDatum.hash \

