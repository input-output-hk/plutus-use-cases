#!/usr/bin/env bash

set -euox pipefail

../../../../scripts/buildPool.sh "2c7fb8188344ff3bfc3a0c86a883bb800861e8c5c3965a8f3ea0ea179ab1dae9#0" \
  "9e5fbac1f9fd998f31e023643d886679261f8f5c38208a07d6551c1dc5b85998#1" \
  "2c7fb8188344ff3bfc3a0c86a883bb800861e8c5c3965a8f3ea0ea179ab1dae9#1" \
  ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/uniswapPlutusScript.plutus \
  "addr_test1wrge3ffz7udrwndxc58afhzuau7lha5ylvx73whjjzczn9czczsnr" \
  "b3ac23c200b650f3a5780c9facbd185518a4cd8eb9dca7df08c6f348.Uniswap" \
  "f3a215c69aa5f38c84d90022d43d4294ed854185416bfbfc72d258d6.PoolState" \
  ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/factoryDatum.hash \
  ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/poolDatum.hash \
  "addr_test1qrlt4547kcveetpcrqfnwy2m6twsh2lwncsyt60c4aeflwljl2wj5av3e50fr80j5qa8gg7v07caf0s7c8xwp7we6rks5lmf57" \
  ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/liquidityCurrencyPolicy.plutus \
  ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/poolDatum.empty.plutus \
  ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/unipool-redeemer \
  ~/Documents/Obsidian/IOHK/cardano-node/result/alonzo-purple/payment.skey

