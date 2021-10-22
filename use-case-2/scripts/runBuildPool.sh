#!/usr/bin/env bash

set -euox pipefail

../../../../scripts/buildPool.sh "444d5cea4e19cb706a2f7634a54bd41e3a3817146b4ef193020f48655921db56#0" \
  "320f9525cee2d8e489e8063128cddac8d1b4fafe88a5cfe9beae3cb34d943228#1" \
  "444d5cea4e19cb706a2f7634a54bd41e3a3817146b4ef193020f48655921db56#1" \
  ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/uniswapPlutusScript.plutus \
  "addr_test1wpr9r6r6q0wgydckagwh52kvw2fxwq3mc3mpcym7l6l9lzqgvz4f8" \
  "eb7f3647710b4675b2f67f654329b78cd5ea37b65f010f584060c710.Uniswap" \
  "207875f104148d5f3bacb2601ce9ee519defbd276be5fe241d84107a.PoolState" \
  ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/factoryDatum.plutus \
  ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/poolDatum.plutus \
  "addr_test1qrlt4547kcveetpcrqfnwy2m6twsh2lwncsyt60c4aeflwljl2wj5av3e50fr80j5qa8gg7v07caf0s7c8xwp7we6rks5lmf57" \
  ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/liquidityCurrencyPolicy.plutus \
  ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/poolDatum.empty.plutus \
  ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/unipool-redeemer \
  ~/Documents/Obsidian/IOHK/cardano-node/result/alonzo-purple/payment.skey \
  ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/factoryDatum.hash \
  ~/Documents/Obsidian/bobTheBuilder5/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/unipool/poolDatum.hash \

