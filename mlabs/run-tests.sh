#!/usr/bin/env bash
set -euo pipefail

nix build -L .#mlabs-plutus-use-cases:test:mlabs-plutus-use-cases-tests \
             .#mlabs-plutus-use-cases:exe:deploy-app                    \
             .#mlabs-plutus-use-cases:exe:governance-demo               \
             .#mlabs-plutus-use-cases:exe:lendex-demo                   \
             .#mlabs-plutus-use-cases:exe:mlabs-plutus-use-cases        \
             .#mlabs-plutus-use-cases:exe:nft-demo                      \
             .#mlabs-plutus-use-cases:exe:nft-marketplace
