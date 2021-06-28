#!/usr/bin/env bash

set -euo pipefail

nix-build -A plutus-starter.haskellNixProject.hsPkgs.plutus-starter.components.exes.plutus-starter-pab
./result/bin/plutus-starter-pab
