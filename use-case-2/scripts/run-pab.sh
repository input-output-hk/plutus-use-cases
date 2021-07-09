#!/usr/bin/env bash

set -euo pipefail

DIR=$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)

"$(nix-build "$DIR"/.. -A plutus-starter.haskellNixProject.hsPkgs.plutus-starter.components.exes.plutus-starter-pab --no-out-link)"/bin/plutus-starter-pab
