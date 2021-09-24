#!/usr/bin/env bash

set -euo pipefail

DIR=$(cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)

if netstat -nlt | awk '{print $4}' | grep -q ':8080' ; then
    echo "Port 8080 is in use; please stop any process using port 8080 and then try again"
    exit 1
fi

"$(nix-build "$DIR"/.. -A plutus-starter.haskellNixProject.hsPkgs.plutus-starter.components.exes.plutus-starter-pab --no-out-link)"/bin/plutus-starter-pab
