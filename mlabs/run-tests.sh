#!/usr/bin/env bash
set -euo pipefail

system=$(nix eval --impure --expr builtins.currentSystem)

nix run -L .#mlabs-plutus-use-cases:test:mlabs-plutus-use-cases-tests
nix build -L .#check."$system"
