#!/usr/bin/env bash

ob thunk unpack dep/plutus-starter
cd dep/plutus-starter
nix-shell --run "cabal new-repl exe:plutus-starter-pab"
