#!/bin/bash
set -e
dir=$(dirname "$0")

cabal exec plutus-chain-index -- --config $dir/chain-index-config.json --db-path $dir/chain-index.db --socket-path /tmp/node.socket start-index
