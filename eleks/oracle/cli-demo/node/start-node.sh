#!/bin/bash
set -e
dir=$(dirname "$0")

cabal exec -- cardano-node run \
--topology $dir/testnet-topology.json \
--database-path $dir/db \
--socket-path /tmp/node.socket \
--host-addr 0.0.0.0 \
--port 3001 \
--config $dir/testnet-config.json