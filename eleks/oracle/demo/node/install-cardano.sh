#!/bin/bash
set -e

mkdir bin
cd bin
wget https://hydra.iohk.io/build/7739444/download/1/cardano-node-1.30.1-macos.tar.gz
tar -xf cardano-node-1.30.1-macos.tar.gz
rm cardano-node-1.30.1-macos.tar.gz

cp bin* /usr/local/bin
rm bin*

wget https://hydra.iohk.io/build/7366583/download/1/testnet-config.json
wget https://hydra.iohk.io/build/7366583/download/1/testnet-byron-genesis.json
wget https://hydra.iohk.io/build/7366583/download/1/testnet-shelley-genesis.json
wget https://hydra.iohk.io/build/7366583/download/1/testnet-alonzo-genesis.json
wget https://hydra.iohk.io/build/7366583/download/1/testnet-topology.json