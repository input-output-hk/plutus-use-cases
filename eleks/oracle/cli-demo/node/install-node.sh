#!/bin/bash
set -e

rm -rf bin || True
mkdir bin

cd bin
wget https://hydra.iohk.io/build/9259028/download/1/cardano-node-1.32.1-macos.tar.gz
tar -xf cardano-node-1.32.1-macos.tar.gz
#rm cardano-node-1.31.0-macos.tar.gz
rm -rf configuration || True
cd -
cp bin/*  /usr/local/bin 

wget -O testnet-config.json https://hydra.iohk.io/build/7654130/download/1/testnet-config.json
wget -O testnet-byron-genesis.json https://hydra.iohk.io/build/7654130/download/1/testnet-byron-genesis.json
wget -O testnet-shelley-genesis.json https://hydra.iohk.io/build/7654130/download/1/testnet-shelley-genesis.json
wget -O testnet-alonzo-genesis.json https://hydra.iohk.io/build/7654130/download/1/testnet-alonzo-genesis.json
wget -O testnet-topology.json https://hydra.iohk.io/build/7654130/download/1/testnet-topology.json