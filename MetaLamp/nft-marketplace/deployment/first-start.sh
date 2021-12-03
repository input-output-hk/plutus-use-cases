#!/bin/sh

export NODE_TAG=$1
export NODE_PATH=$2

CONFIG=https://hydra.iohk.io/build/7366583/download/1/testnet-config.json
BYRON_GENESIS=https://hydra.iohk.io/build/7366583/download/1/testnet-byron-genesis.json
SHELLEY_GENESIS=https://hydra.iohk.io/build/7366583/download/1/testnet-shelley-genesis.json
ALONZO_GENESIS=https://hydra.iohk.io/build/7366583/download/1/testnet-alonzo-genesis.json
TOPOLOGY=https://hydra.iohk.io/build/7189190/download/1/testnet-topology.json

##Making some folders
mkdir -p ${NODE_PATH}/configuration/config/
mkdir -p ${NODE_PATH}/configuration/topology/
mkdir -p ${NODE_PATH}/configuration/sockets/

##Making DB Folder
mkdir -p ${NODE_PATH}/database/

##Touch for a Socket
touch ${NODE_PATH}/configuration/sockets/node.socket

##Getting Config
echo "--getting config"
wget  $CONFIG -P ${NODE_PATH}/configuration/config
wget  $BYRON_GENESIS -P ${NODE_PATH}/configuration/config/
wget  $SHELLEY_GENESIS -P ${NODE_PATH}/configuration/config/
wget  $ALONZO_GENESIS -P ${NODE_PATH}/configuration/config/

##Getting Topology
echo "--getting topology"
wget $TOPOLOGY -P ${NODE_PATH}/configuration/topology/

##Starting Docker-Compose
docker-compose up
