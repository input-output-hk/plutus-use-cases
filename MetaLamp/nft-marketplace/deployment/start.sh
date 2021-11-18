#!/bin/sh

export NODE_TAG=$1
export NODE_PATH=$2

##Starting Docker-Compose
docker-compose up
