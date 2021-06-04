#!/bin/bash

curl -s -H "Content-Type: application/json" \
   --request POST \
   --data "[]" \
   http://localhost:8080/api/new/contract/instance/$1/endpoint/funds