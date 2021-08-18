## The Plutus Application Backend (PAB) example

We have provided an example PAB application in `./pab`. With the PAB we can serve and interact
with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

Here, the PAB is configured with the `NFT` contract from `./src/Plutus/Contracts/NFT.hs`

Here's an example of running and interacting with this contract via the API. For this it will help if you
have `jq` installed.

1. Build the PAB executable:

```
cabal build bet-pab
```

2. Run the PAB binary:

```
cabal exec -- bet-pab
````

2. Run the PAB binary:

```
cabal exec -- oracle-client
````

export INSTANCE_ID='66bbf5bc-2493-445a-ab14-dcd857f69246'
curl -s http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/status | jq

curl -s http://localhost:8080/api/contract/instances | jq

curl -s http://localhost:8080/api/new/contract/instances/wallet/1 | jq '.[].cicDefintion | select(.mbpOracle != null)'

curl -s http://localhost:8080/api/new/contract/instances/wallet/1 | jq '.[] | select(.cicDefintion.mbpOracle != null) | .cicContract.unContractInstanceId'


curl -s http://localhost:8080/api/new/contract/definitions | jq '.cicCurrentState.cicDefintion'


| jq '.cicCurrentState.observableState'