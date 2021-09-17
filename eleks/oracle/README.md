## The Plutus Application Backend (PAB) example

We have provided an example PAB application in `./pab`. With the PAB we can serve and interact
with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

Here, the PAB is configured with the `Oracle` contract from `./src/Plutus/Contracts/Oracle.hs`

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

3. Run the PAB binary:

```
cabal exec -- oracle-client
````

### Queries

##Pab Query
1. Instance status
export INSTANCE_ID=...
curl -s http://localhost:9080/api/new/contract/instance/$INSTANCE_ID/status | jq

2. Running mutual bat contract info and instance id
curl -s http://localhost:9080/api/contract/instances/wallet/1 | jq '.[] | select(.cicDefinition.tag=="MutualBetBettorContract") | .cicDefinition, .cicContract.unContractInstanceId'

//todo make a bet