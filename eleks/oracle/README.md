## Games info rest server 

1. Build the game rest server:
```
cabal build gameserver
```
2. Run the Games server:
```
cabal exec -- gameserver
```

### game server API 

1. Games list
curl -s http://localhost:8081/games

2. Game by id 
curl -s http://localhost:8081/games/1

3. Mark game as completed
Available statuses
NS   - "Not Started",
LIVE - "In Progress", 
FT   - "Match Finished"
curl -v -X PUT -H "Content-Type: application/json" \
    -d '{"ugpSatus": "FT", "ugpWinnerTeamId": 55}' \
    http://localhost:8081/games/1


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
```

### Pab Queries

1. Instance status
export INSTANCE_ID=...
curl -s http://localhost:9080/api/new/contract/instance/$INSTANCE_ID/status | jq

2. Running mutual bat contract info and instance id
curl -s http://localhost:9080/api/contract/instances/wallet/1 | jq '.[] | select(.cicDefinition.tag=="MutualBetBettorContract") | .cicDefinition, .cicContract.unContractInstanceId'


curl -s http://localhost:9080/api/contract/instances/wallet/1 | jq '.[] | .cicDefinition, .cicContract.unContractInstanceId'
### Pab transactions
1. Make a bet 
export INSTANCE_ID=...
curl -H "Content-Type: application/json" \
  --request POST \
  --data '{"nbpAmount":1500000, "nbpOutcome": 55}' \
  http://localhost:9080/api/contract/instance/$INSTANCE_ID/endpoint/bet

2. Get contract state
curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9080/api/contract/instance/$INSTANCE_ID/status | jq '.cicCurrentState.observableState'