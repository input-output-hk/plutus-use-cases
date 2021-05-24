# Plutus Platform starter project.

This project gives a simple starter project for using the Plutus Platform.

## Setting up

For now, the only supported tooling setup is to use the provided VSCode devcontainer to get an environment with the correct tools set up.

- Install Docker
- Install VSCode
  - Install the [Remote Development extension pack](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack)
  - You do *not* need to install the Haskell extension
- Get the docker image (for now, we need to build this with Nix)
  - Clone https://github.com/input-output-hk/plutus 
  - Set up your machine to build things with Nix, following the Plutus README (make sure to set up the binary cache!)
  - Build and load the docker container: `docker load < $(nix-build default.nix -A devcontainer)`
- Clone this repository and open it in VSCode
  - It will ask if you want to open it in the container, say yes.
  - `cabal build` from the terminal should work
  - Opening a Haskell file should give you IDE features (it takes a little while to set up the first time)


## The Plutus Application Backend (PAB) example

We have provided an example PAB application in `./pab`. With the PAB we can serve and interact
with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

Here, the PAB is configured with the `NFT` contract from `./src/Plutus/Contracts/NFT.hs`

Here's an example of running and interacting with this contract via the API. For this it will help if you
have `jq` installed.

1. Build the PAB executable:

```
cabal build plutus-starter-pab
```

2. Run the PAB binary:

```
cabal exec -- plutus-starter-pab
````

This will then start up the server on port 8080. The devcontainer process will then automatically expose this port so that you can connect to it from any terminal (it doesn't have to be a terminal running in the devcontainer).

1. Start the instances:

```
# Wallet 1
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "NFTStartContract", "caWallet":{"getWallet": 1}}' \
  http://localhost:8080/api/new/contract/activate | jq

# Wallet 2
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "CurrencyContract", "caWallet":{"getWallet": 2}}' \
  http://localhost:8080/api/new/contract/activate | jq
```

From these two queries you will get back two contract instance IDs. These will be needed
in the subsequent steps for running actions against. We can optionally take a look at the state
of the contract with the `status` API:

2. Get the status

```
export INSTANCE_ID=...
curl -s http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/status | jq
```

This has a lot of information; and in particular we can see what endpoints are still available
to call.

3. Start by creating NFT token


Create token
```
export INSTANCE_ID=...
curl -H "Content-Type: application/json" \
  --request POST \
  --data '{"cpTokenName":"TestToken","cpDescription":"Test description","cpAuthor":"John Smith","cpFile":"https://ipfs.io/ipfs/bafybeieznanm2s27u2okty2xgorltsfoegtgvamovfjx56ctgijtz4caoy"}' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/create
```

Get response
```
curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/status | jq '.cicCurrentState.observableState'
```

4. Query my tokens
```
export INSTANCE_ID=...
curl -H "Content-Type: application/json" \
  --request POST \
  --data '[]' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/userNftTokens
```

Get response
```
curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/status | jq '.cicCurrentState.observableState'
```

5. Set token for sell
    let nftTokenSellParams = NFTMarket.SellParams { spTokenSymbol = nftTokenSymbol token1Meta, spSellPrice = 1000}
Create sell parameters
```
cabal repl

import Contracts.NFT
import Ledger.Value   
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy.Char8 as BSL
args = SellParams { spTokenSymbol = CurrencySymbol $ B.pack "642e93f74cc55820874d3fb4e0b8300ef2c351b23260b1250d26d69d2a060c47", spSellPrice = 1000 }
BSL.putStrLn $ encode args
```

```
export INSTANCE_ID=...
curl -H "Content-Type: application/json" \
  --request POST \
  --data '{"spSellPrice":1000,"spTokenSymbol":{"unCurrencySymbol":"36343265393366373463633535383230383734643366623465306238333030656632633335316232333236306231323530643236643639643261303630633437"}}' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/sell
```

Get response
```
export INSTANCE_ID=...
curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/status | jq '.cicCurrentState.observableState'
```