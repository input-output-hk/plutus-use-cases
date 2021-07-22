# Welcome to the Stable Coin Use Case Project

This project aims to develop a simple stable coin use case project using the Plutus Platform.

The stable coin arcitecutre used here is mainly based on age-usd provided by Emurgo.
https://github.com/Emurgo/age-usd
This protocol is based on crypto backed algorithimic stable coin protocol.

Here are primilary two tokens in action Reserve Tokens and Stable Token. 
Reserve tokens is used to provide reserves to insure the stability of tokens incase of price fluctuations. Stable tokens holders can redeem their tokens in current exchange rate for usd to ada regardless of initial price they paid to mint tokens. So the value is stable in terms of pegged rate of usd to ada at any moment. Reserve providers can benefit from increasing value of ada compared to usd as when redeeming  reserve tokens they get more underlying ada value compared to thier inital amount they used for minting reserve tokens. But also they bear loss when ada price is decreasing as they get less amount to cover more stable tokens amount to provide stable tokens holder stability.
## Setting up

##### Setup and run nix shell
1. Install Nix from
https://nixos.org/nix/

2. Setup binary caches
Note: This is most needed to speed up build up process.
```
mkdir ~/.config/nix
echo 'substituters = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/' >> ~/.config/nix/nix.conf
echo 'trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=' >> ~/.config/nix/nix.
conf
```
3. Clone https://github.com/input-output-hk/plutus

4. Switched to cloned plutus directory.
5. Run 
```
nix build -f default.nix plutus.haskell.packages.plutus-core.components.library 
```
from the root to build the Plutus Core library.

6. Start nix shell from plutus folder with command
```
nix-shell
```
With these avobe steps nix-shell terminal should be running.

These below steps should be done inside nix-shell


##### Build and run this repo
1. Clone this repository

2. Build the project
```
cabal build 
```
3. Run the PAB for stablecoin
```
cabal run stablecoin-pab
```
Wait for Contract activation upto slot 19 as shown in running terminal.
The PAB server is started on port 8080.

4. Get oracle contract instance with response from below command in which inside json structue tag named as "OracleContract"
```
curl -s http://localhost:8080/api/new/contract/instances | jq
```
You can find contract instance id inside "cicContract":{
  "unContractInstanceId":\<Contract Instance Id\> 
}
5. Export oracle instance id as
``` 
export ORACLE_ID =<Contract Id from above step>
```
6. Set initial exchange rate for 1 usd to ada in lovelaces from
```
curl -H "Content-Type: application/json" \
  --request POST \
  --data <Type your Exchange Rate> \
  http://localhost:8080/api/new/contract/instance/$ORACLE_ID/endpoint/update
```
7. Like as step 4 export Contract instance id for another wallet having tag StableContract
```
export INSTANCE_ID =<Contract Id for StableContract>
```
8. Mint reserve token with
```
curl -H "Content-Type: application/json" \
  --request POST \
  --data '
  {
    "tokenAmount":<Type your token amount to mint in number>         
  }' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/mintReserveCoin
```
9. Mint stable token with
```
curl -H "Content-Type: application/json" \
  --request POST \
  --data '
  {
    "tokenAmount":<Type your token amount to mint in number>         
  }' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/mintStableCoin
```
10. Redeem reserve token with
```
curl -H "Content-Type: application/json" \
  --request POST \
  --data '
  {
    "tokenAmount":<Type your token amount to redeem in number>         
  }' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/redeemStableCoin
```
11. Redeem stable token with
```
curl -H "Content-Type: application/json" \
  --request POST \
  --data '
  {
    "tokenAmount":<Type your token amount to redeem in number>         
  }' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/redeemStableCoin
```
12. After minting and redeeming you can check your balance with
  * Post call to funds endpoint to update funds to contract status
```
curl -H "Content-Type: application/json" \
  --request POST \
  --data '" "' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/funds
```

  * Check status endpoint of contract to get latest state of your funds
  ```
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/status | jq '.cicCurrentState.observableState'
  ```
13. Get current circulating supply of tokens in stable contract with
  * Post call to currentState endpoint to update currentState to contract status
```
curl -H "Content-Type: application/json" \
  --request POST \
  --data '" "' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/currentState
```

  * Check status endpoint of contract to get latest state of stable coin contract
  ```
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/status | jq '.cicCurrentState.observableState'
  ```

14. Get current rate of exchanges from
  * Post call to currentRates endpoint to update current rates to contract status
```
curl -H "Content-Type: application/json" \
  --request POST \
  --data '" "' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/currentRates
```

  * Check status endpoint of contract to get latest state of your funds
  ```
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/status | jq '.cicCurrentState.observableState'
  ```


## The Plutus Application Backend (PAB) example

We have provided an example PAB application in `./pab`. With the PAB we can serve and interact
with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

Here, the PAB is configured with one contract, the `Game` contract from `./examples/src/Plutus/Contracts/Game.hs`.

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

First, let's verify that the game is present in the server:

3. Check what contracts are present:

```
curl -s http://localhost:8080/api/new/contract/definitions | jq
```

You should receive a list of contracts and the endpoints that can be called on them, and the arguments
required for those endpoints.

We're interested in the `GameContract` one.

#### Playing the guessing game over the API

The game has two players (wallets). One will initialise the contract and lock a value inside. Another
wallet will then make guesses. Supposing they guess correctly, they'll receive the funds that were
locked; otherwise, they won't!

1. Start the instances:

```
# Wallet 1
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "GameContract", "caWallet":{"getWallet": 1}}' \
  http://localhost:8080/api/new/contract/activate | jq

# Wallet 2
curl -s -H "Content-Type: application/json" \
  --request POST \
  --data '{"caID": "GameContract", "caWallet":{"getWallet": 2}}' \
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

3. Start the game by locking some value inside

Now, let's call the `lock` endpoint to start the game. In order to do so, we need to construct
a JSON representation of the `LockParams` that the endpoint takes (look at `Game.hs`). The easiest
way is to simply build the term in haskell and ask `aeson` to encode it. From the terminal:

```
cabal repl
> import Plutus.Contracts.Game
> import Ledger.Ada
> args = LockParams { secretWord = "eagle", amount = lovelaceValueOf 90 }
> import Data.Aeson
> import Data.ByteString.Lazy.Char8 as BSL
> BSL.putStrLn $ encode args
{"amount":{"getValue":[[{"unCurrencySymbol":""},[[{"unTokenName":""},90]]]]},"secretWord":"eagle"}
```

Great! This is all we need to call the `lock` endpoint, so let's do that now with
the instance from Wallet 1:

4. Lock some value (Wallet 1)

```
export INSTANCE_ID=...
curl -H "Content-Type: application/json" \
  --request POST \
  --data '{"amount":{"getValue":[[{"unCurrencySymbol":""},[[{"unTokenName":""},90]]]]},"secretWord":"eagle"}' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/lock
```

We can do likewise to work out what the JSON for `GuessParams` is, and then make a guess from
Wallet 2:

5. Make a guess (Wallet 2)

```
export INSTANCE_ID=...
curl -H "Content-Type: application/json" \
  --request POST \
  --data '{"guessWord": "duck"}' \
  http://localhost:8080/api/new/contract/instance/$INSTANCE_ID/endpoint/guess
```

Note that this guess is wrong, so in the log of the server we will see that the transaction
didn't validate.

As an exercise, you can now spin up another instance for Wallet 2 and make a correct guess, and
confirm that the transaction validates and the Ada is transferred into the right wallet.

Note that you can verify the balances by looking at the log of `plutus-starter-pab` 
when exiting it by pressing return.

Finally, also node that the PAB also exposes a websocket, which you can read about in
the general [PAB Architecture documentation](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).
