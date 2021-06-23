## [Use case 2] — DEX Token Swap, Liquidity and Stake Pools

Our main goal of this project is to get onboarded into the Plutus smart contract ecosystem, and gain full understanding of it's mechanics of the UTxO transaction model.

The project should make available the following functionallity that can be consumed by the WebUI:

* Swap 2 tokens within a pre-made liquidity pool
* Contribute to the liquidity pool by adding tokens

The examples are based on the plutus-use-cases uniswap example from the official plutus repo.

## CodeBase

There are 2 distinct areas of this codebase:

* **Plutus Contracts & PAB server**
  `pab/Main.hs` — is the executable source that wraps the PAB web server.
  `src/**`  — contains the Plutus contracts, helper functions and type definitions.
  `specs/**` — HSpec tests folder (TODO)

* **Web-UI**
  Located in the `web-ui` directory, it contains the stencil UI for consuming the PAB endpoints using a web browser. 

## Setup

1. Clone the official plutus repository 
2. Check out the `58bf9ed626d498c140c69a859a508da03843d097` comit
3. Enter nix-shell
4. Change to dex-token-swap directory
5. Run `cabal update`

(Example):

```bash
git clone git@github.com:input-output-hk/plutus.git
cd plutus
git checkout 58bf9ed626d498c140c69a859a508da03843d097

# Enter nix shell
nix-shell

# Change to AltLabs/dex-token-swap
cabal update
```

### Build & Run (PAB)

```bash
# Build
cabal build plutus-starter-pab

# Run PAB (Servant Webserver API) by default on port 8080
cabal exec -- plutus-starter-pab
```

### Verify PAB is operating properly

Here's an example of running and interacting with this contract via the API. For this it will help if you
have `jq` installed.

Check what contracts are present:

```
curl -s http://localhost:8080/api/new/contract/definitions | jq
```

You should receive a list of contracts and the endpoints that can be called on them, and the arguments
required for those endpoints.

## Swap overview 

#### PAB current bootstraping process

![Alt text](./img/Plutus_Notes-PAB_Note94.jpg?raw=true "Optional Title")

#### Endpoints

Users of the `Uniswap` contract have the following **endpoints** *consumable*:

* `create` — Creates a liquidity pool for a pair of coins. The creator provides liquidity for both coins and gets liquidity tokens in return.

  Each Liquidity pool creates another UTXO with a different token from the factory (state token) each time a pool is created a new token is minted
  **source: **`src/Plutus/Contracts/PoolForgery.hs`

* `close` — Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
  **source: **`src/Plutus/Contracts/PoolForgery.hs`

* `swap` — Uses a liquidity pool two swap one sort of coins in the pool against the other.

  **source: **`src/Plutus/Contracts/Uniswap.hs`

* `add` — Adds some liquidity to an existing liquidity pool in exchange for newly minted liquidity tokens.
  **source: **`src/Plutus/Contracts/Uniswap.hs`

* `remove` — Removes some liquidity from a liquidity pool in exchange for liquidity tokens.
  **source: **`src/Plutus/Contracts/Uniswap.hs`

#### `start` endpoint
The `start` endpoint is unique in a sense, that it's invoked during the PAB boot and it shuldn't be consumable by the user for this case. It creates a uniswap *Factory* 

This *Factory* will keep track of the <u>existing</u> **liquidity pools** and enforce that there will be at most one liquidity pool  for any pair of tokens at any given time. It keeps such record by the use of DATUM in the UTxO. Internally it invokes `forgeContract` from `Currency.hs` which defines and makes use of the `OneShotCurrency` data type for making of the NFT, which can uniquely identify the *Factory*.

#### Data Types

*(src/Plutus/Contracts/Data.hs)*

Important to note here are data types used by validators which are `UniswapAction` and `UniswapDatum` which are used to represent the *Redeemer* and *Datum* in the UTxO.

##### JSON types

All data types ending with *Params (eg. CreateParams) contain the structure that the consumable JSON api endpoints expect.

#### Playing with swap API with curl

(TODO)

coming very soon...

