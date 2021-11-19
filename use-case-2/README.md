## Welcome to The POKE-DEX (Plutus Obelisk Koin Economy Decentralized Exchange)

This DApp demonstration allows users to swap and stake tokens using token exchange smart contracts on the Cardano block chain.

This DApp is made possible using the IOHK's Plutus Application Backend(PAB) that exposes endpoints to smart contracts deployed in a local developer environment, manages wallet accounts, and executes transactions. PAB utilizes Nix as a build tool and Haskell as a programming language within its repository, which played a big part in influencing what tools were selected to build the other components of the DApp.

The frontend and middleware of this DApp is made possible using Obelisk, a framework that allows you to build high-quality web and mobile applications. Obelisk shares the same build tool and programming language as PAB, Haskell and Nix. This makes communication between PAB and Obelisk pleasant when it comes to parsing data types, sharing data types, and deployment.

By the end of this README you will be able to run the POKE-DEX on your machine locally and observe the behaviors of smart contracts against the Cardano mock chain by the power of PAB. Start by installing Obelisk, then running PAB, followed by running Obelisk as explained below

## System Requirements

* Linux (i686, x86_64, aarch64).
* macOS (x86_64).

## Running the App

1. **Install Obelisk** by following the instructions [here](https://github.com/obsidiansystems/obelisk#installing-obelisk).

1. **Optional: Add Nix caches**.  Otherwise you will have to spend a very long time compiling things from source.  The following includes both Obelisk and Cardano (IOHK) caches:
    ```nix
        binaryCaches = [
          "https://nixcache.reflex-frp.org"
          "https://hydra.iohk.io"
          "https://iohk.cachix.org"
        ];
        binaryCachePublicKeys = [
          "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
          "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
          "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
        ];
    ```

1. **Make sure you don't have anything running using port 8080**.  You can check by running `netstat -nltp | grep ':8080'`; if you see something like `tcp        0      0 0.0.0.0:8080            0.0.0.0:*               LISTEN      1234/some-server-process`, that means the port is in use by `some-server-process`, and you will need to stop that process.

1. **Run PAB** in its own terminal window by `cd`ing to this directory and runing `./scripts/run-pab.sh`.  Once it has started running, it will output something like `[INFO] Starting PAB backend server on port: 8080`, followed by many additional lines starting with `[INFO]`.  You will need to leave the PAB running for as long as you are using the app; if the PAB shuts down for any reason, all chain data will be lost, and you will need to `rm -rf db` to clear the app state and then restart from this step.

1. **Run this App** in a separate terminal window by `cd`ing to this directory and running `ob run`.  After a while, it will output lines starting with `"Pool tokens persisted:`, at which point the app is fully functional.

1. **Open the App** in a Chrome or Chromium window (**not** Firefox, see [below](#development-mode-supported-browsers)), navigate to [http://localhost:8000](http://localhost:8000).

##  Development Mode Supported Browsers
`ob run` uses a lot of tricks to make development fast.  Currently, it is only tested with Chrome-based browsers, such as Google Chrome and Chromium.  Firefox does **not** work - typically the app will load, but then hang.

Production deployments of the application should work in all major browsers, including Firefox.

##  Developing this Plutus Obelisk DApp

  Here are some useful tools and tips to get you started on developing and trouble shooting.
  1. Diving into PAB and Smart Contract programming!
    This application leverages a significant amount of Lar's Uniswap Smart Contract implementation thoroughly explained in the Plutus Pioneer Program. Here is a [video link](https://www.youtube.com/watch?v=Dg36h9YPMz4) in case you missed it!

  Some changes were made to the Smart Contract calls in order to provide more information to the frontend and to add other additional smart contract functionality. However, most of the modules and pure Uniswap functionality remained the same.

  Assuming you've already installed the Obelisk command line tool, let's take a look what PAB is tasked with doing under the hood by running the following commands.
  1. `ob thunk unpack dep/plutus-starter` will fetch the pinned version of plutus starter and replace a github .json file with the cloned plutus-starter repository
  1. `cd dep/plutus-starter`
  1. use your favorite text editor to open `pab/Main.hs` to inspect the source code of what PAB is running when launching this DApp.
  1. Building and Running PAB in GHCI Repl
    Now that you've used `ob thunk unpack` to get into `dep/plutus-starter`, from inside `dep/plutus-starter` you can get into a repl using the following commands:

      ```
      $ nix-shell
      $ cabal new-repl exe:plutus-starter-pab`
      ```
      At this point you can use `:r ` to reload and see what the GHC compiler has to say about the changes you've made. When you're comfortable with your changes, run `main` to start PAB which will cause it to listen on port 8080

  1. Developing the "App" Part of the "DApp"
    Now that we've got PAB ready for action, let's return back to the top level of this Obelisk project (`plutus-use-cases/use-case-2`) and briefly go over this application's components.

      - Server-side components are found within the `backend` folder. In the architecture of this DApp, the backend is responsible for communication directly with PAB via HTTP Request using JSON payloads that PAB's API endpoints are capable of performing smart contract operations with (If you're curious where PAB endpoints are constructed, take a peek at the `UniswapUserSchema` type located in `dep/plutus-starter/src/Plutus/Contracts/Uniswap/OffChain.hs`). Within `backend/src/Backend.hs` you will see request handlers that correspond to the smart contract operations endpoints which submit requests to PAB and parse PABs `observableState` JSON key in order to obtain PAB's response.
      - In-browser components are found within the `frontend` folder. This is where HTML rendering, event handling, and API calls to `backend` components are taking place. The frontend is written using Reflex-FRP. The frontend fetches a significant amount of its information about PAB's smart contract state via use of PAB's websocket and API endpoints via the websocket. Some interesting modules to look at are `frontend/src/Frontend.hs` and `frontend/src/Frontend/WebsocketParse.hs`.
      - Components shared between server-side and browser-side code are found within the `common` folder. As of now this mostly consists of middleware, datatypes that are shared between the frontend and backend components, as well as datatypes that are shared with PAB (shared data types are to be removed in coming Obelisk updates).

  1. Ok! Less talking, more poking user interfaces! While PAB is running and listening on port 8080, in a different terminal, run `ob run --no-interpret ./dep/plutus-starter`
    This will start up an interactive repl that will refresh itself anytime you make changes to the 3 component folders I've mentioned above (frontend, common, and backend). If everything compiled, the DApp should now be running on localhost:8000. Have fun!

##  TEST NET CARDANO-NODE AND CARDANO-WALLET SETUP:

This section outlines how to setup a cardano-node and cardano-wallet to perform transactions against testnet.

Startup the Alonzo Testnet Node

```
ob thunk unpack dep/cardano-node
cd dep/cardano-node
nix-build -A scripts.testnet.node -o result/alonzo-testnet/cardano-node-alonzo-testnet
nix-build -A cardano-cli -o result/alonzo-testnet/cardano-cli
cd result/alonzo-testnet/
./cardano-node-alonzo-testnet/bin/cardano-node-testnet
```

Setup & Run Cardano Wallet with Testnet Genesis Node

```
ob thunk unpack dep/cardano-node
cd dep/cardano-wallet
nix-build -A cardano-wallet -o result
```

 - create a symlink to cardano-node's node socket cardano-wallet's directory with the following command:

`ln -s ../cardano-node/result/alonzo-testnet/state-node-alonzo-purple/node.socket .`

 - download contest of https://hydra.iohk.io/build/7654130/download/1/testnet-byron-genesis.json to testnet-byron-genesis.json in this directory. Note: testnet magic number can be edited in this file

 - run the following command to launch a cardano-wallet on port 8090.

```
./result/bin/cardano-wallet serve --node-socket node.socket --testnet ./testnet-byron-genesis.json --database wallet-data --listen-address 0.0.0.0 --port 8090
```

cardano-wallet API endpoints are now available for use against testnet. See documentation here for more information about cardano-wallet endpoints: https://input-output-hk.github.io/cardano-wallet/api/edge/#tag/Wallets

##  NEXT UP: Deploy Contracts to a real Alonzo Node (WORK IN PROGRESS)

Build and Run Alonzo Purple Node
```
cd dep
ob thunk unpack cardano-node
cd cardano-node
nix-build -A cardano-cli -o result/alonzo-purple/cardano-cli
nix-build -A scripts.alonzo-purple.node -o result/alonzo-purple/cardano-node-alonzo-purple
cd result/alonzo-purple
./cardano-node-alonzo-purple/bin/cardano-node-alonzo-purple
```

In a seperate terminal, verify the Node's sychronization. When `syncProgress` reads 100.00, syncing has completed.
```
cd result/alonzo-purple
CARDANO_NODE_SOCKET_PATH=./state-node-alonzo-purple/node.socket ./cardano-cli/bin/cardano-cli query tip --testnet-magic 8
```

Mint a Uniswap Token (Assuming you already have a payment address with ADA)

Make note of a TxHash and TxIx you would like to use to mint a uniswap token
```
cardano-cli/bin/cardano-cli query utxo --address [ADDRESS] --testnet-magic 8
```

Send an arbitrary amount of funds to self in order to create a new utxo handle in order to not lose all funds in the unlikely event of collateral seizing runtime errors. Feel free to use the script call below from within ./dep/cardano-node/result/alonzo-purple
```
./../../../../scripts/buildCollateral.sh [TXHASH] [TXIX] [TO ADDRESS] [CHANGE ADDRESS] [SEND AMOUNT] [PAYMENT.SKEY PATH]
```
Once the node has mined your transaction, you'll be able to query for the new utxo handle to be used when minting the token

Next up, compile a script. Currently, the code we'll be using to compile a script can be found within plutus.

```
cd dep
ob thunk unpack plutus
cd plutus
nix-shell
cabal repl plutus-use-cases:lib:plutus-use-cases
:l Plutus.Contracts.CompileCurrency
main [TXHASH STRING] [TXIX STRING] [("Uniswap",1)]
```

That would have created a compiled script file called uniswapCurrency.plutus. This file will be used in the following minting script. NOTE: use this script from within ./dep/cardano-node/result/alonzo-purple
```
export CARDANO_NODE_SOCKET_PATH=./state-node-alonzo-purple/node.socket
mkdir dumpdir
touch issue.addr
echo '{"constructor":0,"fields":[]}' >> redeemerScript.0
./mintTokenScript.sh [PAYMENT ADDRESS] [PAYMENT VKEY] [PAYMENT SKEY] [UTXO HASH] [UTXO HASH INDEX] [PATH TO SCRIPT]
```

Give the node some time to mine the transaction.

After the previous transaction has been submitted, when querying your test wallet address for utxo's, you should have a utxo handle with a Uniswap Token available.

Next up, create a Uniswap Script and Script Address to send the Uniswap Token to.

Within the previously used cabal repl, run the following commands
```
:l Plutus.Contracts.FactoryScript
main [UNISWAP CURRENCY SYMBOL]
```
This will create `uniswapPlutusScript.plutus` and `factory.datumHash` to be used in the following bash script to build a script address, be sure to use this script from within ./dep/cardano-node/result/alonzo-purple

```
../../../../scripts/buildUniswapTokenAddress.sh [UNISWAP PLUTUS SCRIPT FILE]
```

Now that we have a script address, feel free to use the following script in order to send the Uniswap-Token to the Uniswap-Script-Address, be sure to use this script from within ./dep/cardano-node/result/alonzo-purple

```
../../../../scripts/sendUniswapTokenToUniswapScript.sh [TOKEN TXHAS#TXIX] [TXHASH#TXIX] [COLLATERAL TXHASH#TXIX] [PAYMENT SKEY] [SCRIPT ADDRESS FILE] [DATUM HASH FILE] [CHANGE ADDRESS] [UNISWAP CURRENCY SYMBOL]
```

Give some time for the node to mine your newly submitted transaction. Once successful, you should use cardano-cli to query the uniswap script address to confirm the uniswap token has been sent.

TODO: Document Mint PikaCoins
(Placeholder hint) The same script that was used to generate a single Uniswap Token, can be used to generate other ambiguous tokens.

TODO: Create Pool between ADA and PikaCoin
Get back into the plutus ghci

NOTE: to use ADA as one of the tokens to create a token pool with, pass in an empty string as both the currency symbol and token name.

```
:l Plutus.Contracts.UniPools
main [COIN A AMOUNT] [COIN B AMOUNT] [UNISWAP CURRENCY SYMBOL] [(COIN A CURRENCY SYMBOL, COIN A TOKENNAME)] [(COIN B CURRENCY SYMBOL, COIN B TOKENNAME)]
TODO: Make use of liquidity calculator and minimum lovelace requirements algorithms in bash script to handle hard coded integers.
```
NOTE: known happy expample path: `main 1930992 100000 [UNISWAP CURRENCY SYMBOL] [(COIN A CURRENCY SYMBOL, COIN A TOKENNAME)] [(COIN B CURRENCY SYMBOL, COIN B TOKENNAME)]`

This command will generate some files ./unipool necessary for cardano-cli to submit the smart contracts available to create a token pool between two coins and mint their liquidity tokens.

Use the following script to submit token pools, their liquidity state, and liquidity tokens to the Alonzo node:
```
# ./buildPool.sh [TXHASH#TXIX] [TOKEN B TXHASH#TXIX] [UNISWAP TOKEN TXHASH#TXIX] [SCRIPT FILE] [UNISWAP SCRIPT ADDRESS] [UNISWAP TOKEN CURRENCY SYMBOL] [POOL TOKEN CURRENCY SYMBOL] [FACTORY DATUM EMBED FILE] [LIQUIDITY POOL DATUM EMBED FILE] [CHANGE ADDRESS] [LIQUIDITY CURRENCY POLICY] [UNIPOOL DATUM HASH] [UNISWAP ACTION REDEEMER FILE] [SKEY FILE] [LIQUIDITY TOKEN CURRENCY SYMBOL] [TOKEN B CURRENCY SYMBOL]
```

Once submitted successfully, moments later when querying the uniswap contract address, there will be a utxo available that should show PoolState along with a datum hash and the amount of tokens used to initialize the token pool.

Now we're ready to perform a swap. In the terminal running ghci against plutus-use-cases, run the following command to generate the redeemer and pool script datum that will be used when building the cardano-cli transaction

```
:l Plutus.Contracts.CompileCurrency
main [(CURRENCY SYMBOL,TOKEN NAME)] [AMOUNT] [(CURRENCY SYMBOL,TOKEN NAME)] [AMOUNT] [POOL DATUM FILE PATH]
```

That will generate a directory called `rawSwap` that has the redeemer used to tell the uniswap script which contract action to be performed and the pool datum that reports the state of the uniswap liquidity pools

Feel free to use the following script to build and submit a transaction to perform the swap

Note: be sure to use this script from within ./dep/cardano-node/result/alonzo-purple
```
# ../../../../scripts/handleSwap.sh [TXHASH#TXIX] [UNISWAP SCRIPT FILE] [UNIPOOL DATUM FILE] [REDEEMER FILE] [COLLATERAL ADDRESS] [UNISWAP SCRIPT ADDRESS] [COIN TO BE SWAPPED - CURRENCYSYMBOL.TOKENNAME] [SKEY FILE] [ADDITIONAL FUNDS - TXHASH#TXIX] [POOL UTXO - TXHASH#TXIX] [POOL STATE CURRENCYSYMBOL.TOKENNAME]
```
