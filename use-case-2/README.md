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

1. Assuming  cardano-node, cardano-cali, and deploying the Uniswap Smart Contract have completed (outlined in the remainder of this document) you will have to update the `config/backend/dappBackendConfig.json` file and run `ob run --no-interpret dep/plutus --no-interpret dep/cardano-node` in a seperate terminal to launch the SampleSwap Dapp on locahost port 8000.

1. **Open the App** in a Chrome or Chromium window (**not** Firefox, see [below](#development-mode-supported-browsers)), navigate to [http://localhost:8000](http://localhost:8000).

##  Development Mode Supported Browsers
`ob run` uses a lot of tricks to make development fast.  Currently, it is only tested with Chrome-based browsers, such as Google Chrome and Chromium.  Firefox does **not** work - typically the app will load, but then hang.

Production deployments of the application should work in all major browsers, including Firefox.

##  About the Smart Contract used

  Here are some useful tools and tips to get you started on developing and trouble shooting.
  1. Diving into Smart Contract programming!
    This application leverages a significant amount of Lars' [Uniswap Smart Contract](https://github.com/obsidiansystems/plutus/tree/is-plutus-bump-with-validator-compilers/plutus-use-cases/src/Plutus/Contracts/Uniswap) implementation thoroughly explained in the Plutus Pioneer Program. Here is a [video link](https://www.youtube.com/watch?v=Dg36h9YPMz4) in case you missed it!

##  TEST NET CARDANO-NODE AND CARDANO-CLI SETUP:

This section outlines how to setup a cardano-node and cardano-cli to perform transactions against testnet.

Startup the Alonzo Testnet Node

```
ob thunk unpack dep/cardano-node
cd dep/cardano-node
nix-build -A scripts.testnet.node -o result/alonzo-testnet/cardano-node-alonzo-testnet
nix-build -A cardano-cli -o result/alonzo-testnet/cardano-cli
cd result/alonzo-testnet/
./cardano-node-alonzo-testnet/bin/cardano-node-testnet
```

##  Deploy Uniswap Contracts to a Testnet Alonzo Node

Build and Run Alonzo Testnet Node
```
cd dep
ob thunk unpack cardano-node
cd cardano-node
nix-build -A cardano-cli -o result/alonzo-testnet/cardano-cli
nix-build -A scripts.testnet.node -o result/alonzo-testnet/cardano-node-alonzo-testnet
cd result/alonzo-testnet
./cardano-node-alonzo-testnet/bin/cardano-node-alonzo-testnet
```

In a seperate terminal, verify the Node's sychronization. When `syncProgress` reads 100.00, syncing has completed.
```
cd result/alonzo-testnet
CARDANO_NODE_SOCKET_PATH=./state-node-alonzo-testnet/node.socket ./cardano-cli/bin/cardano-cli query tip --testnet-magic 1097911063
```

Mint a Uniswap Token (Assuming you already have a payment address with ADA)

Make note of a TxHash and TxIx you would like to use to mint a uniswap token
```
cardano-cli/bin/cardano-cli query utxo --address [ADDRESS] --testnet-magic 1097911063
```

Send an arbitrary amount of funds to self in order to create a new utxo handle in order to not lose all funds in the unlikely event of collateral seizing runtime errors. Feel free to use the script call below from within ./dep/cardano-node/result/alonzo-testnet
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
:set -XOverloadedStrings
main [TXHASH STRING] [TXIX STRING] [("Uniswap",1)]
```

That would have created a compiled script file called uniswapCurrency.plutus. This file will be used in the following minting script. NOTE: use this script from within ./dep/cardano-node/result/alonzo-testnet
```
export CARDANO_NODE_SOCKET_PATH=./state-node-alonzo-testnet/node.socket
mkdir dumpdir
touch issue.addr
echo '{"constructor":0,"fields":[]}' >> redeemerScript.0
./mintUniswapTokenScript.sh [PAYMENT ADDRESS] [PAYMENT VKEY] [PAYMENT SKEY] [UTXO HASH] [UTXO HASH INDEX] [PATH TO SCRIPT(uniswapCurrency.plutus)]
```

Give the node some time to mine the transaction.

After the previous transaction has been submitted, when querying your test wallet address for utxo's, you should have a utxo handle with a Uniswap Token available. 
Pay special attention to the hash prepended to the Uniswap token when you query your available utxo. That hash is your Uniswap token's currency symbol and you will need it
in the next step.

Next up, create a Uniswap Script and Script Address to send the Uniswap Token to.

Within the previously used cabal repl, run the following commands
```
:l Plutus.Contracts.FactoryScript
main [UNISWAP CURRENCY SYMBOL]
```
This will create `uniswapPlutusScript.plutus` and `factory.datumHash` to be used in the following bash script to build a script address, be sure to use this script from within ./dep/cardano-node/result/alonzo-testnet

```
../../../../scripts/buildUniswapTokenAddress.sh [UNISWAP PLUTUS SCRIPT FILE PATH (uniswapPlutusScript.plutus)]
```

Now that we have a script address, feel free to use the following script in order to send the Uniswap-Token to the Uniswap-Script-Address, be sure to use this script from within ./dep/cardano-node/result/alonzo-testnet

```
../../../../scripts/sendUniswapTokenToUniswapScript.sh [TOKEN TXHAS#TXIX] [TXHASH#TXIX] [COLLATERAL TXHASH#TXIX] [PAYMENT SKEY] [SCRIPT ADDRESS FILE] [DATUM HASH FILE(factory.datumHash)] [CHANGE ADDRESS] [UNISWAP CURRENCY SYMBOL]
```

Give some time for the node to mine your newly submitted transaction. Once successful, you should use cardano-cli to query the uniswap script address to confirm the uniswap token has been sent.

Now might be a good time to mint some random tokens of your choice if you don't have tokens you would like to create a Uniswap token pool for already. 
You'll use the same step for `Plutus.Contract.CompileCurrency`, followed by making use of ./scripts/mintTokenScript.sh

```
:l Plutus.Contracts.CompileCurrency
:set -XOverloadedStrings
main [TXHASH STRING] [TXIX STRING] [("RandomToken",100000)]
./mintTokenScript.sh [PAYMENT ADDRESS] [PAYMENT VKEY] [PAYMENT SKEY] [UTXO HASH] [UTXO HASH INDEX] [PATH TO SCRIPT] [TOKEN NAME] [TOKEN AMOUNT]
```
Note: You may have to make changes to tx-in and tx-out within the script as needed to mitigate the lack of asset balancing within these scripts.

TODO: Create Pool between ADA and RandomToken
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
  Note: This script is a lot to keep track of, strongly advised to use ./scripts/runBuildPool.sh as a guidline, you'll also have to manage your tx-ins and outs within script file
```
# ./buildPool.sh [TXHASH#TXIX] [TOKEN B TXHASH#TXIX] [UNISWAP TOKEN TXHASH#TXIX] [SCRIPT FILE] [UNISWAP SCRIPT ADDRESS] [UNISWAP TOKEN CURRENCY SYMBOL] [POOL TOKEN CURRENCY SYMBOL] [FACTORY DATUM EMBED FILE] [LIQUIDITY POOL DATUM EMBED FILE] [CHANGE ADDRESS] [LIQUIDITY CURRENCY POLICY] [EMPTY UNIPOOL DATUM FILE PA    TH] [UNISWAP ACTION REDEEMER FILE(unipool-redeemer)] [SKEY FILE] [FACTORY DATUM HASH FILE(factoryDatum.hash)] [POOL DATUM HASH FILE(poolDatum.hash)] [TOKEN B CURRENCYSYMBOL.TOKENNAME] [LIQUIDITY TOKEN CURRENCYSYMBOL.TOKENNAME] [TOKEN A AMOUNT] [TOKEN B AMOUNT] [LIQUIDITY POOL AMOUNT]
```

Once submitted successfully, moments later when querying the uniswap contract address, there will be a utxo available that should show PoolState along with a datum hash and the amount of tokens used to initialize the token pool.

Now we're ready to perform a swap. In the terminal running ghci against plutus-use-cases, run the following command to generate the redeemer and pool script datum that will be used when building the cardano-cli transaction

```
:l Plutus.Contracts.RawSwap
main [CURRENCY SYMBOL A] [TOKEN NAME A] [AMOUNT] [CURRENCY SYMBOL B] [TOKEN NAME B] [AMOUNT] [POOL DATUM FILE PATH] [CARDANO-CLI EXE PATH] [CARDANO-NODE NODE SOCKET PATH] [UNISWAP ADDRESS] [TESTNET MAGIC NUMBER]
```

That will generate a directory called `rawSwap` that has the redeemer used to tell the uniswap script which contract action to be performed and the pool datum that reports the state of the uniswap liquidity pools

Feel free to use the following script to build and submit a transaction to perform the swap

Note: be sure to use this script from within ./dep/cardano-node/result/alonzo-testnet
```
# ../../../../scripts/handleSwap.sh [TXHASH#TXIX] [UNISWAP SCRIPT FILE] [UNIPOOL DATUM FILE] [REDEEMER FILE] [COLLATERAL ADDRESS] [UNISWAP SCRIPT ADDRESS] [COIN TO BE SWAPPED - CURRENCYSYMBOL.TOKENNAME] [SKEY FILE] [ADDITIONAL FUNDS - TXHASH#TXIX] [POOL UTXO - TXHASH#TXIX] [POOL STATE CURRENCYSYMBOL.TOKENNAME]
```
##  WASM: Cardano Serialization Libs

To generate or develop cardano serialization functions here's what you can do:

```
ob thunk unpack dep/minimal-webpack5-wasm-demo
cd dep/minimal-webpack5-wasm-demo
# Make changes to desired src files
nix-shell
yarn webpack && cp -v dist/*.wasm dist/main.js ../../static
```

##  ABOUT static/plutus-raw-swap

This file was generated to allow Obelisk backend to generate files regarding Pool Datum State and to utilize the same exact functions that have been deployed on chain
in it's own backend off chain.

Below I will inform how one can make development changes to this script. Please be aware that if you make changes to the validators functions that produce this script,
you may need to redeploy the Uniswap smart contract and make changes to similar code seen in validators, such as `checkSwap`.

To develop this code you will need to:

```
ob thunk unpack ./dep/plutus
cd ./dep/plutus
```

make desired changes to `./plutus-use-cases/src/Plutus/Contracts/RawSwap.hs` and `./plutus-use-cases/scripts/BruteSwap.hs`

build a new plutus-raw-swap executable by doing the following from within the ./dep/plutus dir

```
nix-shell
cabal build plutus-use-cases:exe:plutus-raw-swap
```

Lastly copy the generated exe to the static dir at the base of the obelisk project.
`cp dep/plutus/dist-newstyle/build/x86_64-linux/ghc-8.10.4.20210212/plutus-use-cases-0.1.0.0/x/plutus-raw-swap/build/plutus-raw-swap/plutus-raw-swap static/plutus-raw-swap`
