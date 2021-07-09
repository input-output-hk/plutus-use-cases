## Welcome to The POKE-DEX (Plutus Obelisk Koin Economy Decentralized Exchange)

This Dapp demonstration allows users to swap and stake tokens using token exchange smart contracts on the Cardano block chain.

This Dapp is made possible using the IOHK's Plutus Application Backend(PAB) that exposes endpoints to smart contracts deployed in a local developer environment, manages wallet accounts, and executes transactions. PAB utilizes Nix as a build tool and Haskell as a programming language within it's repository, which played a big part in influencing what tools were selected to build the other components of the Dapp.

The frontend and middleware of this Dapp is made possible using Obelisk, a framework that allows you to build high-quality web and mobile applications. Obelisk shares the same build tool and programming language as PAB, Haskell and Nix. This makes communication between PAB and Obelisk pleasant when it comes to parsing data types, sharing data types, and deployment.

By the end of this README you will be able to run the POKE-DEX on your machine locally and observe the behaviors of smart contracts against the Cardano mock chain by the power of PAB. Start by installing Obelisk, then running PAB, followed by running Obelisk as explained below

## Installing Obelisk

1. [Install Obelisk](https://github.com/obsidiansystems/obelisk#installing-obelisk).

##  Running Plutus Application Backend (PAB)

1. [Run PAB] After installing Obelisk, use `./scripts/run-pab.sh` to launch the PAB and have it listen on port 8080

##  Starting Obelisk Frontend

  1. After running the Plutus Application Backend, in a different terminal, run `ob run`
  1. The frontend should be running on localhost:8000 when successful and visible via your browser.

##  Supported Browsers
  1. Google Chrome
  1. Chromium

  Note: Firefox can not run the app given that `ob run`(the only instructions provided to see the fronted) does not currently support this browser, app may appear to be broken.

##  Developing this Plutus Obelisk Dapp

  Here are some useful tools and tips to get you started on developing and trouble shooting.
  1. Diving into PAB and Smart Contract programming!
    This application leverages a significant amount of Lar's Uniswap Smart Contract implementation thoroughly explained in the Plutus Pioneer Program. Here is a [video link](https://www.youtube.com/watch?v=Dg36h9YPMz4) in case you missed it! 

  Some changes were made to the Smart Contract calls in order to provide more information to the frontend and to add other additional smart contract functionality. However, most of the modules and pure Uniswap functionality remained the same.

  Assuming you've already installed the Obelisk command line tool, let's take a look what PAB is tasked with doing under the hood by running the following commands.
  1. `ob thunk unpack dep/plutus-starter` will fetch the pinned version of plutus starter and replace a github .json file with the cloned plutus-starter repository
  1. `cd dep/plutus-starter`
  1. use your favorite text editor to open `pab/Main.hs` to inspect the source code of what PAB is running when launching this Dapp.
  1. Building and Running PAB in GHCI Repl
    Now that you've used `ob thunk unpack` to get into `dep/plutus-starter`, from inside `dep/plutus-starter` you can get into a repl using the following commands:
    
      ```
      $ nix-shell
      $ cabal new-repl exe:plutus-starter-pab`
      ```
      At this point you can use `:r ` to reload and see what the GHC compiler has to say about the changes you've made. When you're comfortable with your changes, run `main` to start PAB which will cause it to listen on port 8080

  1. Developing the "App" Part of the "DApp"
    Now that we've got PAB ready for action, let's return back to the top level of this Obelisk project (`plutus-use-cases/use-case-2`) and briefly go over this application's components.
    - Backend components are found within the `backend` folder. In the architecture of this Dapp, the backend is responsible for communication directly with PAB via HTTP Request using JSON payloads that PAB's API endpoints are capable of performing smart contract operations with (If you're curious where PAB endpoints are constructed, take a peek at the `UniswapUserSchema` type located in `dep/plutus-starter/src/Plutus/Contracts/Uniswap/OffChain.hs`). Within `backend/src/Backend.hs` you will see request handlers that correspond to the smart contract operations endpoints which submit requests to PAB and parse PAB
    s `observableState` JSON key in order to obtain PAB's response.
    - Common components are found within the `common` folder. As of now this mostly consists of middleware, datatypes that are shared between the frontend and backend components, as well as datatypes that are shared with PAB (shared data types are to be removed in coming Obelisk updates).
    - Frontend components are found within the `frontend` folder. This is where dom building, event handling, and API calls to `backend` components are taking place. The frontend is written using Reflex-FRP. The frontend fetches a significant amount of it's information about PAB's smart contract state via use of PAB's websocket and API endpoints via the websocket. Some interesting modules to look at are `frontend/src/Frontend.hs` and `frontend/src/Frontend/WebsocketParse.hs`.

  1. Ok less talking, more poking user interfaces! While PAB is running and listening on port 8080, in a different terminal, run `ob run --no-interpret ./dep/plutus-starter`
    This will start up an interactive repl that will refresh itself anytime you make changes to the 3 component folders I've mentioned above (frontend, common, and backend). If everything compiled, the Dapp should now be running on localhost:8000. Have fun!
