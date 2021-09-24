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

2. **Make sure you don't have anything running using port 8080**.  You can check by running `netstat -nltp | grep ':8080'`; if you see something like `tcp        0      0 0.0.0.0:8080            0.0.0.0:*               LISTEN      1234/some-server-process`, that means the port is in use by `some-server-process`, and you will need to stop that process.

3. **Run PAB** in its own terminal window by `cd`ing to this directory and runing `./scripts/run-pab.sh`.  Once it has started running, it will output something like `[INFO] Starting PAB backend server on port: 8080`, followed by many additional lines starting with `[INFO]`.  You will need to leave the PAB running for as long as you are using the app; if the PAB shuts down for any reason, all chain data will be lost, and you will need to `rm -rf db` to clear the app state and then restart from this step.

4. **Run this App** in a separate terminal window by `cd`ing to this directory and running `ob run`.  After a while, it will output lines starting with `"Pool tokens persisted:`, at which point the app is fully functional.

5. **Open the App** in a Chrome or Chromium window (**not** Firefox, see [below](#development-mode-supported-browsers)), navigate to [http://localhost:8000](http://localhost:8000).

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
