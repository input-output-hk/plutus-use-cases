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

  1. After running the Plutus Application Backend, in a different terminal, run `ob run --no-interpret ./dep/plutus-starter`
  1. The frontend should be running on localhost:8000 when successful and visible via your browser.

##  Supported Browsers
  1. Google Chrome
  1. Chromium
  
  Note: Firefox can not run the app given that `ob run`(the only instructions provided to see the fronted) does not currently support this browser, app may appear to be broken. 
