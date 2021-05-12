# Plutus Platform starter project

This project gives a simple starter project for using the Plutus Platform.

## Setting up

- Install [nix](https://nixos.org/download.html)
- Clone and setup [plutus repo](https://github.com/input-output-hk/plutus) following README instructions (pay attention on setting binary cache)

## The Plutus Application Backend (PAB) example

We have provided an example PAB application in `./pab`. With the PAB we can serve and interact
with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

Here, the PAB is configured with sample contract, the `Game` contract from `./src/Plutus/Contracts/Game.hs`.

1. Start `nix-shell`
 
*Make sure that plutus is set up - and run nix-shell inside cloned Plutus repo*

1. Build the PAB executable:

```
cabal build plutus-starter-pab
```

2. Run the PAB binary:

```
cabal run plutus-starter-pab
````

This will then start up the server on port 8080. The devcontainer process will then automatically expose this port so that you can connect to it from any terminal (it doesn't have to be a terminal running in the devcontainer).