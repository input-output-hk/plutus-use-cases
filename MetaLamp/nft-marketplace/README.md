# Lending Pool

Lending Pool is a smart contract application on Plutus Platform.
The smart contract protocol is based on [Aave](https://aave.com/), but does not strictly follow it.

## Setting up

- Install nix
- Clone https://github.com/input-output-hk/plutus
- Set up your machine to build things with Nix, following the Plutus README (make sure to set up the binary cache!)

## The Plutus Application Backend (PAB) usage

We have provided two PAB applications in `./pab` and `./pab-simulation`. The first one is made for real world usage and interaction through frontend [client](client/README.md), the second one is a big test scenario.
With the PAB we can serve and interact with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

1. Enter the nix shell (from `lending-pool` directory):

```
nix-shell
```

2. Build the PAB executables (cd to plutus-use-cases/MetaLamp/lending-pool):

```
cabal build all
```

3. Run the PAB binary:

```
cabal run pab
```

This will then start up the server on port 8080.

4. To run test simulation do:

```
cabal run pab-simulation
```

## Client

See the client [readme](client/README.md).

## Troubleshooting

See [note](client/README.md/#Troubleshooting).

## Protocol functionality

See the description of user endpoints [here](src/Plutus/Contracts/Endpoints.hs)
