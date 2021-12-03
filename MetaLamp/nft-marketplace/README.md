# NFT Marketplace

[The description and specification of a project](Spec.md)

## Setting up

- Install nix
- Clone https://github.com/input-output-hk/plutus
- Set up your machine to build things with Nix, following the Plutus README (make sure to set up the binary cache!)

## The Plutus Application Backend (PAB) usage

We have provided two PAB applications in `./pab` and `./pab-simulation`. The first one is made for real world usage and interaction through frontend [client](client/README.md), the second one is a big test scenario.
With the PAB we can serve and interact with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

1. Enter the nix shell (from `nft-marketplace` directory):

```
nix-shell
```

2. Build the PAB executables (cd to plutus-use-cases/MetaLamp/nft-marketplace):

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

## IPFS

1. Install IPFS daemon following the [instruction](https://docs.ipfs.io/install/command-line/#official-distributions)

2. To initialize the repository using to store IPFS settings, run:

```
ipfs init
```

3. Set up the `CORS` settings:

```
ipfs config --json API.HTTPHeaders.Access-Control-Allow-Origin '["webui://-", "http://localhost:3000", "http://127.0.0.1:5001", "https://webui.ipfs.io", "https://localhost:8009", "http://localhost:8009"]'
```

```
ipfs config --json API.HTTPHeaders.Access-Control-Allow-Methods '["PUT", "POST"]'
```

4. Run IPFS server:

```
ipfs daemon
```

## Client

See the client [readme](client/README.md).
## Protocol functionality

See the description of user endpoints [here](src/Plutus/Contracts/NftMarketplace/Endpoints.hs)
