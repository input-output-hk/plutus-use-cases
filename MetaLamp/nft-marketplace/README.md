# NFT Marketplace

Marketplace model:
. Store only files from IPFS network. Get the IpfsCid
. NFT token should be minted first in a separate transaction so that state token won't be linked to IpfsCid
. Minting policy is parametrized by IpfsCid
. TokenName of a NFT is the IpfsCid
. NFT ID is the CurrencySymbol
. Store "data NFT" above in scripts datum as (Map nftId "data NFT")
. Only user with NFT (inside his wallet) identified by the nftId has access to view "data NFT"
. Nobody can modify "data NFT"
. Nobody can mint another NFT with same nftId
. Nobody can burn NFT (?)
TODO do we store NFT metadata from Cardano in IPFS also?

TODO wrap "data NFT" into (isOnSale, "data NFT")
to give access to view "data NFT" which are on sale for other users
(only user with NFT inside wallet could change isOnSale)

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
