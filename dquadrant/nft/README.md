# Plutus Tokens MarketPlace


## Setting up

- Assume that we are in directory /home/user, clone this project to directory `/home/user/plutus-use-cases`
- Install Nix [Instructions](https://nixos.org/download.html)
- Create either `/etc/nix/nix.conf` or `~/.config/nix/nix.conf` with following content

```
substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

- Clone https://github.com/input-output-hk/plutus to `/home/user/plutus`
- cd to /home/user/plutus and Checkout `plutus-starter-devcontainer/v1.0.6` tag
- On the Plutus repository root, issue `nix-shell` command
- Inside the nix shell, `cd` to `/home/user/plutus-use-cases/dquadrant/nft`

##### Running Tests
 ` cabal test plutus-tokens:test:tokens-test`

##### Starting PAB server
 `cabal run tokens-pab`

##### Starting the client  [Instructions here](./client/README.md)


## Architecture
### DirectSale
Direct Sale is created by submitting a utxo to scriptAddress with Datum containing DirectSale Schema.
Anyone can then claim the utxos by paying enough to the seller and fees to operator.

![DirectSale Flow Image](./docs/DirectSaleFlow.jpg)

```http request
POST http://localhost8080/api/new/contract/instances/${instance_id}
```


Auctions are created by submitting utxo

![Auction Flow Image](./docs/AuctionFlow.jpg)


## Interacting with PAB server
Pab server is available on http://localhost:8080 .

In the PAB server simulation,
 - Wallet 10 is operator of the market
 - Wallet 1-5 have active endpoints to interact with them.

During the simulation at anytime, you can give `\n` (NewLine or Enter) key input to the pab-server to see it's status.
It will list all the wallets and balances in them.





