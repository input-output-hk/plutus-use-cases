# NFT marketplace PAB

This executable runs PAB with NFT marketplace contracts.

To serve contract PAB requires:
 - Cardano node  (socket path must be set through config, see `plutus-pab.yaml.sample`)
 - cardano-wallet (url must be set through config, see `plutus-pab.yaml.sample`)
 - chain-index (url must be set through config, see `plutus-pab.yaml.sample`)

To be able to sign transactions `--passphrase` need to be provided at PAB launch (see `cabal exec nft-marketplace -- --help`). That passphrase should be for wallet from cardano-wallet (WBE) which `id` will be used for contract activation.

PAB launch example:
```
cabal exec nft-marketplace -- --config path/to/plutus-pab.yaml --passphrase walletA_passphrase migrate (creates database)

cabal exec nft-marketplace -- --config path/to/plutus-pab.yaml --passphrase walletA_passphrase webserver
```

To activate NFT admin contract:
```
curl --location --request POST 'localhost:9080/api/contract/activate' \
--header 'Content-Type: application/json' \
--data-raw '{
    "caID": {
        "tag": "NftAdminContract"
    },
    "caWallet": {
        "getWalletId": "walletA_id"
    }
}'
```

To activate NFT user contract:
```
curl --location --request POST 'localhost:9080/api/contract/activate' \
--header 'Content-Type: application/json' \
--data-raw '{
    "caID": {
        "tag": "UserContract",
        "contents": {
            "app'\''symbol": {
                "unCurrencySymbol": "6666"
            }
        }
    },
    "caWallet": {
        "getWalletId": "walletA_id"
    }
}'
```
! Note - this data from the example above:
```
"contents": {
  "app'\''symbol": {
    "unCurrencySymbol": "6666"
    }
  }
```
provided only as example, real `NftAppSymbol` data should be obtained from the state of admin contract.