## Local
1. Start pab
  If it's the first time your running, you'll need to ask the PAB to make the
  database:
  ```
  cabal exec -- testnet-oracle-pab --config pab-testnet/oracle/local-config.yml migrate
  ```

  Then, run the PAB

  ```
  cabal exec -- testnet-oracle-pab \
    --config pab-testnet/oracle/local-config.yml webserver \
    --passphrase cardano-wallet
  ```

#local

 export WALLET_ID=2d4cc31a4b3116ab86bfe529d30d9c362acd0b44
 curl -H "Content-Type: application/json" -v -X POST -d \
    "{\"caID\":{\"tag\":\"OracleContract\", \"contents\": 
    {\"opSigner\":\"SCYrlxVqkH2FAfCCS4DyEX77x78ahDVxTwBhmF3010QmecWIArrBTVFHpkN7K5fWW+YYqQ2SjSlSRg7VLBWU1MFbmQNEEisSSUpe3RAg2esy40sPgmkfjjFkXdq3Ev8r4NaUHxjedKQzDZWqLU9JSxBq4i24kaOdu6dek8mtl5U=\",\"opFees\":{\"getLovelace\":2000000},\"opCollateral\":{\"getLovelace\":2000000}}
     },\"caWallet\":{\"getWalletId\":\"$WALLET_ID\"}}" \
    localhost:9085/api/contract/activate
  ```

  query markte params
  ```
  export INSTANCE_ID=2b864357-4e0c-47cb-932f-796d11886fdd
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9085/api/contract/instance/$INSTANCE_ID/status | jq '.cicCurrentState.observableState.contents'



curl -H "Content-Type: application/json" -v -X POST -d \
    '{"caID":{"tag":"OracleRequest", "contents": 
{
  "oOperator": {
    "getPubKeyHash": "9ed7d88109a20fc0ffe82926040f0768a6bd1dbeceb8124a9050ff85"
  },
  "oCollateral": {
    "getLovelace": 2000000
  },
  "oFee": {
    "getLovelace": 2000000
  },
  "oRequestTokenSymbol": {
    "unCurrencySymbol": "9f093fbc47a14b082e1699551fe9cec5a6512f2fb912e112e2f6c2df"
  },
  "oOperatorKey": {
    "getPubKey": "c15b990344122b12494a5edd1020d9eb32e34b0f82691f8e31645ddab712ff2b"
  }
}
  },"caWallet":{"getWalletId":"2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"}}' \
    localhost:9085/api/contract/activate
    
export INSTANCE_ID=2aedfe39-7d45-40ce-abb8-7b814b58ca07
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9085/api/contract/instance/$INSTANCE_ID/status | jq '.cicCurrentState'

export INSTANCE_ID=2b864357-4e0c-47cb-932f-796d11886fdd
curl -H "Content-Type: application/json" \
  --request POST \
  --data '[]' \
  http://localhost:9085/api/contract/instance/$INSTANCE_ID/endpoint/games

  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9085/api/contract/instance/$INSTANCE_ID/status | jq '.cicCurrentState'
### wallets
curl -H "content-type: application/json" \
      -XGET localhost:46493/v2/wallets/$WALLET_ID | jq '.'