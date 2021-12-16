## Local
### Oracle 
1. Start pab
  If it's the first time your running, you'll need to ask the PAB to make the
  database:
  ```
  cabal exec -- testnet-both-pab --config pab-testnet/both/local-cluster-config.yml migrate
  ```

  Then, run the PAB

  ```
  cabal exec --  testnet-both-pab  \
    --config pab-testnet/both/local-cluster-config.yml webserver \
    --passphrase cardano-wallet
  ```

#local

 export WALLET_ID=2d4cc31a4b3116ab86bfe529d30d9c362acd0b44
 curl -H "Content-Type: application/json" -v -X POST -d \
    "{\"caID\":{\"tag\":\"OracleContract\", \"contents\": 
    {\"opSigner\":\"SCYrlxVqkH2FAfCCS4DyEX77x78ahDVxTwBhmF3010QmecWIArrBTVFHpkN7K5fWW+YYqQ2SjSlSRg7VLBWU1MFbmQNEEisSSUpe3RAg2esy40sPgmkfjjFkXdq3Ev8r4NaUHxjedKQzDZWqLU9JSxBq4i24kaOdu6dek8mtl5U=\",\"opFees\":{\"getLovelace\":2000000}}
     },\"caWallet\":{\"getWalletId\":\"$WALLET_ID\"}}" \
    localhost:9088/api/contract/activate
  ```

  query markte params
  ```
  export OWNER_INSTANCE_ID=65f8f5bd-6030-4a9e-ab40-e9b9bd65e0f0
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9088/api/contract/instance/$OWNER_INSTANCE_ID/status | jq '.cicCurrentState.observableState.contents'



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
    "unCurrencySymbol": "df91a29de51e57d0f50f9bf980953c9730ee196de21209075fe5ea63"
  },
  "oOperatorKey": {
    "getPubKey": "c15b990344122b12494a5edd1020d9eb32e34b0f82691f8e31645ddab712ff2b"
  }
}
  },"caWallet":{"getWalletId":"2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"}}' \
    localhost:9088/api/contract/activate
    
export CLIENT_INSTANCE_ID=8d39fc83-6821-4909-a1f4-ae17d0ecd18e
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9085/api/contract/instance/$CLIENT_INSTANCE_ID/status | jq '.cicCurrentState'


curl -H "Content-Type: application/json" \
  --request POST \
  --data '[]' \
  http://localhost:9088/api/contract/instance/$OWNER_INSTANCE_ID/endpoint/games

  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9088/api/contract/instance/$OWNER_INSTANCE_ID/status | jq '.cicCurrentState'
### wallets
curl -H "content-type: application/json" \
      -XGET localhost:46493/v2/wallets/$WALLET_ID | jq '.'


Update  Live
curl -H "Content-Type: application/json" \
  --request POST \
  --data "{\"uoGameStatus\":\"LIVE\",\"uoGameId\":1,\"uoWinnerId\":0}" \
  http://localhost:9088/api/contract/instance/$OWNER_INSTANCE_ID/endpoint/update

update Finish
curl -H "Content-Type: application/json" \
  --request POST \
  --data "{\"uoGameStatus\":\"FT\",\"uoGameId\":1,\"uoWinnerId\":55}" \
  http://localhost:9088/api/contract/instance/$OWNER_INSTANCE_ID/endpoint/update


## Redeem

curl -H "Content-Type: application/json" -v -X POST -d \
    '{"caID":{"tag":"OracleRedeemRequest", "contents": 
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
    "unCurrencySymbol": "df91a29de51e57d0f50f9bf980953c9730ee196de21209075fe5ea63"
  },
  "oOperatorKey": {
    "getPubKey": "c15b990344122b12494a5edd1020d9eb32e34b0f82691f8e31645ddab712ff2b"
  }
}

  },"caWallet":{"getWalletId":"2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"}}' \
    localhost:9088/api/contract/activate

export REDEEM_CONTRACT_ID=78ca73b9-f47c-4791-9e87-0736d6e55b55
curl -H "Content-Type: application/json" \
  --request POST \
  --data "{\"roGame\":1}" \
  http://localhost:9088/api/contract/instance/$REDEEM_CONTRACT_ID/endpoint/redeem


  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9088/api/contract/instance/$REDEEM_CONTRACT_ID/status | jq '.cicCurrentState'
### Mutual bet 
1. Start pab
  If it's the first time your running, you'll need to ask the PAB to make the
  database:
  ```
  cabal exec -- testnet-mutual-bet-pab --config pab-testnet/both/local-cluster-config.yml migrate
  ```

  Then, run the PAB

  ```
  cabal exec -- testnet-mutual-bet-pab \
    --config pab-testnet/both/local-cluster-config.yml webserver \
    --passphrase cardano-wallet
  ```

2. Get wallet id key hash

 export WALLET_ID=2d4cc31a4b3116ab86bfe529d30d9c362acd0b44
 curl -XGET http://localhost:9088/wallet/$WALLET_ID/own-public-key | jq '.wiPubKeyHash'

3. Start mutual bet owner 

curl -H "Content-Type: application/json" -v -X POST -d \
    '{"caID":{"tag":"MutualBetStartContract", "contents":
    { 
    "mbspOracle":
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
    "unCurrencySymbol": "df91a29de51e57d0f50f9bf980953c9730ee196de21209075fe5ea63"
  },
  "oOperatorKey": {
    "getPubKey": "c15b990344122b12494a5edd1020d9eb32e34b0f82691f8e31645ddab712ff2b"
  }
}
    , 
    "mbspOwner":{ "getPubKeyHash": "9ed7d88109a20fc0ffe82926040f0768a6bd1dbeceb8124a9050ff85"}, 
    "mbspTeam1":55, 
    "mbspBetFee":{ "getLovelace":2000000},
    "mbspGame":1,
    "mbspMinBet":{ "getLovelace":2000000},
    "mbspTeam2":42
  }

  },"caWallet":{"getWalletId":"2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"}}' \
    localhost:9088/api/contract/activate

4. Get params 
  export INSTANCE_ID=9b865441-6acb-4636-89af-e886151c2aa8
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9088/api/contract/instance/$INSTANCE_ID/status | jq '.cicCurrentState.observableState.Right'

5. Start user contract

curl -H "Content-Type: application/json" -v -X POST -d \
    '{"caID":{"tag":"MutualBetBettorContract", "contents":
{
  "mbpOracle": {
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
      "unCurrencySymbol": "df91a29de51e57d0f50f9bf980953c9730ee196de21209075fe5ea63"
    },
    "oOperatorKey": {
      "getPubKey": "c15b990344122b12494a5edd1020d9eb32e34b0f82691f8e31645ddab712ff2b"
    }
  },
  "mbpOwner": {
    "getPubKeyHash": "9ed7d88109a20fc0ffe82926040f0768a6bd1dbeceb8124a9050ff85"
  },
  "mbpTeam1": 55,
  "mbpBetFee": {
    "getLovelace": 2000000
  },
  "mbpGame": 1,
  "mbpMinBet": {
    "getLovelace": 2000000
  },
  "mbpTeam2": 42,
  "mbpMutualBetId": {
    "unAssetClass": [
      {
        "unCurrencySymbol": "bfc85eba30dfc395c864ae85bb59b52e914779d0e07711a9d64ffeea"
      },
      {
        "unTokenName": "MutualBet"
      }
    ]
  }
}
    , "tag ": "MutualBetUser"

  },"caWallet":{"getWalletId":"2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"}}' \
    localhost:9088/api/contract/activate

4. Get user state  
  export CLIENT_INSTANCE_ID=507c3788-20a7-45b0-9731-e1dca6bf0b5a
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9088/api/contract/instance/$CLIENT_INSTANCE_ID/status | jq '.cicCurrentState'


5. Make a bet 
set -x
curl -H "Content-Type: application/json" \
  --request POST \
  --data '{"nbpAmount":3000000, "nbpWinnerId": 55}' \
  http://localhost:9088/api/contract/instance/$CLIENT_INSTANCE_ID/endpoint/bet

curl -vk -H "Content-Type: application/json" -XPOST http://localhost:9083/from-hash/datum -d '"6da1eae2947ed534e647ba39e3053a928f9a6b4a1ed0ce14d55c60f6000f7ef1"'


curl -H "Content-Type: application/json" -XGET http://localhost:9083/tip