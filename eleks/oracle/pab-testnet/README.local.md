## Local
### Oracle 
1. Start pab
  If it's the first time your running, you'll need to ask the PAB to make the
  database:
  ```
  cabal exec -- testnet-oracle-pab --config pab-testnet/oracle/local-cluster-config.yml migrate
  ```

  Then, run the PAB

  ```
  cabal exec -- testnet-oracle-pab \
    --config pab-testnet/oracle/local-cluster-config.yml webserver \
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
  export OWNER_INSTANCE_ID=81d04613-f143-402f-b24b-bcde02016663
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9085/api/contract/instance/$OWNER_INSTANCE_ID/status | jq '.cicCurrentState.observableState.contents'



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
    "unCurrencySymbol": "fffa2c704f23301bb411fb28ab626d1c5cdcd4de13bac9814bb45dd7"
  },
  "oOperatorKey": {
    "getPubKey": "c15b990344122b12494a5edd1020d9eb32e34b0f82691f8e31645ddab712ff2b"
  }
}

  },"caWallet":{"getWalletId":"2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"}}' \
    localhost:9085/api/contract/activate
    
export CLIENT_INSTANCE_ID=450709cd-6f63-49bc-bae2-cdce4d75d96b
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9085/api/contract/instance/$CLIENT_INSTANCE_ID/status | jq '.cicCurrentState'


curl -H "Content-Type: application/json" \
  --request POST \
  --data '[]' \
  http://localhost:9085/api/contract/instance/$OWNER_INSTANCE_ID/endpoint/games

  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9085/api/contract/instance/$OWNER_INSTANCE_ID/status | jq '.cicCurrentState'
### wallets
curl -H "content-type: application/json" \
      -XGET localhost:46493/v2/wallets/$WALLET_ID | jq '.'


Update  Live
curl -H "Content-Type: application/json" \
  --request POST \
  --data "{\"uoGameStatus\":\"LIVE\",\"uoGameId\":1,\"uoWinnerId\":0}" \
  http://localhost:9085/api/contract/instance/$OWNER_INSTANCE_ID/endpoint/update

update Finish
curl -H "Content-Type: application/json" \
  --request POST \
  --data "{\"uoGameStatus\":\"FT\",\"uoGameId\":1,\"uoWinnerId\":55}" \
  http://localhost:9085/api/contract/instance/$OWNER_INSTANCE_ID/endpoint/update



Redeem


curl -H "Content-Type: application/json" -v -X POST -d \
    '{"caID":{"tag":"OracleUse", "contents": 
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
    "unCurrencySymbol": "fffa2c704f23301bb411fb28ab626d1c5cdcd4de13bac9814bb45dd7"
  },
  "oOperatorKey": {
    "getPubKey": "c15b990344122b12494a5edd1020d9eb32e34b0f82691f8e31645ddab712ff2b"
  }
}

  },"caWallet":{"getWalletId":"2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"}}' \
    localhost:9085/api/contract/activate

curl -H "Content-Type: application/json" \
  --request POST \
  --data "{\"roGame\":1}" \
  http://localhost:9085/api/contract/instance/$CLIENT_INSTANCE_ID/endpoint/use



### Mutual bet 
1. Start pab
  If it's the first time your running, you'll need to ask the PAB to make the
  database:
  ```
  cabal exec -- testnet-mutual-bet-pab --config pab-testnet/mutual-bet/local-cluster-config.yml migrate
  ```

  Then, run the PAB

  ```
  cabal exec -- testnet-mutual-bet-pab \
    --config pab-testnet/mutual-bet/local-cluster-config.yml webserver \
    --passphrase cardano-wallet
  ```

2. Get wallet id key hash

 export WALLET_ID=2d4cc31a4b3116ab86bfe529d30d9c362acd0b44
 curl -XGET http://localhost:9086/wallet/$WALLET_ID/own-public-key | jq '.wiPubKeyHash'

3. Start mutual bet owner 

curl -H "Content-Type: application/json" -v -X POST -d \
    '{"caID":{"tag":"MutualBetOwner", "contents":
    { 
    "mbpOracle":
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
    "unCurrencySymbol": "fffa2c704f23301bb411fb28ab626d1c5cdcd4de13bac9814bb45dd7"
  },
  "oOperatorKey": {
    "getPubKey": "c15b990344122b12494a5edd1020d9eb32e34b0f82691f8e31645ddab712ff2b"
  }
}


    , 
    "mbpOwner":{ "getPubKeyHash": "9ed7d88109a20fc0ffe82926040f0768a6bd1dbeceb8124a9050ff85"}, 
    "mbpTeam1":55, 
    "mbpBetFee":{ "getLovelace":2000000},
    "mbpGame":1,
    "mbpMinBet":{ "getLovelace":2000000},
    "mbpTeam2":42
  }

  },"caWallet":{"getWalletId":"2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"}}' \
    localhost:9086/api/contract/activate

4. Get token 
  export INSTANCE_ID=7d4e3cf5-ad8d-43ac-a414-52bad0638112
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9086/api/contract/instance/$INSTANCE_ID/status | jq '.cicCurrentState.observableState.mutualBetThreadToken'

5. Start user contract

curl -H "Content-Type: application/json" -v -X POST -d \
    '{"caID":{"tag":"MutualBetUser", "contents": [
    {
  "ttCurrencySymbol": {
    "unCurrencySymbol": "c25bf0585977df501be921b599b7573d8afc3f8442a6c8f47cc729c5"
  },
  "ttOutRef": {
    "txOutRefId": {
      "getTxId": "0eb9f7eeb5021fed465191a45ee81444080be4ccb1f1f6698de5aa7f6048c826"
    },
    "txOutRefIdx": 6
  }
},
    { 
    "mbpOracle":
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
    , 
    "mbpOwner":{ "getPubKeyHash": "9ed7d88109a20fc0ffe82926040f0768a6bd1dbeceb8124a9050ff85"}, 
    "mbpTeam1":55, 
    "mbpBetFee":{ "getLovelace":2000000},
    "mbpGame":1,
    "mbpMinBet":{ "getLovelace":2000000},
    "mbpTeam2":42
  }], "tag ": "MutualBetUser"

  },"caWallet":{"getWalletId":"2d4cc31a4b3116ab86bfe529d30d9c362acd0b44"}}' \
    localhost:9086/api/contract/activate

4. Get user state  
  export INSTANCE_ID=1b144838-eb5e-48d6-b796-9441dcf1f3a9
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9086/api/contract/instance/$INSTANCE_ID/status | jq '.cicCurrentState'





curl -vk -H "Content-Type: application/json" -XPOST http://localhost:9083/from-hash/datum -d '"6da1eae2947ed534e647ba39e3053a928f9a6b4a1ed0ce14d55c60f6000f7ef1"'


curl -H "Content-Type: application/json" -XGET http://localhost:9083/tip