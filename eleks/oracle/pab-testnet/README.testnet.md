## Local
### Restore wallet 
curl -H "content-type: application/json" -XPOST \
    -d @pab-testnet/oracle/restore-wallet.json \
    localhost:8090/v2/wallets
88cc0e42b4a4f29d192c8b05967c386cb4c2aeaa

curl -H "content-type: application/json" -XPOST \
    -d @pab-testnet/oracle/client-wallet.json \
    localhost:8090/v2/wallets

    d6c363f6bb216612ea38646341a7322df0b915ff
### Oracle 
1. Start pab
  If it's the first time your running, you'll need to ask the PAB to make the
  database:
  ```
  cabal exec -- testnet-oracle-pab --config pab-testnet/oracle/testnet-config.yml migrate
  ```

  Then, run the PAB

  ```
  cabal exec -- testnet-oracle-pab \
    --config pab-testnet/oracle/testnet-config.yml webserver \
    --passphrase pab123456789
  ```

#local

 export OWNER_WALLET_ID=88cc0e42b4a4f29d192c8b05967c386cb4c2aeaa
 curl -H "Content-Type: application/json" -v -X POST -d \
    "{\"caID\":{\"tag\":\"OracleContract\", \"contents\": 
    {\"opSigner\":\"QA6cMN4QqEQJqHtGFrCLmzTL9XzQjw86PISr8cuuMEn5OMzbiK/ENAtHl+TS9mtY7F2MqGOD2m1srKUGUvTB5Hm4MeksJ0HhAQelN4eyTeobJxwmn85oEmr113fbGsXacq7e/K2fHpaZh4vzy/rq6pKKqgrgwDCKTTG9LLSlrAI=\",\"opFees\":{\"getLovelace\":2000000},\"opCollateral\":{\"getLovelace\":2000000}}
     },\"caWallet\":{\"getWalletId\":\"$OWNER_WALLET_ID\"}}" \
    localhost:9085/api/contract/activate
  ```

  query markte params
  ```
  export OWNER_INSTANCE_ID=40fcc86f-556e-4c67-bf13-80117e6a677d
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9085/api/contract/instance/$OWNER_INSTANCE_ID/status | jq '.cicCurrentState.observableState.contents'


2. Start client

  cabal exec -- testnet-oracle-pab --config pab-testnet/oracle/client-testnet-config.yml migrate
  ```

  Then, run the PAB

  ```
  cabal exec -- testnet-oracle-pab \
    --config pab-testnet/oracle/client-testnet-config.yml webserver \
    --passphrase clientwallet

3. Request token
export CLIENT_WALLET=d6c363f6bb216612ea38646341a7322df0b915ff
curl -H "Content-Type: application/json" -v -X POST -d \
    '{"caID":{"tag":"OracleRequest", "contents": 
{
  "oOperator": {
    "getPubKeyHash": "519ccb9453513f5165a661281819dbb487581a25cba373c6d51bcd8c"
  },
  "oCollateral": {
    "getLovelace": 2000000
  },
  "oFee": {
    "getLovelace": 2000000
  },
  "oRequestTokenSymbol": {
    "unCurrencySymbol": "0dacdaeb4026ba9eeec5c4c26fe853eb9115f27100cbbac6ada7f841"
  },
  "oOperatorKey": {
    "getPubKey": "79b831e92c2741e10107a53787b24dea1b271c269fce68126af5d777db1ac5da"
  }
}

  },"caWallet":{"getWalletId":"d6c363f6bb216612ea38646341a7322df0b915ff"}}' \
    localhost:9087/api/contract/activate
    
export INSTANCE_ID=18a28461-2b7f-4f56-b76a-c0ae5cad7365
  curl -H "Content-Type: application/json" \
  --request GET \
  http://localhost:9087/api/contract/instance/$INSTANCE_ID/status | jq '.cicCurrentState'

38de6c1dfd313615897ba388e4cd502e85f71c610ef43b3fd3677a4
38de6c1dfd313615897ba388e4cd502e85f71c610ef43b3fd3677a4
4. Query games by owner
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





curl -vk -H "Content-Type: application/json" -XPOST http://localhost:9083/from-hash/datum -d '"38de6c1dfd313615897ba388e4cd502e85f71c610ef43b3fd3677a49"'


curl -H "Content-Type: application/json" -XGET http://localhost:9083/tip



