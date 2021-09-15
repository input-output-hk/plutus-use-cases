API
===

```
type API = "faucet" :> Capture "address" AddressParam :> QueryParam "tokenName" TokenName :> GetNoContent
            :<|> "tokens" :> Get '[JSON] [TokenName]
```

TODO