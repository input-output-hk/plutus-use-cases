module NFT.Cardano.Builtins where
  open import Agda.Builtin.Bool
  open import Agda.Builtin.Nat

  postulate
    UtxoWith : Set → Set

  private 
    ℕ = Nat
  
  data Address : Set where
     PublicKey  : Address
     AppAddress : Address

  -- A Transaction 
  postulate Tx : Set 

  -- Plutus BuilinString 
  postulate BuiltinString : Set

  postulate MintingPolicy : Set

  data TokenName : Set where
    tkName : BuiltinString → TokenName 

  data CurrencySymbol : Set where
    mkCurrSymbol : MintingPolicy → CurrencySymbol
  
  record AssetClass : Set where
    field
      AssetClass'Name           : TokenName
      AssetClass'CurrencySymbol : CurrencySymbol 

  data Value : Set where
    mkValue : AssetClass → ℕ → Value 

  mint : MintingPolicy → TokenName → ℕ → Value
  mint policy name quantity = mkValue assetClass quantity
    where
      assetClass = record
        { AssetClass'Name = name
        ; AssetClass'CurrencySymbol = mkCurrSymbol policy 
        }
  
  Singleton : ℕ
  Singleton = 1

