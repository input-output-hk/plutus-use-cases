
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}


module NodeFactory.Plutus.Contracts.StableCoin.Types
  where

import           Ledger
import           Ledger.Value        (AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
import           Playground.Contract (FromJSON, Generic, ToJSON, ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude             as Haskell
import           Text.Printf         (PrintfArg)

-- | SC-state coin token
data SC = SC
PlutusTx.makeIsDataIndexed ''SC [('SC, 0)]
PlutusTx.makeLift ''SC

-- | Vault-state coin token
data VaultState = VaultState
PlutusTx.makeIsDataIndexed ''VaultState [('VaultState, 0)]
PlutusTx.makeLift ''VaultState

-- | sUSD coin token
data sUSD = sUSD
PlutusTx.makeIsDataIndexed ''sUSD [('sUSD, 0)]
PlutusTx.makeLift ''sUSD

-- | A single 'AssetClass'.
newtype Coin a = Coin { unCoin :: AssetClass }
  deriving stock   (Haskell.Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema, Eq, Haskell.Eq, Haskell.Ord)
PlutusTx.makeIsDataIndexed ''Coin [('Coin, 0)]
PlutusTx.makeLift ''Coin

-- | Likewise for 'Integer'.
newtype Amount a = Amount { unAmount :: Integer }
  deriving stock   (Haskell.Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToSchema, Eq, Ord, PrintfArg)
  deriving newtype (Haskell.Eq, Haskell.Ord, Haskell.Num)
  deriving newtype (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
PlutusTx.makeIsDataIndexed ''Amount [('Amount, 0)]
PlutusTx.makeLift ''Amount

{-# INLINABLE valueOf #-}
valueOf :: Coin a -> Amount a -> Value
valueOf c a = assetClassValue (unCoin c) (unAmount a)

{-# INLINABLE unitValue #-}
unitValue :: Coin a -> Value
unitValue c = valueOf c 1

{-# INLINABLE isUnity #-}
isUnity :: Value -> Coin a -> Bool
isUnity v c = amountOf v c == 1

{-# INLINABLE amountOf #-}
amountOf :: Value -> Coin a -> Amount a
amountOf v = Amount . assetClassValueOf v . unCoin

{-# INLINABLE mkCoin #-}
mkCoin :: CurrencySymbol -> TokenName -> Coin a
mkCoin c = Coin . assetClass c

data StableCoin = StableCoin
    { sCoin :: Coin SC
    , scStablecoinTokenName     :: TokenName
--    , oracle                    :: Oracle
    } deriving stock    (Haskell.Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)
PlutusTx.makeIsDataIndexed ''StableCoin [('StableCoin, 0)]
PlutusTx.makeLift ''StableCoin

instance Eq StableCoin where
    {-# INLINABLE (==) #-}
    x == y = (sCoin x == sCoin y && scStablecoinTokenName x == scStablecoinTokenName y)

data StableCoinVault = StableCoinVault
    { owner  :: !PubKeyHash      -- owner of the of the vault
    , amount :: !Integer         -- amount of ADA locked in vault
    } deriving (Haskell.Show, Generic, ToJSON, FromJSON, ToSchema)
PlutusTx.makeIsDataIndexed ''StableCoinVault [('StableCoinVault, 0)]
PlutusTx.makeLift ''StableCoinVault

instance Eq StableCoinVault where
    {-# INLINABLE (==) #-}
    x == y = (owner x == owner y && amount x == amount y)

-- Actions that can be executed on vault
data StableCoinAction = Create StableCoinVault | Close | Liquidate
    deriving Haskell.Show
PlutusTx.makeIsDataIndexed ''StableCoinAction [ ('Create,    0)
                                              , ('Close,     1)
                                              , ('Liquidate, 2)
                                              ]
PlutusTx.makeLift ''StableCoinAction

data StableCoinDatum = 
    Factory [StableCoinVault]
  | Vault StableCoinVault
  deriving stock (Haskell.Show)

PlutusTx.unstableMakeIsData ''StableCoinDatum --[ ('Factory, 0)
                                              --, ('Vault,   1)
                                              --]
PlutusTx.makeLift ''StableCoinDatum
