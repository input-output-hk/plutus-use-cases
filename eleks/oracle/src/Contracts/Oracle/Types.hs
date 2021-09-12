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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# options_ghc -fno-warn-orphans          #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

module Contracts.Oracle.Types
  where

import           Ledger
import           Ledger.Oracle       (SignedMessage(..))
import           Ledger.Value        (TokenName (..), AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
import           Playground.Contract (Show, FromJSON, Generic, ToJSON, ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude              as Haskell
import           Text.Printf         (PrintfArg)

data Oracle = Oracle
    { --oSymbol   :: !CurrencySymbol
      oRequestTokenSymbol   :: !CurrencySymbol
    , oOperator :: !PubKeyHash
    , oOperatorKey :: !PubKey
    , oFee      :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''Oracle

data OracleRequestToken = OracleRequestToken
    { ortOperator :: !PubKeyHash
    , ortFee      :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''OracleRequestToken

oracleToRequestToken:: Oracle -> OracleRequestToken
oracleToRequestToken oracle = OracleRequestToken
    { ortOperator = oOperator oracle
    , ortFee = oFee oracle
    }

data OracleData = OracleData
    { ovGame           :: Integer
    , ovWinner         :: Integer
    , ovRequestAddress :: PubKeyHash
    , ovWinnerSigned   :: Maybe (SignedMessage Integer)
    }
    deriving (Show, Generic, FromJSON, ToJSON, Haskell.Eq)

PlutusTx.unstableMakeIsData ''OracleData
PlutusTx.makeLift ''OracleData

instance Eq OracleData where
    {-# INLINABLE (==) #-}
    l == r = (ovGame l == ovGame r) && 
             (ovWinner l == ovWinner r) &&
             (ovRequestAddress l == ovRequestAddress r) &&
             (ovWinnerSigned l PlutusTx.Prelude.== ovWinnerSigned r)

instance Eq a => Eq (SignedMessage a) where
    l == r =
        osmSignature l == osmSignature r
        && osmMessageHash l == osmMessageHash r
        && osmDatum l == osmDatum r

data OracleRedeemer = Update | Use
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer

{-# INLINABLE oracleTokenName #-}
oracleTokenName :: TokenName
oracleTokenName = TokenName "oracleTokenName"

-- {-# INLINABLE oracleAsset #-}
-- oracleAsset :: Oracle -> AssetClass
-- oracleAsset oracle = AssetClass (oSymbol oracle, oracleTokenName)

{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe OracleData
oracleValue o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d