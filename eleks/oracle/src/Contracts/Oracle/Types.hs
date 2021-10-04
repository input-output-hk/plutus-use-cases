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
import           Types.Game          (GameId, TeamId, FixtureStatusShort (..))

data Oracle = Oracle
    { --oSymbol   :: !CurrencySymbol
      oRequestTokenSymbol :: !CurrencySymbol -- Oracle request token currency symbol
    , oOperator           :: !PubKeyHash -- Oracle owner
    , oOperatorKey        :: !PubKey -- Oracle owner key used to verify signed data
    , oFee                :: !Ada -- Oracle fee amount
    , oCollateral         :: !Ada -- Oracle fee amount
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''Oracle

-- Token used for Oracle service monterization, 
-- One buy this token to pay for oracle service
data OracleRequestToken = OracleRequestToken
    { ortOperator   :: !PubKeyHash -- Oracle operator, address to send fee 
    , ortFee        :: !Ada -- token price
    , ortCollateral :: !Ada
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord)

PlutusTx.makeLift ''OracleRequestToken

oracleToRequestToken:: Oracle -> OracleRequestToken
oracleToRequestToken oracle = OracleRequestToken
    { ortOperator = oOperator oracle
    , ortFee = oFee oracle
    , ortCollateral = oCollateral oracle
    }

PlutusTx.makeLift ''FixtureStatusShort
PlutusTx.makeIsDataIndexed ''FixtureStatusShort [('NS, 0), ('LIVE, 1), ('FT, 2), ('CANC, 3)]
instance Eq FixtureStatusShort where
    {-# INLINABLE (==) #-}
    NS   == NS   = True
    LIVE == LIVE = True
    FT   == FT   = True
    CANC == CANC = True
    _    == _    = False 

data OracleSignedMessage = OracleSignedMessage
    { osmWinnerId   :: TeamId
    , osmGameId     :: GameId
    , osmGameStatus :: FixtureStatusShort
    } deriving (Show, Haskell.Eq)
PlutusTx.makeIsDataIndexed ''OracleSignedMessage [('OracleSignedMessage, 0)]
PlutusTx.makeLift ''OracleSignedMessage

instance Eq OracleSignedMessage where
    {-# INLINABLE (==) #-}
    l == r = (osmGameId l == osmGameId r) && 
             (osmWinnerId l == osmWinnerId r) && 
             (osmGameStatus l == osmGameStatus r)

data OracleData = OracleData
    { ovGame           :: Integer
    , ovRequestAddress :: PubKeyHash
    , ovSignedMessage  :: Maybe (SignedMessage OracleSignedMessage)
    }
    deriving (Show, Generic, FromJSON, ToJSON, Haskell.Eq)

PlutusTx.makeIsDataIndexed ''OracleData [('OracleData, 0)]
PlutusTx.makeLift ''OracleData

instance Eq OracleData where
    {-# INLINABLE (==) #-}
    l == r = (ovGame l == ovGame r) && 
             (ovRequestAddress l == ovRequestAddress r) &&
             (ovSignedMessage l PlutusTx.Prelude.== ovSignedMessage r)

instance Eq a => Eq (SignedMessage a) where
    l == r =
        osmSignature l == osmSignature r
        && osmMessageHash l == osmMessageHash r
        && osmDatum l == osmDatum r

data OracleRedeemer = Update | OracleRedeem
    deriving Show
PlutusTx.makeIsDataIndexed ''OracleRedeemer [('Update, 0), ('OracleRedeem, 1)]

data OracleRequestRedeemer = Request | RedeemToken
    deriving Show
PlutusTx.makeIsDataIndexed ''OracleRequestRedeemer [('Request, 0), ('RedeemToken, 1)]

{-# INLINABLE oracleRequestTokenName #-}
oracleRequestTokenName :: TokenName
oracleRequestTokenName = TokenName "oracleRequestTokenName"

{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe OracleData
oracleValue o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d