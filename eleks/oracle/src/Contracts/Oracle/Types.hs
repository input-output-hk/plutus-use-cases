{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -fno-warn-orphans          #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

module Contracts.Oracle.Types
  where

import Data.Aeson
import Data.Map (lookup)
import Data.OpenApi.Schema qualified as OpenApi
import Ledger hiding (txOutRefs)
import Ledger.Value (TokenName (..))
import Playground.Contract (Generic, Show, ToSchema)
import Plutus.ChainIndex.Tx (ChainIndexTx (..), ChainIndexTxOutputs (..), txOutRefs)
import Plutus.Contract.Oracle (SignedMessage (..))
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential, ScriptCredential))
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Haskell
import Types.Game (FixtureStatusShort (..), GameId, TeamId)

data Oracle = Oracle
    { --oSymbol   :: !CurrencySymbol
      oRequestTokenSymbol :: !CurrencySymbol -- Oracle request token currency symbol
    , oOperator           :: !PubKeyHash -- Oracle owner
    , oOperatorKey        :: !PubKey -- Oracle owner key used to verify signed data
    , oFee                :: !Ada -- Oracle fee amount
    , oCollateral         :: !Ada -- Oracle fee amount
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq, Haskell.Ord, OpenApi.ToSchema)

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


data OracleRedeemer = Update | OracleRedeem
    deriving Show
PlutusTx.makeIsDataIndexed ''OracleRedeemer [('Update, 0), ('OracleRedeem, 1)]

data OracleRequestRedeemer = Request | RedeemToken
    deriving Show
PlutusTx.makeIsDataIndexed ''OracleRequestRedeemer [('Request, 0), ('RedeemToken, 1)]

data OracleParams = OracleParams
    { --opSymbol :: !CurrencySymbol,
      opFees       :: !Ada
    , opCollateral :: !Ada
    , opSigner     :: !Haskell.String
    } deriving (Haskell.Eq, Show, Haskell.Ord, Generic, FromJSON, ToJSON, OpenApi.ToSchema)

data RedeemOracleParams = RedeemOracleParams
    { roGame           :: Integer -- use owned oracle request
    }
    deriving (Haskell.Eq, Show, Haskell.Ord, Generic, ToSchema, FromJSON, ToJSON, OpenApi.ToSchema)

data GameStateChange
    = GameStateChange
        { gmsOutRef            :: TxOutRef
        , gmsOutTx             :: ChainIndexTxOut
        , gmsOracleData        :: OracleData
        , gmsSignedMessage     :: SignedMessage OracleSignedMessage
        , gmsSignedMessageData :: OracleSignedMessage
        }
        deriving stock (Haskell.Show)

{-# INLINABLE oracleRequestTokenName #-}
oracleRequestTokenName :: TokenName
oracleRequestTokenName = TokenName "ortk"

{-# INLINABLE oracleValue #-}
oracleValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe OracleData
oracleValue o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

fromTxOutToChainIndexTxOut :: ChainIndexTx -> TxOut -> Maybe ChainIndexTxOut
fromTxOutToChainIndexTxOut ChainIndexTx{_citxData} TxOut { txOutAddress, txOutValue, txOutDatumHash } =
  case addressCredential txOutAddress of
    PubKeyCredential _ -> pure $ PublicKeyChainIndexTxOut txOutAddress txOutValue
    ScriptCredential vh ->
        txOutDatumHash >>=
        \h -> lookup h _citxData >>=
        \datum -> pure $ ScriptChainIndexTxOut txOutAddress (Left vh) (Right datum) txOutValue

-- | Get tx output references and tx outputs from tx.
chainIndexTxOutsWithRef :: ChainIndexTx -> [(Maybe ChainIndexTxOut, TxOutRef)]
chainIndexTxOutsWithRef tx@ChainIndexTx { _citxOutputs = ValidTx outputs } = zip (map (fromTxOutToChainIndexTxOut tx) outputs) $ txOutRefs tx
chainIndexTxOutsWithRef ChainIndexTx { _citxOutputs = InvalidTx }          = []
