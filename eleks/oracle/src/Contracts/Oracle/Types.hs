{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# options_ghc -fno-warn-orphans          #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-strictness            #-}
{-# options_ghc -fno-specialise            #-}

module Contracts.Oracle.Types
  where

import           Cardano.Crypto.Wallet           (xprv, unXPrv, XPrv)
import           Cardano.Crypto.Wallet.Encrypted (EncryptedKey)
import           Control.Monad                   (mzero)
import           Data.Aeson
import           Data.Aeson.TH   
import           Data.Aeson.Types
import           Data.Either                     (fromRight)
import           Data.Map                        (lookup)
import           Ledger                          hiding (txOutRefs)
import           Ledger.Oracle                   (SignedMessage(..))
import           Ledger.Value                    (TokenName (..), AssetClass (..), assetClass, assetClassValue, assetClassValueOf)
import           Playground.Contract             (Show, FromJSON, Generic, ToJSON, ToSchema)
import           Plutus.ChainIndex.Tx            (txOutRefs, ChainIndexTx (..), ChainIndexTxOutputs (..))
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Prelude                         as Haskell
import           Text.Printf                     (PrintfArg)
import           Types.Game                      (GameId, TeamId, FixtureStatusShort (..))
import           Data.ByteString                 (ByteString)
import qualified Data.OpenApi.Schema             as OpenApi
import           Plutus.V1.Ledger.Api            (Credential (PubKeyCredential, ScriptCredential))
import           Ledger.Crypto                   (PrivateKey)

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

instance FromJSON XPrv where
    parseJSON (Object v) = (v .: "encryptedKey" :: Parser ByteString) >>= (\s -> case (xprv s) of Left _ -> mzero; Right r -> return r) 
      
instance ToJSON XPrv where 
   toJSON xprv =
        object ["encryptedKey" .= unXPrv xprv]

data OracleRedeemer = Update | OracleRedeem
    deriving Show
PlutusTx.makeIsDataIndexed ''OracleRedeemer [('Update, 0), ('OracleRedeem, 1)]

data OracleRequestRedeemer = Request | RedeemToken
    deriving Show
PlutusTx.makeIsDataIndexed ''OracleRequestRedeemer [('Request, 0), ('RedeemToken, 1)]

data OracleParams = OracleParams
    { opSymbol :: !CurrencySymbol
    , opFees   :: !Ada
    , opCollateral :: !Ada
    , opPublicKey :: PubKey
    , opSigner :: !PrivateKey
    } deriving (Generic, FromJSON, ToJSON, OpenApi.ToSchema)

{-# INLINABLE oracleRequestTokenName #-}
oracleRequestTokenName :: TokenName
oracleRequestTokenName = TokenName "oracleRequestTokenName"

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