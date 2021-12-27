{-# OPTIONS_GHC -Wno-orphans #-}

module Mlabs.NFT.Spooky (
  Credential (..),
  StakingCredential (..),
  Address (..),
  toSpookyAddress,
  unSpookyAddress,
  TxId (..),
  getTxId,
  TxOutRef (..),
  txOutRefId,
  txOutRefIdx,
  ScriptPurpose (..),
  TxOut (..),
  txOutAddress,
  txOutValue,
  txOutDatumHash,
  TxInInfo (..),
  txInInfoOutRef,
  txInInfoResolved,
  TxInfo (..),
  txInfoData,
  txInfoSignatories,
  txInfoOutputs,
  txInfoInputs,
  txInfoMint,
  valuePaidTo,
  pubKeyOutputsAt,
  findDatum,
  ScriptContext (..),
  scriptContextTxInfo,
  scriptContextPurpose,
  ownCurrencySymbol,
  Spooky,
  toSpooky,
  unSpooky,
) where

import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Hask

import GHC.Generics (Generic)

import Ledger (
  CurrencySymbol,
  Datum,
  POSIXTimeRange,
  ValidatorHash,
 )
import Ledger qualified
import Ledger.Scripts (DatumHash)
import Ledger.Value (Value)
import Plutus.V1.Ledger.Api (DCert, PubKeyHash)
import Plutus.V1.Ledger.Credential qualified as Credential
import PlutusTx.Spooky (Spooky, toSpooky, unSpooky)
import Schema (ToSchema (toSchema))

instance ToSchema BuiltinData where
  toSchema = toSchema @Hask.String

data Credential
  = PubKeyCredential (Spooky PubKeyHash)
  | ScriptCredential (Spooky ValidatorHash)
  deriving stock (Generic, Hask.Show, Hask.Eq)
PlutusTx.unstableMakeIsData ''Credential
PlutusTx.makeLift ''Credential

instance Eq Credential where
  PubKeyCredential pkh == PubKeyCredential pkh' = pkh == pkh'
  ScriptCredential vh == ScriptCredential vh' = vh == vh'
  _ == _ = False

{-# INLINEABLE unSpookyCredential #-}
unSpookyCredential :: Credential -> Credential.Credential
unSpookyCredential (PubKeyCredential pkh) = Credential.PubKeyCredential (unSpooky pkh)
unSpookyCredential (ScriptCredential hash) = Credential.ScriptCredential (unSpooky hash)

{-# INLINEABLE toSpookyCredential #-}
toSpookyCredential :: Credential.Credential -> Credential
toSpookyCredential (Credential.PubKeyCredential pkh) = PubKeyCredential (toSpooky pkh)
toSpookyCredential (Credential.ScriptCredential hash) = ScriptCredential (toSpooky hash)

data StakingCredential
  = StakingHash (Spooky Credential)
  | StakingPtr (Spooky Integer) (Spooky Integer) (Spooky Integer)
  deriving stock (Generic, Hask.Show, Hask.Eq)
PlutusTx.unstableMakeIsData ''StakingCredential
PlutusTx.makeLift ''StakingCredential

instance Eq StakingCredential where
  {-# INLINEABLE (==) #-}
  StakingHash c == StakingHash c' = c == c'
  StakingPtr a b c == StakingPtr a' b' c' =
    a == a'
      && b == b'
      && c == c'
  _ == _ = False

{-# INLINEABLE unSpookyStakingCredential #-}
unSpookyStakingCredential :: StakingCredential -> Credential.StakingCredential
unSpookyStakingCredential (StakingHash hash) = Credential.StakingHash (unSpookyCredential . unSpooky $ hash)
unSpookyStakingCredential (StakingPtr a b c) = Credential.StakingPtr (unSpooky a) (unSpooky b) (unSpooky c)

{-# INLINEABLE toSpookyStakingCredential #-}
toSpookyStakingCredential :: Credential.StakingCredential -> StakingCredential
toSpookyStakingCredential (Credential.StakingHash pkh) = StakingHash (toSpooky . toSpookyCredential $ pkh)
toSpookyStakingCredential (Credential.StakingPtr a b c) = StakingPtr (toSpooky a) (toSpooky b) (toSpooky c)

data Address = Address
  { addressCredential' :: Spooky Credential
  , addressStakingCredential' :: Spooky (Maybe StakingCredential)
  }
  deriving stock (Generic, Hask.Show, Hask.Eq)
PlutusTx.unstableMakeIsData ''Address
PlutusTx.makeLift ''Address

instance Eq Address where
  {-# INLINEABLE (==) #-}
  Address c s == Address c' s' =
    c == c'
      && s == s'

{-# INLINEABLE unSpookyAddress #-}
unSpookyAddress :: Address -> Ledger.Address
unSpookyAddress (Address cred sCred) =
  Ledger.Address (unSpookyCredential . unSpooky $ cred) (fmap unSpookyStakingCredential . unSpooky $ sCred)

{-# INLINEABLE toSpookyAddress #-}
toSpookyAddress :: Ledger.Address -> Address
toSpookyAddress (Ledger.Address cred sCred) =
  Address (toSpooky . toSpookyCredential $ cred) (toSpooky . fmap toSpookyStakingCredential $ sCred)

newtype TxId = TxId {getTxId' :: Spooky BuiltinByteString}
  deriving stock (Generic, Hask.Show, Hask.Eq)
PlutusTx.unstableMakeIsData ''TxId

instance Eq TxId where
  {-# INLINEABLE (==) #-}
  TxId a == TxId a' =
    a == a'

{-# INLINEABLE getTxId #-}
getTxId :: TxId -> BuiltinByteString
getTxId = unSpooky . getTxId'

data TxOutRef = TxOutRef
  { txOutRefId' :: Spooky TxId
  , txOutRefIdx' :: Spooky Integer
  }
  deriving stock (Generic, Hask.Show, Hask.Eq, Hask.Ord)
PlutusTx.unstableMakeIsData ''TxOutRef

instance Eq TxOutRef where
  {-# INLINEABLE (==) #-}
  TxOutRef a b == TxOutRef a' b' =
    a == a'
      && b == b'

{-# INLINEABLE txOutRefId #-}
txOutRefId :: TxOutRef -> TxId
txOutRefId = unSpooky . txOutRefId'

{-# INLINEABLE txOutRefIdx #-}
txOutRefIdx :: TxOutRef -> Integer
txOutRefIdx = unSpooky . txOutRefIdx'

data ScriptPurpose
  = Minting (Spooky CurrencySymbol)
  | Spending (Spooky TxOutRef)
  | Rewarding (Spooky StakingCredential)
  | Certifying (Spooky DCert)
  deriving stock (Generic, Hask.Show, Hask.Eq)
PlutusTx.unstableMakeIsData ''ScriptPurpose

instance Eq ScriptPurpose where
  {-# INLINEABLE (==) #-}
  Minting cs == Minting cs' = cs == cs'
  Spending ref == Spending ref' = ref == ref'
  Rewarding sc == Rewarding sc' = sc == sc'
  Certifying cert == Certifying cert' = cert == cert'
  _ == _ = False

data TxOut = TxOut
  { txOutAddress' :: Spooky Address
  , txOutValue' :: Spooky Value
  , txOutDatumHash' :: Spooky (Maybe DatumHash)
  }
  deriving stock (Hask.Eq, Generic)
PlutusTx.unstableMakeIsData ''TxOut

instance Eq TxOut where
  {-# INLINEABLE (==) #-}
  TxOut a v dh == TxOut a' v' dh' =
    a == a'
      && v == v'
      && dh == dh'

{-# INLINEABLE txOutAddress #-}
txOutAddress :: TxOut -> Address
txOutAddress = unSpooky . txOutAddress'

{-# INLINEABLE txOutValue #-}
txOutValue :: TxOut -> Value
txOutValue = unSpooky . txOutValue'

{-# INLINEABLE txOutDatumHash #-}
txOutDatumHash :: TxOut -> Maybe DatumHash
txOutDatumHash = unSpooky . txOutDatumHash'

-- | An input of a pending transaction.
data TxInInfo = TxInInfo
  { txInInfoOutRef' :: Spooky TxOutRef
  , txInInfoResolved' :: Spooky TxOut
  }
  deriving stock (Generic, Hask.Show, Hask.Eq)

PlutusTx.unstableMakeIsData ''TxInInfo

instance Eq TxInInfo where
  {-# INLINEABLE (==) #-}
  TxInInfo a b == TxInInfo a' b' =
    a == a'
      && b == b'

{-# INLINEABLE txInInfoOutRef #-}
txInInfoOutRef :: TxInInfo -> TxOutRef
txInInfoOutRef = unSpooky . txInInfoOutRef'

{-# INLINEABLE txInInfoResolved #-}
txInInfoResolved :: TxInInfo -> TxOut
txInInfoResolved = unSpooky . txInInfoResolved'

-- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
data TxInfo = TxInfo
  { -- | Transaction inputs
    txInfoInputs' :: Spooky [TxInInfo]
  , -- | Transaction outputs
    txInfoOutputs' :: Spooky [TxOut]
  , -- | The fee paid by this transaction.
    txInfoFee' :: Spooky Value
  , -- | The 'Value' minted by this transaction.
    txInfoMint' :: Spooky Value
  , -- | Digests of certificates included in this transaction
    txInfoDCert' :: Spooky [DCert]
  , -- | Withdrawals
    txInfoWdrl' :: Spooky [(StakingCredential, Integer)]
  , -- | The valid range for the transaction.
    txInfoValidRange' :: Spooky POSIXTimeRange
  , -- | Signatures provided with the transaction, attested that they all signed the tx
    txInfoSignatories' :: Spooky [PubKeyHash]
  , txInfoData' :: Spooky [(DatumHash, Datum)]
  , -- | Hash of the pending transaction (excluding witnesses)
    txInfoId' :: Spooky TxId
  }
  deriving stock (Generic, Hask.Eq)

PlutusTx.unstableMakeIsData ''TxInfo

instance Eq TxInfo where
  {-# INLINEABLE (==) #-}
  TxInfo i o f m c w r s d tid == TxInfo i' o' f' m' c' w' r' s' d' tid' =
    i == i'
      && o == o'
      && f == f'
      && m == m'
      && c == c'
      && w == w'
      && r == r'
      && s == s'
      && d == d'
      && tid == tid'

{-# INLINEABLE txInfoData #-}
txInfoData :: TxInfo -> [(DatumHash, Datum)]
txInfoData = unSpooky . txInfoData'

{-# INLINEABLE txInfoSignatories #-}
txInfoSignatories :: TxInfo -> [PubKeyHash]
txInfoSignatories = unSpooky . txInfoSignatories'

{-# INLINEABLE txInfoOutputs #-}
txInfoOutputs :: TxInfo -> [TxOut]
txInfoOutputs = unSpooky . txInfoOutputs'

{-# INLINEABLE txInfoInputs #-}
txInfoInputs :: TxInfo -> [TxInInfo]
txInfoInputs = unSpooky . txInfoInputs'

{-# INLINEABLE txInfoMint #-}
txInfoMint :: TxInfo -> Value
txInfoMint = unSpooky . txInfoMint'

{-# INLINEABLE valuePaidTo #-}

-- | Get the total value paid to a public key address by a pending transaction.
valuePaidTo :: TxInfo -> PubKeyHash -> Value
valuePaidTo ptx pkh = mconcat (pubKeyOutputsAt pkh ptx)

{-# INLINEABLE pubKeyOutputsAt #-}

-- | Get the values paid to a public key address by a pending transaction.
pubKeyOutputsAt :: PubKeyHash -> TxInfo -> [Value]
pubKeyOutputsAt pk p =
  let flt tx = case txOutAddress tx of
        (Address cred _) -> case unSpooky cred of
          PubKeyCredential pk' -> if pk == unSpooky pk' then Just (txOutValue tx) else Nothing
          _ -> Nothing
   in mapMaybe flt (txInfoOutputs p)

{-# INLINEABLE findDatum #-}

-- | Find the data corresponding to a data hash, if there is one
findDatum :: DatumHash -> TxInfo -> Maybe Datum
findDatum dsh tx = snd <$> find f (txInfoData tx)
  where
    f (dsh', _) = dsh' == dsh

data ScriptContext = ScriptContext
  { scriptContextTxInfo' :: Spooky TxInfo
  , scriptContextPurpose' :: Spooky ScriptPurpose
  }
  deriving stock (Generic, Hask.Eq)
PlutusTx.unstableMakeIsData ''ScriptContext

{-# INLINEABLE scriptContextTxInfo #-}
scriptContextTxInfo :: ScriptContext -> TxInfo
scriptContextTxInfo = unSpooky . scriptContextTxInfo'

{-# INLINEABLE scriptContextPurpose #-}
scriptContextPurpose :: ScriptContext -> ScriptPurpose
scriptContextPurpose = unSpooky . scriptContextPurpose'

{-# INLINEABLE ownCurrencySymbol #-}
ownCurrencySymbol :: ScriptContext -> CurrencySymbol
ownCurrencySymbol context =
  let purpose = scriptContextPurpose context
   in case purpose of
        Minting cs -> unSpooky cs
        _ -> error ()
