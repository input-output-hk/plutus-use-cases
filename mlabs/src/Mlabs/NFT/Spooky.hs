module Mlabs.NFT.Spooky where

import PlutusTx.Prelude
import Prelude qualified as Hask

import GHC.Generics (Generic)

import Ledger (
  Address (Address),
  CurrencySymbol,
  PubKeyHash,
  POSIXTimeRange,
  Datum,
 )

import Ledger.Value (Value)
import PlutusTx qualified
import PlutusTx.Spooky (unSpooky, Spooky)
import Ledger.Scripts (DatumHash)
import Plutus.V1.Ledger.Api (DCert, StakingCredential, Credential (PubKeyCredential))


newtype TxId = TxId { getTxId' :: Spooky BuiltinByteString }
  deriving stock (Generic, Hask.Show, Hask.Eq)
PlutusTx.unstableMakeIsData ''TxId

instance Eq TxId where
  TxId a == TxId a' =
    a == a'

data TxOutRef = TxOutRef
  { txOutRefId'  :: Spooky TxId
  , txOutRefIdx' :: Spooky Integer
  }
  deriving stock (Generic, Hask.Show, Hask.Eq)
PlutusTx.unstableMakeIsData ''TxOutRef

instance Eq TxOutRef where
  TxOutRef a b == TxOutRef a' b' =
    a == a'
    && b == b'

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
  { txOutAddress' :: Spooky Address,
    txOutValue' :: Spooky Value,
    txOutDatumHash' :: Spooky (Maybe DatumHash)
  }
  deriving stock (Hask.Eq, Generic)
PlutusTx.unstableMakeIsData ''TxOut

instance Eq TxOut where
  {-# INLINEABLE (==) #-}
  TxOut a v dh == TxOut a' v' dh' =
    a == a'
    && v ==  v'
    && dh ==  dh'

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
    { txInInfoOutRef'   :: Spooky TxOutRef
    , txInInfoResolved' :: Spooky TxOut
    } deriving stock (Generic, Hask.Show, Hask.Eq)
PlutusTx.unstableMakeIsData ''TxInInfo

instance Eq TxInInfo where
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
    txInfoInputs' :: Spooky [TxInInfo],
    -- | Transaction outputs
    txInfoOutputs' :: Spooky [TxOut],
    -- | The fee paid by this transaction.
    txInfoFee' :: Spooky Value,
    -- | The 'Value' minted by this transaction.
    txInfoMint' :: Spooky Value,
    -- | Digests of certificates included in this transaction
    txInfoDCert' :: Spooky [DCert],
    -- | Withdrawals
    txInfoWdrl' :: Spooky [(StakingCredential, Integer)],
    -- | The valid range for the transaction.
    txInfoValidRange' :: Spooky POSIXTimeRange,
    -- | Signatures provided with the transaction, attested that they all signed the tx
    txInfoSignatories' :: Spooky [PubKeyHash],
    txInfoData' :: Spooky [(DatumHash, Datum)],
    -- | Hash of the pending transaction (excluding witnesses)
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
txInfoData :: TxInfo ->  [(DatumHash, Datum)]
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

{-# INLINABLE valuePaidTo #-}
-- | Get the total value paid to a public key address by a pending transaction.
valuePaidTo :: TxInfo -> PubKeyHash -> Value
valuePaidTo ptx pkh = mconcat (pubKeyOutputsAt pkh ptx)

{-# INLINABLE pubKeyOutputsAt #-}
-- | Get the values paid to a public key address by a pending transaction.
pubKeyOutputsAt :: PubKeyHash -> TxInfo -> [Value]
pubKeyOutputsAt pk p =
    let flt tx = case txOutAddress tx of
          (Address (PubKeyCredential pk') _) -> if pk == pk' then Just (txOutValue tx) else Nothing
          _ -> Nothing
    in mapMaybe flt (txInfoOutputs p)

{-# INLINABLE findDatum #-}
-- | Find the data corresponding to a data hash, if there is one
findDatum :: DatumHash -> TxInfo -> Maybe Datum
findDatum dsh tx = snd <$> find f (txInfoData tx)
    where
        f (dsh', _) = dsh' == dsh

data ScriptContext = ScriptContext
  { scriptContextTxInfo' :: Spooky TxInfo,
    scriptContextPurpose' :: Spooky ScriptPurpose
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
