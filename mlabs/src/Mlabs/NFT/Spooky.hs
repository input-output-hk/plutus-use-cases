{-# OPTIONS_GHC -Wno-orphans #-}

module Mlabs.NFT.Spooky (
  PubKeyHash (..),
  getPubKeyHash,
  toSpookyPubKeyHash,
  unSpookyPubKeyHash,
  DatumHash (..),
  getDatumHash,
  CurrencySymbol (..),
  toSpookyCurrencySymbol,
  unSpookyCurrencySymbol,
  TokenName (..),
  toSpookyTokenName,
  unSpookyTokenName,
  unTokenName,
  Value (..),
  unSpookyValue,
  flattenValue,
  singleton,
  valueOf,
  lovelaceValueOf,
  symbols,
  adaSymbol,
  adaToken,
  AssetClass (..),
  toSpookyAssetClass,
  unSpookyAssetClass,
  unAssetClass,
  assetClass,
  assetClassValue,
  assetClassValueOf,
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

import Control.Monad (guard)
import Data.OpenApi.Schema qualified as OpenApi
import Ledger (
  Datum,
  POSIXTimeRange,
  ValidatorHash,
 )
import Ledger qualified
import Ledger.Crypto qualified as Crypto
import Playground.Contract (FromJSON, ToJSON, ToSchema)
import Plutus.V1.Ledger.Api (DCert)
import Plutus.V1.Ledger.Credential qualified as Credential
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as Map
import PlutusTx.Spooky
import PlutusTx.These (These (..))
import Schema (ToSchema (toSchema))

instance ToSchema BuiltinData where
  toSchema = toSchema @Hask.String

newtype PubKeyHash = PubKeyHash {getPubKeyHash' :: Spooky BuiltinByteString}
  deriving stock (Generic, Hask.Show)
  deriving newtype (Hask.Eq, Hask.Ord, Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.makeLift ''PubKeyHash

instance Ord PubKeyHash where
  {-# INLINEABLE compare #-}
  compare h h' = compare (getPubKeyHash h) (getPubKeyHash h')

{-# INLINEABLE getPubKeyHash #-}
getPubKeyHash :: PubKeyHash -> BuiltinByteString
getPubKeyHash = unSpooky . getPubKeyHash'

{-# INLINEABLE toSpookyPubKeyHash #-}
toSpookyPubKeyHash :: Crypto.PubKeyHash -> PubKeyHash
toSpookyPubKeyHash (Crypto.PubKeyHash hash) = PubKeyHash . toSpooky $ hash

{-# INLINEABLE unSpookyPubKeyHash #-}
unSpookyPubKeyHash :: PubKeyHash -> Crypto.PubKeyHash
unSpookyPubKeyHash (PubKeyHash hash) = Crypto.PubKeyHash . unSpooky $ hash

newtype DatumHash = DatumHash {getDatumHash' :: Spooky BuiltinByteString}
  deriving stock (Generic)
  deriving newtype (Hask.Eq, Hask.Ord, Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

{-# INLINEABLE getDatumHash #-}
getDatumHash :: DatumHash -> BuiltinByteString
getDatumHash = unSpooky . getDatumHash'

newtype CurrencySymbol = CurrencySymbol {unCurrencySymbol' :: Spooky BuiltinByteString}
  deriving stock (Generic, Hask.Show)
  deriving newtype (Hask.Eq, Hask.Ord, Eq, PlutusTx.UnsafeFromData, PlutusTx.FromData, PlutusTx.ToData)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.makeLift ''CurrencySymbol

instance Ord CurrencySymbol where
  {-# INLINEABLE compare #-}
  compare cs cs' = compare (unCurrencySymbol cs) (unCurrencySymbol cs')

{-# INLINEABLE unCurrencySymbol #-}
unCurrencySymbol :: CurrencySymbol -> BuiltinByteString
unCurrencySymbol = unSpooky . unCurrencySymbol'

{-# INLINEABLE toSpookyCurrencySymbol #-}
toSpookyCurrencySymbol :: Ledger.CurrencySymbol -> CurrencySymbol
toSpookyCurrencySymbol (Value.CurrencySymbol cs) = CurrencySymbol . toSpooky $ cs

{-# INLINEABLE unSpookyCurrencySymbol #-}
unSpookyCurrencySymbol :: CurrencySymbol -> Ledger.CurrencySymbol
unSpookyCurrencySymbol (CurrencySymbol cs) = Value.CurrencySymbol . unSpooky $ cs

newtype TokenName = TokenName {unTokenName' :: Spooky BuiltinByteString}
  deriving stock (Generic, Hask.Show)
  deriving newtype (Hask.Eq, Hask.Ord, Eq, PlutusTx.UnsafeFromData, PlutusTx.FromData, PlutusTx.ToData)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
PlutusTx.makeLift ''TokenName

instance Ord TokenName where
  {-# INLINEABLE compare #-}
  compare tn tn' = compare (unTokenName tn) (unTokenName tn')

{-# INLINEABLE unTokenName #-}
unTokenName :: TokenName -> BuiltinByteString
unTokenName = unSpooky . unTokenName'

{-# INLINEABLE toSpookyTokenName #-}
toSpookyTokenName :: Ledger.TokenName -> TokenName
toSpookyTokenName (Value.TokenName tn) = TokenName . toSpooky $ tn

{-# INLINEABLE unSpookyTokenName #-}
unSpookyTokenName :: TokenName -> Ledger.TokenName
unSpookyTokenName (TokenName tn) = Value.TokenName . unSpooky $ tn

newtype AssetClass = AssetClass {unAssetClass' :: Spooky (CurrencySymbol, TokenName)}
  deriving stock (Generic, Hask.Show)
  deriving newtype (Hask.Eq, Hask.Ord, Eq, PlutusTx.UnsafeFromData, PlutusTx.FromData, PlutusTx.ToData)
  deriving anyclass (ToJSON, FromJSON, ToSchema, OpenApi.ToSchema)
PlutusTx.makeLift ''AssetClass

instance Ord AssetClass where
  {-# INLINEABLE compare #-}
  compare ac ac' = compare (unAssetClass ac) (unAssetClass ac')

{-# INLINEABLE unAssetClass #-}
unAssetClass :: AssetClass -> (CurrencySymbol, TokenName)
unAssetClass = unSpooky . unAssetClass'

{-# INLINEABLE toSpookyAssetClass #-}
toSpookyAssetClass :: Ledger.AssetClass -> AssetClass
toSpookyAssetClass ac =
  let (c, t) = Value.unAssetClass ac
   in assetClass (toSpookyCurrencySymbol c) (toSpookyTokenName t)

{-# INLINEABLE unSpookyAssetClass #-}
unSpookyAssetClass :: AssetClass -> Ledger.AssetClass
unSpookyAssetClass ac =
  let (c, t) = unAssetClass ac
   in Value.assetClass (unSpookyCurrencySymbol c) (unSpookyTokenName t)

{-# INLINEABLE assetClass #-}
assetClass :: CurrencySymbol -> TokenName -> AssetClass
assetClass s t = AssetClass $ toSpooky (s, t)

newtype Value = Value {getValue' :: Map.Map CurrencySymbol (Map.Map TokenName Integer)}
  deriving stock (Generic)
  deriving newtype (PlutusTx.UnsafeFromData, PlutusTx.FromData, PlutusTx.ToData)
  deriving anyclass (ToJSON, FromJSON)
PlutusTx.makeLift ''Value

instance Hask.Semigroup Value where
  (<>) = unionWith (+)

instance Semigroup Value where
  {-# INLINEABLE (<>) #-}
  (<>) = unionWith (+)

instance Hask.Monoid Value where
  mempty = Value Map.empty

instance Monoid Value where
  {-# INLINEABLE mempty #-}
  mempty = Value Map.empty

instance Hask.Eq Value where
  (==) = eq

instance Eq Value where
  {-# INLINEABLE (==) #-}
  (==) = eq

{-# INLINEABLE unSpookyValue #-}
unSpookyValue :: Value -> Value.Value
unSpookyValue =
  mconcat
    . fmap (\(cs, tn, v) -> Value.singleton (unSpookyCurrencySymbol cs) (unSpookyTokenName tn) v)
    . flattenValue

{-# INLINEABLE checkPred #-}
checkPred :: (These Integer Integer -> Bool) -> Value -> Value -> Bool
checkPred f l r =
  let inner :: Map.Map TokenName (These Integer Integer) -> Bool
      inner = Map.all f
   in Map.all inner (unionVal l r)

{-# INLINEABLE checkBinRel #-}

{- | Check whether a binary relation holds for value pairs of two 'Value' maps,
   supplying 0 where a key is only present in one of them.
-}
checkBinRel :: (Integer -> Integer -> Bool) -> Value -> Value -> Bool
checkBinRel f l r =
  let unThese k' = case k' of
        This a -> f a 0
        That b -> f 0 b
        These a b -> f a b
   in checkPred unThese l r

{-# INLINEABLE eq #-}

-- | Check whether one 'Value' is equal to another. See 'Value' for an explanation of how operations on 'Value's work.
eq :: Value -> Value -> Bool
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
eq = checkBinRel (==)

{-# INLINEABLE unionVal #-}

-- | Combine two 'Value' maps
unionVal :: Value -> Value -> Map.Map CurrencySymbol (Map.Map TokenName (These Integer Integer))
unionVal (Value l) (Value r) =
  let combined = Map.union l r
      unThese k = case k of
        This a -> This <$> a
        That b -> That <$> b
        These a b -> Map.union a b
   in unThese <$> combined

{-# INLINEABLE unionWith #-}
unionWith :: (Integer -> Integer -> Integer) -> Value -> Value -> Value
unionWith f ls rs =
  let combined = unionVal ls rs
      unThese k' = case k' of
        This a -> f a 0
        That b -> f 0 b
        These a b -> f a b
   in Value (fmap (fmap unThese) combined)

{-# INLINEABLE flattenValue #-}
flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
flattenValue (Value v) = do
  (cs, m) <- Map.toList v
  (tn, a) <- Map.toList m
  guard $ a /= 0
  return (cs, tn, a)

{-# INLINEABLE singleton #-}

-- | Make a 'Value' containing only the given quantity of the given currency.
singleton :: CurrencySymbol -> TokenName -> Integer -> Value
singleton c tn i = Value (Map.singleton c (Map.singleton tn i))

{-# INLINEABLE valueOf #-}

-- | Get the quantity of the given currency in the 'Value'.
valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
valueOf (Value mp) cur tn =
  case Map.lookup cur mp of
    Nothing -> 0 :: Integer
    Just i -> fromMaybe 0 (Map.lookup tn i)

{-# INLINEABLE lovelaceValueOf #-}
lovelaceValueOf :: Integer -> Value
lovelaceValueOf = singleton adaSymbol adaToken

{-# INLINEABLE symbols #-}

-- | The list of 'CurrencySymbol's of a 'Value'.
symbols :: Value -> [CurrencySymbol]
symbols (Value mp) = Map.keys mp

{-# INLINEABLE assetClassValue #-}

-- | A 'Value' containing the given amount of the asset class.
assetClassValue :: AssetClass -> Integer -> Value
assetClassValue ac i =
  let (c, t) = unAssetClass ac
   in singleton c t i

{-# INLINEABLE assetClassValueOf #-}

-- | Get the quantity of the given 'AssetClass' class in the 'Value'.
assetClassValueOf :: Value -> AssetClass -> Integer
assetClassValueOf v ac =
  let (c, t) = unAssetClass ac
   in valueOf v c t

{-# INLINEABLE adaSymbol #-}
adaSymbol :: CurrencySymbol
adaSymbol = CurrencySymbol . toSpooky @BuiltinByteString $ ""

{-# INLINEABLE adaToken #-}
adaToken :: TokenName
adaToken = TokenName . toSpooky @BuiltinByteString $ ""

data Credential
  = PubKeyCredential (Spooky PubKeyHash)
  | ScriptCredential (Spooky ValidatorHash)
  deriving stock (Generic, Hask.Show, Hask.Eq)
PlutusTx.unstableMakeIsData ''Credential
PlutusTx.makeLift ''Credential

instance Eq Credential where
  {-# INLINEABLE (==) #-}
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
