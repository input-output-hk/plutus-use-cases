{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Mlabs.EfficientNFT.Lock (mkValidator, lockValidator) where

import PlutusTx qualified
import PlutusTx.Prelude

import Data.Default (def)
import Ledger (
  CurrencySymbol,
  Extended (Finite),
  LowerBound (LowerBound),
  PaymentPubKeyHash,
  ScriptContext,
  Slot (Slot),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoMint, txInfoOutputs, txInfoValidRange),
  TxOut (txOutDatumHash, txOutValue),
  findDatum,
  findOwnInput,
  getContinuingOutputs,
  getDatum,
  ivFrom,
  mkValidatorScript,
  scriptContextTxInfo,
  txSignedBy,
  unPaymentPubKeyHash,
 )
import Ledger.TimeSlot (posixTimeToEnclosingSlot)
import Ledger.Typed.Scripts (Any, TypedValidator, unsafeMkTypedValidator, wrapValidator)
import Ledger.Value (Value (getValue), valueOf)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Natural (Natural)

import Mlabs.EfficientNFT.Token (mkTokenName)
import Mlabs.EfficientNFT.Types

{-# INLINEABLE mkValidator #-}
mkValidator :: CurrencySymbol -> Integer -> Slot -> LockDatum -> LockAct -> ScriptContext -> Bool
mkValidator _ lockup lockupEnd inDatum act ctx =
  case act of
    Unstake pkh price ->
      traceIfFalse "CO must not exists" checkNoCO
        && traceIfFalse "sgNFT must be burned" (checkSgBurned price pkh)
        && if ld'entered inDatum < lockupEnd
          then traceIfFalse "Current slot smaller than lockupEnd" checkCurrentSlotFirst
          else traceIfFalse "Current slot smaller than lockup+entered" checkCurrentSlot
    Restake pkh price ->
      traceIfFalse "Cannot mint sg" checkNoSgMinted
        && traceIfFalse "Values in CO cannot change" checkSameCOValues
        && traceIfFalse "Owner must sign the transaction" (txSignedBy info . unPaymentPubKeyHash $ pkh)
        && traceIfFalse "Input does not contain sg" (checkInputContainsSg price pkh)
        && if ld'entered inDatum < lockupEnd
          then
            traceIfFalse "Current slot smaller than lockupEnd" checkCurrentSlotFirst
              && traceIfFalse "Inconsistent datum" checkConsistentDatumRestakeFirst
          else
            traceIfFalse "Current slot smaller than lockup+entered" checkCurrentSlot
              && traceIfFalse "Inconsistent datum" checkConsistentDatumRestake
  where
    -- Helpers
    traceIfNothing :: BuiltinString -> Maybe a -> a
    traceIfNothing err = fromMaybe (traceError err)
    -- traceIfNothing _ = fromMaybe (error ())

    -- Bindings

    inTx :: TxOut
    inTx = txInInfoResolved . traceIfNothing "Own input missing" . findOwnInput $ ctx

    outTx :: TxOut
    outTx =
      ( \case
          [] -> traceError "No CO"
          [tx] -> tx
          _ -> traceError "More than one CO"
      )
        . getContinuingOutputs
        $ ctx

    outDatum :: LockDatum
    outDatum =
      traceIfNothing "Invalid CO datum format"
        . PlutusTx.fromBuiltinData
        . getDatum
        . traceIfNothing "Missing output datum"
        . flip findDatum info
        . traceIfNothing "Missing output datum hash"
        . txOutDatumHash
        $ outTx

    currentSlot :: Slot
    currentSlot =
      let extract (LowerBound (Finite x) _) = x
          extract _ = traceError "Valid range beginning must be finite"
       in posixTimeToEnclosingSlot def . extract . ivFrom . txInfoValidRange $ info

    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Checks

    -- Checks that value in CO does not change
    checkSameCOValues :: Bool
    checkSameCOValues = txOutValue inTx == txOutValue outTx

    -- Checks that no Seabug NFT is minted
    checkNoSgMinted :: Bool
    checkNoSgMinted = isNothing . AssocMap.lookup (ld'sgNft inDatum) . getValue . txInfoMint $ info

    -- Checks that input contains corresponding Seabug NFT
    checkInputContainsSg :: Natural -> PaymentPubKeyHash -> Bool
    checkInputContainsSg price owner =
      let tn = mkTokenName $ NftId (ld'underlyingTn inDatum) price owner
          containsSg tx = valueOf (txOutValue tx) (ld'sgNft inDatum) tn == 1
       in any containsSg . txInfoOutputs $ info

    -- Checks that entered slot is properly updated
    checkConsistentDatumRestake :: Bool
    checkConsistentDatumRestake =
      let validDatum = inDatum {ld'entered = ld'entered inDatum + Slot lockup}
       in validDatum == outDatum

    -- Checks that entered slot is properly updated when restaking for the first time
    checkConsistentDatumRestakeFirst :: Bool
    checkConsistentDatumRestakeFirst =
      let validDatum = inDatum {ld'entered = lockupEnd}
       in validDatum == outDatum

    checkCurrentSlot :: Bool
    checkCurrentSlot = currentSlot > Slot lockup + ld'entered inDatum

    checkCurrentSlotFirst :: Bool
    checkCurrentSlotFirst = currentSlot > lockupEnd

    checkNoCO :: Bool
    checkNoCO = null . getContinuingOutputs $ ctx

    checkSgBurned :: Natural -> PaymentPubKeyHash -> Bool
    checkSgBurned price owner =
      let tn = mkTokenName $ NftId (ld'underlyingTn inDatum) price owner
       in valueOf (txInfoMint info) (ld'sgNft inDatum) tn == (-1)

lockValidator :: CurrencySymbol -> Integer -> Slot -> TypedValidator Any
lockValidator underlyingCs lockup lockupEnd = unsafeMkTypedValidator v
  where
    v =
      mkValidatorScript
        ( $$(PlutusTx.compile [||wrap||])
            `PlutusTx.applyCode` ( $$(PlutusTx.compile [||mkValidator||])
                                    `PlutusTx.applyCode` PlutusTx.liftCode underlyingCs
                                    `PlutusTx.applyCode` PlutusTx.liftCode lockup
                                    `PlutusTx.applyCode` PlutusTx.liftCode lockupEnd
                                 )
        )
    wrap = wrapValidator @LockDatum @LockAct
