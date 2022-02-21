module Mlabs.EfficientNFT.Contract.Burn (burn) where

import PlutusTx.Prelude hiding (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Default (def)
import Ledger (
  Redeemer (Redeemer),
  Extended (Finite, PosInf),
  Interval (Interval),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
  _ciTxOutValue,
  minAdaTxOut,
  scriptHashAddress
 )
import Ledger.Constraints qualified as Constraints
import Ledger.Contexts (scriptCurrencySymbol)
import Ledger.Typed.Scripts (Any, validatorScript)
import Plutus.Contract qualified as Contract
import Plutus.V1.Ledger.Ada (toValue)
import Plutus.V1.Ledger.Api (toBuiltinData)
import Plutus.V1.Ledger.Value (singleton, valueOf)
import Text.Printf (printf)
import Ledger.TimeSlot (slotToBeginPOSIXTime)

import Mlabs.EfficientNFT.Contract.Aux (getAddrUtxos, getUserUtxos)
import Mlabs.EfficientNFT.Token (mkTokenName, policy)
import Mlabs.EfficientNFT.Types
import Mlabs.EfficientNFT.Lock (lockValidator)

burn :: NftData -> UserContract ()
burn nftData = do
  pkh <- Contract.ownPaymentPubKeyHash
  currSlot <- Contract.currentSlot
  let collection = nftData'nftCollection nftData
      policy' = policy collection
      curr = scriptCurrencySymbol policy'
      lockValidator' =
        lockValidator
          (nftCollection'collectionNftCs collection)
          (nftCollection'lockLockup collection)
          (nftCollection'lockLockupEnd collection)
      nft = nftData'nftId nftData
      name = mkTokenName nft
      nftValue = singleton curr name (-1)
      cnftValue = singleton cnftCs cnftTn 1
      mintRedeemer = Redeemer . toBuiltinData $ BurnToken nft
      cnftCs = nftCollection'collectionNftCs collection
      cnftTn = nftId'collectionNftTn nft
      containsCnft (_, tx) = valueOf (_ciTxOutValue tx) cnftCs cnftTn == 1
      now = slotToBeginPOSIXTime def currSlot
      validRange = Interval (LowerBound (Finite now) True) (UpperBound PosInf False)
  utxo' <- find containsCnft . Map.toList <$> (getAddrUtxos $ scriptHashAddress $ nftCollection'lockingScript $ collection)
  (utxo, utxoIndex) <- case utxo' of
    Nothing -> do
      Contract.throwError "NFT not found in locking address"
    Just x -> Hask.pure x
  userUtxos <- getUserUtxos
  let lookup =
        Hask.mconcat
          [ Constraints.mintingPolicy policy'
          , Constraints.typedValidatorLookups lockValidator'
          , Constraints.otherScript (validatorScript lockValidator')
          , Constraints.unspentOutputs $ Map.insert utxo utxoIndex userUtxos
          , Constraints.ownPaymentPubKeyHash pkh
          ]
      tx =
        Hask.mconcat
          [ Constraints.mustMintValueWithRedeemer mintRedeemer nftValue
          , Constraints.mustBeSignedBy pkh
          , Constraints.mustSpendScriptOutput utxo (Redeemer $ toBuiltinData $ Unstake (nftId'owner nft) (nftId'price nft))
          , Constraints.mustPayToPubKey pkh (cnftValue <> toValue minAdaTxOut)
          , Constraints.mustValidateIn validRange
          ]
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.logInfo @Hask.String $ printf "Burn successful"
