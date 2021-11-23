{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.Buy (
  buy,
) where

import Prelude (mconcat)
import Prelude qualified as Hask

import Control.Lens ((^.))
import Control.Monad (void, when)
import Data.Map qualified as Map
import Data.Monoid (Last (..), (<>))
import Data.Text (Text)

import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Constraints qualified as Constraints
import PlutusTx qualified
import PlutusTx.Prelude hiding (mconcat, mempty, (<>))

import Ledger (
  Datum (..),
  Redeemer (..),
  ciTxOutValue,
 )
import Ledger.Typed.Scripts (validatorScript)

import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Contract.Fees
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

{- | BUY.
 Attempts to buy a new NFT by changing the owner, pays the current owner and
 the author, and sets a new price for the NFT.
-}
buy :: forall s. NftAppSymbol -> BuyRequestUser -> Contract UserWriter s Text ()
buy symbol BuyRequestUser {..} = do
  ownOrefTxOut <- getUserAddr >>= fstUtxoAt
  ownPkh <- Contract.ownPubKeyHash
  nftPi <- findNft ur'nftId symbol
  node <- case pi'data nftPi of
    NodeDatum n -> Hask.pure n
    _ -> Contract.throwError "NFT not found"
  price <- case info'price . node'information $ node of
    Nothing -> Contract.throwError "NFT not for sale."
    Just price -> Hask.pure price

  when (ur'price < price) $
    Contract.logError @Hask.String "Bid price is too low."

  userUtxos <- getUserUtxos
  feeRate <- getCurrFeeRate symbol

  (govTx, govLookups) <- getFeesConstraints symbol ur'nftId ur'price

  let feeValue = round $ fromInteger ur'price * feeRate
      (paidToOwner, paidToAuthor) =
        calculateShares (ur'price - feeValue) . info'share . node'information $ node
      nftDatum = NodeDatum $ updateNftDatum ownPkh node
      nftVal = pi'CITxO nftPi ^. ciTxOutValue
      action =
        BuyAct
          { act'bid = ur'price
          , act'newPrice = ur'newPrice
          , act'symbol = symbol
          }
      lookups =
        mconcat $
          [ Constraints.unspentOutputs userUtxos
          , Constraints.unspentOutputs $ Map.fromList [ownOrefTxOut]
          , Constraints.unspentOutputs $ Map.fromList [(pi'TOR nftPi, pi'CITxO nftPi)]
          , Constraints.typedValidatorLookups txPolicy
          , Constraints.otherScript (validatorScript txPolicy)
          ]
            <> govLookups
      tx =
        mconcat $
          [ Constraints.mustPayToTheScript nftDatum nftVal
          , Constraints.mustIncludeDatum (Datum . PlutusTx.toBuiltinData $ nftDatum)
          , Constraints.mustPayToPubKey (getUserId . info'author . node'information $ node) paidToAuthor
          , Constraints.mustPayToPubKey (getUserId . info'owner . node'information $ node) paidToOwner
          , Constraints.mustSpendPubKeyOutput (fst ownOrefTxOut)
          , Constraints.mustSpendScriptOutput
              (pi'TOR nftPi)
              (Redeemer . PlutusTx.toBuiltinData $ action)
          ]
            <> govTx
  void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
  Contract.tell . Last . Just . Left $ ur'nftId
  Contract.logInfo @Hask.String "buy successful!"
  where
    updateNftDatum newOwner node =
      node
        { node'information =
            (node'information node)
              { info'price = ur'newPrice
              , info'owner = UserId newOwner
              }
        }
