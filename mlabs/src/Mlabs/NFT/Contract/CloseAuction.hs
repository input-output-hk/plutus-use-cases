{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.CloseAuction (
  closeAuction,
) where

import PlutusTx.Prelude hiding (mconcat, mempty, unless, (<>))
import Prelude (mconcat)
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Data.Monoid (Last (..), (<>))
import Data.Text (Text)
import Text.Printf (printf)

import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusTx qualified

import Ledger (
  Datum (..),
  Redeemer (..),
  from,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value qualified as Value

import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Contract.Gov.Fees
import Mlabs.NFT.Contract.Gov.Query
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

{- |
  Attempts to close NFT auction, checks if owner is closing an auction and deadline passed,
  pays from script to previous owner, and sets new owner.
-}
closeAuction :: UniqueToken -> AuctionCloseParams -> Contract UserWriter s Text ()
closeAuction uT (AuctionCloseParams nftId) = do
  ownOrefTxOut <- getUserAddr >>= fstUtxoAt
  PointInfo {..} <- findNft nftId uT
  node <- case pi'data of
    NodeDatum n -> Hask.pure n
    _ -> Contract.throwError "NFT not found"

  let mauctionState = info'auctionState . node'information $ node

  auctionState <- maybe (Contract.throwError "Can't close: no auction in progress") pure mauctionState

  userUtxos <- getUserUtxos
  (bidDependentTxConstraints, bidDependentLookupConstraints) <- getBidDependentConstraints auctionState node

  symbol <- getNftAppSymbol uT

  let newOwner = case as'highestBid auctionState of
        Nothing -> info'owner . node'information $ node
        Just (AuctionBid _ bidder) -> bidder

      nftDatum = NodeDatum $ updateDatum newOwner node
      nftVal = Value.singleton (app'symbol symbol) (Value.TokenName . nftId'contentHash $ nftId) 1
      action =
        CloseAuctionAct
          { act'symbol = symbol
          }
      lookups =
        mconcat $
          [ Constraints.unspentOutputs userUtxos
          , Constraints.unspentOutputs $ Map.fromList [ownOrefTxOut]
          , Constraints.unspentOutputs $ Map.fromList [(pi'TOR, pi'CITxO)]
          , Constraints.typedValidatorLookups (txPolicy uT)
          , Constraints.otherScript (validatorScript $ txPolicy uT)
          ]
            <> bidDependentLookupConstraints

      tx =
        mconcat $
          [ Constraints.mustPayToTheScript nftDatum nftVal
          , Constraints.mustIncludeDatum (Datum . PlutusTx.toBuiltinData $ nftDatum)
          , Constraints.mustSpendPubKeyOutput (fst ownOrefTxOut)
          , Constraints.mustSpendScriptOutput
              pi'TOR
              (Redeemer . PlutusTx.toBuiltinData $ action)
          , Constraints.mustValidateIn (from $ as'deadline auctionState)
          ]
            <> bidDependentTxConstraints

  void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
  Contract.tell . Last . Just . Left $ nftId
  void $ Contract.logInfo @Hask.String $ printf "Closing auction for %s" $ Hask.show nftVal
  where
    updateDatum newOwner node =
      node
        { node'information =
            (node'information node)
              { info'owner = newOwner
              , info'auctionState = Nothing
              }
        }

    -- If someone bid on auction, returns constrains to pay to owner, author, mint GOV, and pay fees
    getBidDependentConstraints auctionState node = case as'highestBid auctionState of
      Nothing -> Hask.pure ([], [])
      Just (AuctionBid bid _bidder) -> do
        feeRate <- queryCurrFeeRate uT
        let feeValue = round $ fromInteger bid * feeRate
            (amountPaidToOwner, amountPaidToAuthor) =
              calculateShares (bid - feeValue) (info'share . node'information $ node)
            payTx =
              [ Constraints.mustPayToPubKey
                  (getUserId . info'owner . node'information $ node)
                  amountPaidToOwner
              , Constraints.mustPayToPubKey
                  (getUserId . info'author . node'information $ node)
                  amountPaidToAuthor
              ]
        (govTx, govLookups) <- getFeesConstraints uT nftId bid _bidder
        Hask.pure (govTx <> payTx, govLookups)
