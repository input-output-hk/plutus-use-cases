{-# LANGUAGE UndecidableInstances #-}
-- FIXME: Remove after uncommenting commented parts
{-# OPTIONS_GHC -Wno-unused-imports #-}

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
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))

import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Contract.Gov.Fees
import Mlabs.NFT.Contract.Gov.Query
import Mlabs.NFT.Spooky (toSpooky)
import Mlabs.NFT.Types
import Mlabs.NFT.Validation
import Plutus.Contracts.Auction (auctionBuyer)

{- |
  Attempts to close NFT auction, checks if owner is closing an auction and deadline passed,
  pays from script to previous owner, and sets new owner.
-}
closeAuction :: UniqueToken -> AuctionCloseParams -> Contract UserWriter s Text ()
closeAuction _ _ =
  error ()

-- closeAuction uT (AuctionCloseParams nftId) = do
-- ownOrefTxOut <- getUserAddr >>= fstUtxoAt
-- PointInfo {..} <- findNft nftId uT
-- node <- case pi'data of
--   NodeDatum n -> Hask.pure n
--   _ -> Contract.throwError "NFT not found"

-- let mauctionState = info'auctionState . node'information $ node

-- auctionState <- maybe (Contract.throwError "Can't close: no auction in progress") pure mauctionState

-- userUtxos <- getUserUtxos
-- (bidDependentTxConstraints, bidDependentLookupConstraints) <- getBidDependentConstraints auctionState node

-- symbol <- getNftAppSymbol uT

-- let newOwner = case as'highestBid auctionState of
--       Nothing -> info'owner . node'information $ node
--       Just (AuctionBid _ bidder) -> bidder

--     nftDatum = NodeDatum $ updateDatum newOwner node
--     nftVal = Value.singleton (app'symbol symbol) (Value.TokenName . nftId'contentHash $ nftId) 1
--     action =
--       CloseAuctionAct
--         { act'symbol' = toSpooky symbol
--         }
--     lookups =
--       mconcat $
--         [ Constraints.unspentOutputs userUtxos
--         , Constraints.unspentOutputs $ Map.fromList [ownOrefTxOut]
--         , Constraints.unspentOutputs $ Map.fromList [(pi'TOR, pi'CITxO)]
--         , Constraints.typedValidatorLookups (txPolicy uT)
--         , Constraints.otherScript (validatorScript $ txPolicy uT)
--         ]
--           <> bidDependentLookupConstraints

--     tx =
--       mconcat $
--         [ Constraints.mustPayToTheScript (toBuiltinData nftDatum) nftVal
--         , Constraints.mustIncludeDatum (Datum . PlutusTx.toBuiltinData $ nftDatum)
--         , Constraints.mustSpendPubKeyOutput (fst ownOrefTxOut)
--         , Constraints.mustSpendScriptOutput
--             pi'TOR
--             (Redeemer . PlutusTx.toBuiltinData $ action)
--         , Constraints.mustValidateIn (from $ as'deadline auctionState)
--         ]
--           <> bidDependentTxConstraints

-- void $ Contract.submitTxConstraintsWith lookups tx
-- Contract.tell . Last . Just . Left $ nftId
-- void $ Contract.logInfo @Hask.String $ printf "Closing auction for %s" $ Hask.show nftVal
-- where
--   updateDatum newOwner node =
--     node
--       { node'information' = toSpooky $
--           (node'information node)
--             { info'owner' = toSpooky newOwner
--             , info'auctionState' = toSpooky Nothing
--             }
--       }

--   -- If someone bid on auction, returns constrains to pay to owner, author, mint GOV, and pay fees
--   getBidDependentConstraints auctionState node = case as'highestBid auctionState of
--     Nothing -> Hask.pure ([], [])
--     Just auctionBid -> do
--       feeRate <- queryCurrFeeRate uT
--       let bid = ab'bid auctionBid
--           feeValue = round $ fromInteger bid * feeRate
--           (amountPaidToOwner, amountPaidToAuthor) =
--             calculateShares (bid - feeValue) (info'share . node'information $ node)
--           payTx =
--             [ Constraints.mustPayToPubKey
--                 (getUserId . info'owner . node'information $ node)
--                 amountPaidToOwner
--             , Constraints.mustPayToPubKey
--                 (getUserId . info'author . node'information $ node)
--                 amountPaidToAuthor
--             ]
--       (govTx, govLookups) <- getFeesConstraints uT nftId bid bidder'
--       Hask.pure (govTx <> payTx, govLookups)
