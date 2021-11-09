{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.BidAuction (
  bidAuction,
) where

import PlutusTx.Prelude hiding (mconcat, mempty, unless, (<>))
import Prelude (mconcat, (<>))
import Prelude qualified as Hask

import Control.Monad (void, when)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text)
import Text.Printf (printf)

import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusTx qualified

import Ledger (
  Datum (..),
  Redeemer (..),
  pubKeyHash,
  to,
  txId,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Ada qualified as Ada

import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

bidAuction :: NftAppSymbol -> AuctionBidParams -> Contract UserWriter s Text ()
bidAuction symbol (AuctionBidParams nftId bidAmount) = do
  ownOrefTxOut <- getUserAddr >>= fstUtxoAt
  ownPkh <- pubKeyHash <$> Contract.ownPubKey
  PointInfo {..} <- findNft nftId symbol
  node <- case pi'datum of
    NodeDatum n -> Hask.pure n
    _ -> Contract.throwError "NFT not found"

  let mauctionState = info'auctionState . node'information $ node
  when (isNothing mauctionState) $ Contract.throwError "Can't bid: no auction in progress"
  auctionState <- maybe (Contract.throwError "No auction state when expected") pure mauctionState
  when (bidAmount < as'minBid auctionState) (Contract.throwError "Auction bid lower than minimal bid")

  userUtxos <- getUserUtxos
  let newHighestBid =
        AuctionBid
          { ab'bid = bidAmount
          , ab'bidder = UserId ownPkh
          }
      newAuctionState =
        auctionState {as'highestBid = Just newHighestBid}

      nftDatum = NodeDatum $ updateDatum newAuctionState node
      nftVal = Value.singleton (app'symbol symbol) (Value.TokenName . nftId'contentHash $ nftId) 1
      action =
        BidAuctionAct
          { act'bid = bidAmount
          , act'symbol = symbol
          }
      lookups =
        mconcat
          [ Constraints.unspentOutputs userUtxos
          , Constraints.unspentOutputs $ Map.fromList [ownOrefTxOut]
          , Constraints.unspentOutputs $ Map.fromList [(pi'TOR, pi'CITxO)]
          , Constraints.typedValidatorLookups txPolicy
          , Constraints.otherScript (validatorScript txPolicy)
          ]

      bidDependentTxConstraints =
        case as'highestBid auctionState of
          Nothing -> []
          Just (AuctionBid bid bidder) ->
            [ Constraints.mustPayToPubKey (getUserId bidder) (Ada.lovelaceValueOf bid)
            ]
      tx =
        mconcat
          ( [ Constraints.mustPayToTheScript nftDatum (nftVal <> Ada.lovelaceValueOf bidAmount)
            , Constraints.mustIncludeDatum (Datum . PlutusTx.toBuiltinData $ nftDatum)
            , Constraints.mustSpendPubKeyOutput (fst ownOrefTxOut)
            , Constraints.mustSpendScriptOutput
                pi'TOR
                (Redeemer . PlutusTx.toBuiltinData $ action)
            , Constraints.mustValidateIn (to $ as'deadline auctionState)
            ]
              ++ bidDependentTxConstraints
          )
  ledgerTx <- Contract.submitTxConstraintsWith @NftTrade lookups tx
  Contract.tell . Last . Just . Left $ nftId
  -- void $ Contract.logInfo @Hask.String $ printf "DEBUG open auction TX: %s" (Hask.show ledgerTx)
  void $ Contract.logInfo @Hask.String $ printf "Bidding in auction for %s" $ Hask.show nftVal
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  void $ Contract.logInfo @Hask.String $ printf "Confirmed bid auction for %s" $ Hask.show nftVal
  where
    updateDatum newAuctionState node =
      node
        { node'information =
            (node'information node)
              { info'auctionState = Just newAuctionState
              }
        }
