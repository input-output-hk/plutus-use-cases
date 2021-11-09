{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.CloseAuction (
  closeAuction,
) where

import PlutusTx.Prelude hiding (mconcat, mempty, unless, (<>))
import Prelude (mconcat)
import Prelude qualified as Hask

import Control.Monad (unless, void, when)
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
  from,
  pubKeyHash,
  txId,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)
import Ledger.Value qualified as Value

import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

closeAuction :: NftAppSymbol -> AuctionCloseParams -> Contract UserWriter s Text ()
closeAuction symbol (AuctionCloseParams nftId) = do
  ownOrefTxOut <- getUserAddr >>= fstUtxoAt
  ownPkh <- pubKeyHash <$> Contract.ownPubKey
  PointInfo {..} <- findNft nftId symbol
  node <- case pi'datum of
    NodeDatum n -> Hask.pure n
    _ -> Contract.throwError "NFT not found"

  let mauctionState = info'auctionState . node'information $ node
      isOwner = ownPkh == (getUserId . info'owner . node'information) node

  when (isNothing mauctionState) $ Contract.throwError "Can't close: no auction in progress"
  auctionState <- maybe (Contract.throwError "No auction state when expected") pure mauctionState
  unless isOwner $ Contract.throwError "Only owner can close auction"

  userUtxos <- getUserUtxos
  let newOwner =
        case as'highestBid auctionState of
          Nothing -> info'owner . node'information $ node
          Just (AuctionBid _ bidder) -> bidder

      nftDatum = NodeDatum $ updateDatum newOwner node
      nftVal = Value.singleton (app'symbol symbol) (Value.TokenName . nftId'contentHash $ nftId) 1
      action =
        CloseAuctionAct
          { act'symbol = symbol
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
          Just (AuctionBid bid _bidder) ->
            let (amountPaidToOwner, amountPaidToAuthor) = calculateShares bid (info'share . node'information $ node)
             in [ Constraints.mustPayToPubKey (getUserId . info'owner . node'information $ node) amountPaidToOwner
                , Constraints.mustPayToPubKey (getUserId . info'author . node'information $ node) amountPaidToAuthor
                ]
      tx =
        mconcat
          ( [ Constraints.mustPayToTheScript nftDatum nftVal
            , Constraints.mustIncludeDatum (Datum . PlutusTx.toBuiltinData $ nftDatum)
            , Constraints.mustSpendPubKeyOutput (fst ownOrefTxOut)
            , Constraints.mustSpendScriptOutput
                pi'TOR
                (Redeemer . PlutusTx.toBuiltinData $ action)
            , Constraints.mustValidateIn (from $ as'deadline auctionState)
            ]
              ++ bidDependentTxConstraints
          )
  ledgerTx <- Contract.submitTxConstraintsWith @NftTrade lookups tx
  Contract.tell . Last . Just . Left $ nftId
  void $ Contract.logInfo @Hask.String $ printf "Closing auction for %s" $ Hask.show nftVal
  void $ Contract.awaitTxConfirmed $ txId ledgerTx
  void $ Contract.logInfo @Hask.String $ printf "Confirmed close auction for %s" $ Hask.show nftVal
  where
    updateDatum newOwner node =
      node
        { node'information =
            (node'information node)
              { info'owner = newOwner
              , info'auctionState = Nothing
              }
        }
