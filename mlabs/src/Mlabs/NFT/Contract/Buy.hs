{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.Buy (
  buy,
) where

import PlutusTx.Prelude hiding (mconcat, mempty, (<>))
import Prelude (mconcat)
import Prelude qualified as Hask

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text)

import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusTx qualified

import Ledger (
  Datum (..),
  Redeemer (..),
  ciTxOutValue,
  pubKeyHash,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)

import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

{- | BUY.
 Attempts to buy a new NFT by changing the owner, pays the current owner and
 the author, and sets a new price for the NFT.
-}
buy :: NftAppSymbol -> BuyRequestUser -> Contract (Last NftId) s Text ()
buy symbol BuyRequestUser {..} = do
  ownOrefTxOut <- getUserAddr >>= fstUtxoAt
  ownPkh <- pubKeyHash <$> Contract.ownPubKey
  PointInfo {..} <- findNft ur'nftId symbol
  node <- case pi'datum of
    NodeDatum n -> Hask.pure n
    _ -> Contract.throwError "NFT not found"
  case info'price . node'information $ node of
    Nothing -> Contract.logError @Hask.String "NFT not for sale."
    Just price ->
      if ur'price < price
        then Contract.logError @Hask.String "Bid price is too low."
        else do
          userUtxos <- getUserUtxos
          let (paidToOwner, paidToAuthor) = calculateShares ur'price . info'share . node'information $ node
              nftDatum = NodeDatum $ updateDatum ownPkh node
              nftVal = pi'CITxO ^. ciTxOutValue
              action =
                BuyAct
                  { act'bid = ur'price
                  , act'newPrice = ur'newPrice
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
              tx =
                mconcat
                  [ Constraints.mustPayToTheScript nftDatum nftVal
                  , Constraints.mustIncludeDatum (Datum . PlutusTx.toBuiltinData $ nftDatum)
                  , Constraints.mustPayToPubKey (getUserId . info'author . node'information $ node) paidToAuthor
                  , Constraints.mustPayToPubKey (getUserId . info'owner . node'information $ node) paidToOwner
                  , Constraints.mustSpendPubKeyOutput (fst ownOrefTxOut)
                  , Constraints.mustSpendScriptOutput
                      pi'TOR
                      (Redeemer . PlutusTx.toBuiltinData $ action)
                  ]
          void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
          Contract.tell . Last . Just $ ur'nftId
          Contract.logInfo @Hask.String "buy successful!"
  where
    updateDatum newOwner node =
      node
        { node'information =
            (node'information node)
              { info'price = ur'newPrice
              , info'owner = UserId newOwner
              }
        }
