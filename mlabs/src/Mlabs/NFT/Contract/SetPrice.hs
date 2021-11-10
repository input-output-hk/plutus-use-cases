{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.SetPrice (
  setPrice,
) where

import PlutusTx.Prelude hiding (mconcat, mempty, (<>))
import Prelude (mconcat)
import Prelude qualified as Hask

import Control.Lens ((^.))
import Control.Monad (void, when)

import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text)

--import Mlabs.Plutus.Contract ()
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import PlutusTx qualified

import Ledger (
  Redeemer (..),
  ciTxOutValue,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorScript)

--import Ledger.Value as Value (AssetClass (..), TokenName (..), singleton)
--import Plutus.ChainIndex.Tx ()

import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

--------------------------------------------------------------------------------
-- Set Price

setPrice :: NftAppSymbol -> SetPriceParams -> Contract UserWriter s Text ()
setPrice symbol SetPriceParams {..} = do
  when negativePrice $ Contract.throwError "New price can not be negative"
  ownOrefTxOut <- getUserAddr >>= fstUtxoAt
  ownPkh <- Contract.ownPubKeyHash
  PointInfo {..} <- findNft sp'nftId symbol
  oldNode <- case pi'datum of
    NodeDatum n -> Hask.pure n
    _ -> Contract.throwError "NFT not found"
  when (getUserId ((info'owner . node'information) oldNode) /= ownPkh) $
    Contract.throwError "Only owner can set price"
  let nftDatum = NodeDatum $ updateDatum oldNode
      nftVal = pi'CITxO ^. ciTxOutValue
      action = SetPriceAct sp'price symbol
      lookups =
        mconcat
          [ Constraints.unspentOutputs $ Map.fromList [ownOrefTxOut]
          , Constraints.unspentOutputs $ Map.fromList [(pi'TOR, pi'CITxO)]
          , Constraints.typedValidatorLookups txPolicy
          , Constraints.otherScript (validatorScript txPolicy)
          ]
      tx =
        mconcat
          [ Constraints.mustPayToTheScript nftDatum nftVal
          , Constraints.mustSpendPubKeyOutput (fst ownOrefTxOut)
          , Constraints.mustSpendScriptOutput
              pi'TOR
              (Redeemer . PlutusTx.toBuiltinData $ action)
          ]

  void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
  Contract.tell . Last . Just . Left $ sp'nftId
  Contract.logInfo @Hask.String "set-price successful!"
  where
    updateDatum node = node {node'information = (node'information node) {info'price = sp'price}}

    negativePrice = case sp'price of
      Nothing -> False
      Just v -> v < 0
