{-# LANGUAGE UndecidableInstances #-}

module Mlabs.NFT.Contract.Buy (
  buy,
) where

import PlutusTx qualified
import PlutusTx.Prelude hiding (mconcat, (<>))
import Prelude (mconcat)
import Prelude qualified as Hask

import Control.Lens ((^.))
import Control.Monad (void, when)
import Data.Map qualified as Map
import Data.Monoid (Last (..), (<>))
import Data.Text (Text)

import Ledger (
  Datum (..),
  Redeemer (..),
  ciTxOutValue,
 )
import Ledger.Typed.Scripts (validatorScript)
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Constraints qualified as Constraints
import Plutus.V1.Ledger.Api (ToData (toBuiltinData))

import Mlabs.NFT.Contract.Aux (
  findNft,
  fstUtxoAt,
  getNftAppSymbol,
  getUId,
  getUserAddr,
  getUserUtxos,
 )
import Mlabs.NFT.Contract.Gov.Fees (getFeesConstraints)
import Mlabs.NFT.Contract.Gov.Query (queryCurrFeeRate)
import Mlabs.NFT.Spooky (toSpooky)
import Mlabs.NFT.Types (
  BuyRequestUser (..),
  DatumNft (NodeDatum),
  InformationNft (info'owner', info'price'),
  NftListNode (node'information'),
  PointInfo (pi'CITxO, pi'TOR, pi'data),
  UniqueToken,
  UserAct (BuyAct, act'bid', act'newPrice', act'symbol'),
  UserId (UserId),
  UserWriter,
  getUserId,
  info'author,
  info'owner,
  info'price,
  info'share,
  node'information,
 )
import Mlabs.NFT.Validation (calculateShares, txPolicy)

{- | BUY.
 Attempts to buy a new NFT by changing the owner, pays the current owner and
 the author, and sets a new price for the NFT.
-}
buy :: forall s. UniqueToken -> BuyRequestUser -> Contract UserWriter s Text ()
buy uT BuyRequestUser {..} = do
  ownOrefTxOut <- getUserAddr >>= fstUtxoAt
  ownPkh <- Contract.ownPubKeyHash
  nftPi <- findNft ur'nftId uT
  node <- case pi'data nftPi of
    NodeDatum n -> Hask.pure n
    _ -> Contract.throwError "NFT not found"
  price <- case info'price . node'information $ node of
    Nothing -> Contract.throwError "NFT not for sale."
    Just price -> Hask.pure price

  when (ur'price < price) $
    Contract.throwError "Bid price is too low."

  userUtxos <- getUserUtxos
  feeRate <- queryCurrFeeRate uT

  user <- getUId
  (govTx, govLookups) <- getFeesConstraints uT ur'nftId ur'price user
  symbol <- getNftAppSymbol uT

  let feeValue = round $ fromInteger ur'price * feeRate
      (paidToOwner, paidToAuthor) =
        calculateShares (ur'price - feeValue) . info'share . node'information $ node
      nftDatum = NodeDatum $ updateNftDatum (toSpooky ownPkh) node
      nftVal = pi'CITxO nftPi ^. ciTxOutValue
      action =
        BuyAct
          { act'bid' = toSpooky ur'price
          , act'newPrice' = toSpooky ur'newPrice
          , act'symbol' = toSpooky symbol
          }
      lookups =
        mconcat $
          [ Constraints.unspentOutputs userUtxos
          , Constraints.unspentOutputs $ Map.fromList [ownOrefTxOut]
          , Constraints.unspentOutputs $ Map.fromList [(pi'TOR nftPi, pi'CITxO nftPi)]
          , Constraints.typedValidatorLookups (txPolicy uT)
          , Constraints.otherScript (validatorScript $ txPolicy uT)
          ]
            <> govLookups
      tx =
        mconcat $
          [ Constraints.mustPayToTheScript (toBuiltinData nftDatum) nftVal
          , Constraints.mustIncludeDatum (Datum . PlutusTx.toBuiltinData $ nftDatum)
          , Constraints.mustPayToPubKey (getUserId . info'author . node'information $ node) paidToAuthor
          , Constraints.mustPayToPubKey (getUserId . info'owner . node'information $ node) paidToOwner
          , Constraints.mustSpendPubKeyOutput (fst ownOrefTxOut)
          , Constraints.mustSpendScriptOutput
              (pi'TOR nftPi)
              (Redeemer . PlutusTx.toBuiltinData $ action)
          ]
            <> govTx
  void $ Contract.submitTxConstraintsWith lookups tx
  Contract.tell . Last . Just . Left $ ur'nftId
  Contract.logInfo @Hask.String "buy successful!"
  where
    updateNftDatum newOwner node =
      node
        { node'information' =
            toSpooky
              (node'information node)
                { info'price' = toSpooky ur'newPrice
                , info'owner' = toSpooky $ UserId newOwner
                }
        }
