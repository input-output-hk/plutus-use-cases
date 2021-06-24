{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Plutus.Contract.Wallet.MarketPlace(
    sell,
    cancelUtxoSale,
    findMarketUtxos,
    buyDirectSaleUtxos,
    buyDirectSaleByAssets,
    directSalesOfPk,
    directSalesInMarket,
    auctionsInMarket,
    submitAuction,
    bidAuctionUtxo,
    parseMarketUtxosNoError,
    parseMarketUtxos,
    parseMarketUtxo,
    claimAuctionUtxos,
    marketValidator,
    Market (..),
    marketAddress,
    DirectSale (..),
    Auction(..),
    SellType(..),
    Price,
    valueOfPrice,
)

where
import Plutus.Contract.Blockchain.MarketPlace
import Plutus.Contract.Blockchain.Utils
import Plutus.Contract
import Ledger.Value
import Ledger  hiding(TxOutRefNotFound, unspentOutputs,txOutDatum)
import qualified Data.Map as Map
import PlutusTx.Prelude hiding ((<>))
import qualified Data.Aeson.Types as JsonTypes
import Data.Aeson (FromJSON,ToJSON, toJSON)
import  Ledger.Scripts
import Data.Text (Text, singleton)
import Prelude (String,Show, show, (<>), Monoid (mconcat), Foldable (elem, length), concat)
import Ledger.AddressMap (UtxoMap)
import GHC.Generics (Generic)
import PlutusTx (toData, IsData)
import  Control.Monad.Error.Lens (throwing, throwing_)
import Control.Monad
import Plutus.Contract.Wallet.Utils

-- TODO remove imports below
import qualified PlutusTx
import PlutusTx.Data
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Ada (lovelaceValueOf)
import Plutus.Contract.Constraints
import qualified Data.Set as Set
import Data.Functor



sell ::  (AsContractError e )
        =>Market ->SellType  -> Value ->Price
             -> Contract w s e Tx
sell market sellType sellItem price = do
    pk<-ownPubKey
    let dsData= DirectSale {
                    dsSeller= pubKeyHash pk,
                    dsCost = price,
                    dsType=sellType
                    }
        tx=mustPayToOtherScript (validatorHash $ marketValidator market) (Datum $ toData dsData) sellItem
    logInfo  @String "created transaction"
    submitTx tx

buyDirectSaleUtxos :: (AsContractError e) => Market -> [(TxOutRef,TxOutTx,DirectSale )]  -> Contract w s e Tx
buyDirectSaleUtxos m fUtxos= submitTxConstraintsWith @MarketType lookups tx
    where
        tx=foldMap  toConstraint fUtxos
        lookups=otherScript (marketValidator m )Prelude.<> unspentOutputs consumedOutputs

        consumedOutputs=Map.fromList $ map (\(a,b,c) ->(a,b)) fUtxos

        toConstraint (utxoRef, _, ds) = mustSpendScriptOutput utxoRef (Redeemer ( toData Buy))
                        <> mustPayToPubKey  (dsSeller ds) (dsUserShareValue m ds)
                        <> mustPayToPubKey (mOperator m) (dsMarketShareValue m ds)


submitAuction :: AsContractError e => Market -> [Auction] -> Contract w s e Tx
submitAuction market  as = submitTx $ Prelude.mconcat $ map constraint as
  where
    constraint auction = mustPayToOtherScript (validatorHash $ marketValidator market) (Datum $ toData auction) $ aValue auction

bidAuctionUtxo :: AsContractError e => Market -> (TxOutRef,TxOutTx,Auction) ->Value -> Contract w s e Tx
bidAuctionUtxo market (ref,tx@TxOutTx{txOutTxOut=utxo},ac) bidAmount = do
  ownPkh <- ownPubKey <&> pubKeyHash
  let newAuction  = Auction{
              aOwner        = aOwner ac,
              aBidder       = ownPkh,
              aAssetClass   = aAssetClass ac,
              aMinBid       = aMinBid  ac,
              aMinIncrement = aMinIncrement ac,
              aDuration     = aDuration ac,
              aValue        = aValue ac
        }

      constraints=
        mustSpendScriptOutput ref (Redeemer ( toData Bid))
        <> mustPayToPubKey  (aBidder  ac) lastBidderShareValue
        <> (mustPayToOtherScript (validatorHash $ marketValidator market) (Datum $ toData ac) $ scriptShareValue)

  submitTxConstraintsWith @MarketType lookups constraints
  where
      lookups=otherScript (marketValidator market ) Prelude.<>  unspentOutputs (Map.singleton ref tx)
      lastBidderShare       = auctionAssetValueOf ac  (txOutValue utxo)
      lastBidderShareValue  = auctionAssetValue ac lastBidderShare
      scriptShareValue      = auctionAssetValue ac (-lastBidderShare) <> lastValue <> bidAmount
      lastValue=txOutValue utxo

claimAuctionUtxos ::AsContractError e => Market -> [(TxOutRef,TxOutTx,Auction)] -> Contract w s e Tx
claimAuctionUtxos market refs =  submitTxConstraintsWith @MarketType lookups constraint
  where
  lookups=otherScript (marketValidator market ) Prelude.<>  unspentOutputs uTxoLookup

  constraint=foldMap utxoToConstraint refs
  uTxoLookup=Map.fromList $ map (\(a,b,c) ->(a,b)) refs

  utxoToConstraint  (txOutRef,TxOutTx _ (TxOut _ value _), auction)=
    mustSpendScriptOutput txOutRef (Redeemer $ toData ClaimBid )
    <> mustPayToPubKey (aOwner auction) (aUserShareValue market auction $  value )
    <> mustPayToPubKey  (mOperator market) (aMarketShareValue market auction value)

cancelUtxoSale :: (AsContractError e) => Market -> [TxOutRef]  -> Contract w s e Tx
cancelUtxoSale market refs=do
  l<-lookups
  c<-constraints
  submitTxConstraintsWith @MarketType l c

  where
    lookups=  filteredUtxos <&> \x ->
                (otherScript $ marketValidator market) <>
                (unspentOutputs $  Map.fromList $ map (\(a,b,_) -> (a,b)) x)

    constraints = filteredUtxos <&> Prelude.mconcat . map (\(x,_,_) ->mustSpendScriptOutput x redeemer)

    filteredUtxos :: AsContractError e => Contract w s e [(TxOutRef,TxOutTx,DirectSale)]
    filteredUtxos=case refs of
                  [a] ->  filterUtxosWithDataAt (\ref _ -> ref == a ) $ marketAddress market
                  _   ->  filterUtxosWithDataAt (\ref _ -> ref `Prelude.elem` refSet ) $ marketAddress  market
    refSet= Set.fromList refs
    redeemer= Redeemer $ toData TakeBack

buyDirectSaleByAssets :: (AsContractError e) => Market -> [AssetClass] -> Contract w s e Tx
buyDirectSaleByAssets market assets =
    case assets of
      [asset] ->( do
        utxoInfo <- findDirectSaleByAsset market  asset
        case utxoInfo of
            [v] -> buyDirectSaleUtxos market [v]
        )
      as       ->
        let assets=Set.fromList as
            doLookup (c,t, v) = AssetClass (c, t) `Prelude.elem` assets && v >0
            hasAsset ref txOutTx=
                         foldl (\ t a ->t || doLookup a) False  $ flattenValue (txOutValue (txOutTxOut txOutTx))
        in
          filterUtxosWithDataAt  hasAsset  (marketAddress market) >>=buyDirectSaleUtxos market

parseMarketUtxo::(IsData a,AsContractError e) => Market -> TxOutRef  -> Contract w s e (TxOutRef ,TxOutTx,a)
parseMarketUtxo market =resolveRefWithDataAt (marketAddress market)

parseMarketUtxos::(IsData a,AsContractError e) => Market -> [TxOutRef]  -> Contract w s e [ParsedUtxo a]
parseMarketUtxos market= resolveRefsWithDataAt (marketAddress market)

parseMarketUtxosNoError ::(IsData a,AsContractError e) => Market -> [TxOutRef]  -> Contract w s e [(TxOutRef ,TxOutTx,a)]
parseMarketUtxosNoError market = resolveRefsWithDataAtWithError (marketAddress market)


directSalesInMarket ::  (AsContractError e) =>
             Market
             -> Contract w s e  [(TxOutRef,TxOutTx,DirectSale  )]
directSalesInMarket market = utxosWithDataAt (marketAddress market)


auctionsInMarket :: (AsContractError e) =>
             Market
             -> Contract w s e  [(TxOutRef,TxOutTx,Auction )]
auctionsInMarket market= utxosWithDataAt (marketAddress market)

findAuctionByAsset :: (AsContractError e) => Market -> AssetClass
             -> Contract w s e  [(TxOutRef,TxOutTx,Auction )]
findAuctionByAsset=findMarketUtxoByAsset

findDirectSaleByAsset::(AsContractError e) =>Market -> AssetClass
             -> Contract w s e  [(TxOutRef,TxOutTx,DirectSale )]
findDirectSaleByAsset=findMarketUtxoByAsset

directSalesOfPk ::(AsContractError e) =>
             Market  ->PubKeyHash-> Contract w s e [(TxOutRef,TxOutTx,DirectSale  )]
directSalesOfPk market pkh = directSalesInMarket market <&> filter (\(_,_,ds)-> dsSeller ds==pkh)

findMarketUtxos:: (AsContractError e,IsData st) => Market -> [TxOutRef] -> Contract w s e [(TxOutRef,TxOutTx,st )]
findMarketUtxos market txouts =do
      let items = Set.fromList txouts
      filterUtxosWithDataAt (\x _ ->  Prelude.elem x items) $ marketAddress market


findMarketUtxoByAsset :: (AsContractError e,IsData a) =>
             Market -> AssetClass
             -> Contract w s e  [(TxOutRef,TxOutTx,a )]
findMarketUtxoByAsset market aClass=
    let
        havingAsset ::  TxOutTx -> Bool
        havingAsset  tx= assetClassValueOf  (txOutValue (txOutTxOut tx)) aClass > 0
    in do
        utxos <-utxoAt (marketAddress market)
        pure $ flattenUtxosWithData  $ Map.filter havingAsset utxos