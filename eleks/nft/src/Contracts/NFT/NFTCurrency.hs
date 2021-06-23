{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# options_ghc -fno-strictness         #-}
{-# options_ghc -fno-specialise         #-}

module Contracts.NFT.NFTCurrency
    where

import           Contracts.NFT.Types
import           Control.Monad                    hiding (fmap)
import qualified Data.Text                        as T
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import qualified Ledger.Ada                       as Ada
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     (AssetClass (..), assetClass, assetClassValue, assetClassValueOf, valueOf,
                                                    symbols, unCurrencySymbol, unTokenName, CurrencySymbol (..))
import qualified Ledger.Value                     as Value
import qualified Ledger.Contexts                  as Validation
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), unless)
import           Prelude                          (Semigroup (..), Integer, Bool, show)
import qualified Prelude

-- | An nft currency
data NFTCurrency = NFTCurrency
  { market        :: NFTMarket
  } deriving (Generic, Show, Prelude.Eq, ToJSON, FromJSON)

PlutusTx.makeLift ''NFTCurrency

{-# INLINABLE validateNFTForging #-}
validateNFTForging :: NFTCurrency -> ScriptContext -> Bool
validateNFTForging c@(NFTCurrency NFTMarket{..} tn (refHash, refIdx)) ctx@Validation.ScriptContext{Validation.scriptContextTxInfo=txinfo}
    = 
    traceIfFalse "Pending transaction does not spend the designated transaction output" isOutputSpent
    && traceIfFalse "Should forge two tokens (nft and metadata)" (forgedSymbolsCount == 2)
    && traceIfFalse "Value forged different from expected" (forgedTokenValue == 1)
    where
        ownSymbol = ownCurrencySymbol ctx
        forged = txInfoForge txinfo
        forgedTokenValue = valueOf forged ownSymbol tn
        forgedSymbolsCount = length $ symbols forged
        marketInput = case  [ o
                            | o <- getContinuingOutputs ctx
                            , let v = txOutValue o
                            , isMarketToken v marketId
                            ] of
            [_] -> True
            _      -> traceError "nft forging without market input"

        isOutputSpent = spendsOutput txinfo refHash refIdx

nftMonetrayPolicy :: NFTCurrency -> MonetaryPolicy
nftMonetrayPolicy cur = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . validateNFTForging ||])
        `PlutusTx.applyCode` PlutusTx.liftCode cur

-- | The 'Value' forged by the 'NFTCurrency' contract
nftForgedValue :: NFTCurrency -> Value
nftForgedValue cur = nftCurrencyValue (nftCurrencySymbol cur) cur

{-# INLINABLE nftCurrencySymbol #-}
nftCurrencySymbol :: NFTCurrency -> CurrencySymbol
nftCurrencySymbol = scriptCurrencySymbol . nftMonetrayPolicy

nftCurrencyValue :: CurrencySymbol -> NFTCurrency -> Value
nftCurrencyValue s NFTCurrency{tokenName = tokenName} =
    Value.singleton s tokenName 1

mkNFTCurrency :: NFTMarket -> NFTCurrency
mkNFTCurrency market tn (TxOutRef h i) =
    NFTCurrency
        { market = market
        , tokenName  = tn
        , curRefUtxo = (h, i)
        }