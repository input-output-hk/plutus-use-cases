{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE TypeFamilies #-}
module Plutus.Contract.Wallet.Nft

where

import Plutus.Contract.Blockchain.Nft
import Plutus.Contract
import Ledger hiding(TxOutRefNotFound, singleton,unspentOutputs,mint)
import Ledger.Value
import Data.String
import Ledger.Constraints
import Data.Text hiding(singleton)
import qualified Data.Map as Map
import qualified Data.Aeson.Types as Types
import Data.Void
import Prelude (show, Semigroup ((<>)), ($), (++), Integer)
import Data.Aeson (toJSON)
import Data.String.Conversions (convertString)
import PlutusTx.Prelude (Maybe (Nothing, Just))
import Control.Applicative
import qualified PlutusTx.AssocMap as AssocMap
import Control.Monad
import Control.Lens
import Data.ByteString (ByteString)
import PlutusTx.Builtins (BuiltinByteString)


mintConstratints ::  TokenName -> Contract w s Text (Maybe (ScriptLookups a, TxConstraints i o))
mintConstratints tn = do
    pk    <- ownPubKey
    utxos <- utxoAt (pubKeyAddress pk)
    case Map.keys utxos of
        []       -> logError  @String "no utxo found" >> pure Nothing
        oref : _ -> do
            let val     = singleton (curSymbol oref tn) tn 1
                nftPolicy=  policy oref tn
                policyHash=mintingPolicyHash nftPolicy
                lookups = mintingPolicy nftPolicy <> unspentOutputs utxos
                tx      = mustMintCurrency   policyHash tn  1 <> mustSpendPubKeyOutput oref
            pure $ Just (lookups,tx)
              

type NftSchema =
  Endpoint "mint" BuiltinByteString
  .\/ Endpoint "multiMint" Integer

nftEndpoints :: (HasEndpoint "mint" BuiltinByteString s,HasEndpoint "multiMint" Integer s)
  =>Promise  [Types.Value] s Text ()
nftEndpoints =void  mintEp

mintEp :: HasEndpoint "mint" BuiltinByteString s =>Promise [Types.Value ] s Text Types.Value
mintEp =
  endpoint @"mint" $ \assetName ->do
  v<-mint $ TokenName assetName
  tell [toJSON  v]
  pure $ toJSON v

mint :: (AsContractError e) =>TokenName -> Contract w s e TxId
mint tn=do
    pk    <- ownPubKey
    utxos <- utxoAt (pubKeyAddress pk)
    case Map.keys utxos of
        []       -> throwError  $ review _OtherError "No Utxos found in wallet"
        oref : _ -> do
            let val     = singleton (curSymbol oref tn) tn 1
                nftPolicy=  policy oref tn
                policyHash=mintingPolicyHash nftPolicy
                lookups = mintingPolicy nftPolicy <> unspentOutputs utxos
                tx      = mustMintCurrency   policyHash tn  1 <> mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            logInfo @String $  "forged " ++ show val
            pure  $ txId ledgerTx