{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Monad                                (when)
import           Control.Monad.Reader                         (MonadReader)
import           Data.Aeson.TypeScript.Internal
import           Data.Aeson.TypeScript.TH
import qualified Data.Aeson.Types                             as Aeson
import           Data.ByteString                              (ByteString)
import           Data.Proxy                                   (Proxy (Proxy))
import           Plutus.Abstract.ContractResponse             (ContractState)
import qualified Plutus.Abstract.Percentage                   as Percentage
import           Plutus.Abstract.RemoteData                   (RemoteData)
import           Plutus.Contract.StateMachine.ThreadToken     (ThreadToken)
import qualified Plutus.Contracts.NftMarketplace.Endpoints    as Marketplace
import qualified Plutus.Contracts.NftMarketplace.OnChain.Core as Marketplace
import qualified Plutus.Contracts.Services.Auction            as Auction
import qualified Plutus.Contracts.Services.Sale               as Sale
import           Plutus.PAB.Simulation                        (MarketplaceContracts (..))
import           Plutus.V1.Ledger.Ada                         (Ada)
import           Plutus.V1.Ledger.Crypto                      (PubKeyHash)
import           Plutus.V1.Ledger.Time                        (DiffMilliSeconds,
                                                               POSIXTime)
import           Plutus.V1.Ledger.Tx                          (TxOutRef, RedeemerPtr, ScriptTag, TxInType)
import           Plutus.V1.Ledger.TxId                        (TxId)
import           Plutus.V1.Ledger.Value                       (CurrencySymbol,
                                                               TokenName, Value)
import qualified PlutusTx.AssocMap                            as AssocMap
import           PlutusTx.Builtins.Internal                   (BuiltinByteString)
import           System.Directory                             (doesDirectoryExist,
                                                               removeDirectoryRecursive)
import Plutus.PAB.Webserver.Types (ChainReport)
import           Ledger                                  (Tx, TxId, TxIn, TxOut)
import           Ledger.Index                            (UtxoIndex)
import           Wallet.Rollup.Types                     (AnnotatedTx, SequenceId, DereferencedInput, BeneficialOwner, TxKey)
import Plutus.V1.Ledger.Scripts (ValidatorHash, MintingPolicy, Redeemer, DatumHash, Datum, Validator, Script)
import Data.Map (Map)
import Plutus.V1.Ledger.Slot (Slot)
import           Plutus.V1.Ledger.Interval (Interval, LowerBound, UpperBound, Extended)
import Plutus.V1.Ledger.Crypto (PubKey, Signature)
import           Plutus.V1.Ledger.Bytes    (LedgerBytes (..))
import PlutusTx.Builtins.Internal (BuiltinData)
import PlutusCore.Data (Data)
import qualified Data.ByteString           as BS
import Plutus.V1.Ledger.Address (Address)
import Data.Word8 (Word8)
import           Plutus.V1.Ledger.Credential (Credential, StakingCredential)

$(deriveTypeScript Aeson.defaultOptions ''ChainReport)
$(deriveTypeScript Aeson.defaultOptions ''Tx)
$(deriveTypeScript Aeson.defaultOptions ''TxId)
$(deriveTypeScript Aeson.defaultOptions ''UtxoIndex)
$(deriveTypeScript Aeson.defaultOptions ''AnnotatedTx)
$(deriveTypeScript Aeson.defaultOptions ''TxIn)
$(deriveTypeScript Aeson.defaultOptions ''TxOut)
$(deriveTypeScript Aeson.defaultOptions ''CurrencySymbol)
$(deriveTypeScript Aeson.defaultOptions ''Value)
$(deriveTypeScript Aeson.defaultOptions ''TokenName)
$(deriveTypeScript Aeson.defaultOptions ''TxOutRef)
$(deriveTypeScript Aeson.defaultOptions ''SequenceId)
$(deriveTypeScript Aeson.defaultOptions ''DereferencedInput)
$(deriveTypeScript Aeson.defaultOptions ''BeneficialOwner)
$(deriveTypeScript Aeson.defaultOptions ''PubKeyHash)
$(deriveTypeScript Aeson.defaultOptions ''ValidatorHash)
$(deriveTypeScript Aeson.defaultOptions ''Slot)
$(deriveTypeScript Aeson.defaultOptions ''Interval)
$(deriveTypeScript Aeson.defaultOptions ''LowerBound)
$(deriveTypeScript Aeson.defaultOptions ''UpperBound)
$(deriveTypeScript Aeson.defaultOptions ''Extended)
$(deriveTypeScript Aeson.defaultOptions ''MintingPolicy)
$(deriveTypeScript Aeson.defaultOptions ''PubKey)
$(deriveTypeScript Aeson.defaultOptions ''LedgerBytes)
$(deriveTypeScript Aeson.defaultOptions ''Signature)
$(deriveTypeScript Aeson.defaultOptions ''RedeemerPtr)
$(deriveTypeScript Aeson.defaultOptions ''ScriptTag)
$(deriveTypeScript Aeson.defaultOptions ''Redeemer)
$(deriveTypeScript Aeson.defaultOptions ''BuiltinData)
$(deriveTypeScript Aeson.defaultOptions ''Data)
$(deriveTypeScript Aeson.defaultOptions ''DatumHash)
$(deriveTypeScript Aeson.defaultOptions ''Datum)
$(deriveTypeScript Aeson.defaultOptions ''TxInType)
$(deriveTypeScript Aeson.defaultOptions ''Validator)
$(deriveTypeScript Aeson.defaultOptions ''Address)
$(deriveTypeScript Aeson.defaultOptions ''TxKey)
$(deriveTypeScript Aeson.defaultOptions ''Credential)
$(deriveTypeScript Aeson.defaultOptions ''StakingCredential)

instance TypeScript BuiltinByteString where
  getTypeScriptType _ = "string"

instance TypeScript BS.ByteString where
  getTypeScriptType _ = "string"

instance TypeScript Word8 where
  getTypeScriptType _ = "string"

instance TypeScript Script where
  getTypeScriptType _ = "unknown"

instance (TypeScript a, TypeScript b) => TypeScript (AssocMap.Map a b) where
  getTypeScriptType _ = "AssocMap<" <> (getTypeScriptType (Proxy :: Proxy a)) <> ", " <> (getTypeScriptType (Proxy :: Proxy b)) <> ">"
  getTypeScriptDeclarations _ = [TSRawDeclaration "export type AssocMap<K, V> = [K, V][]"]

instance (TypeScript a, TypeScript b) => TypeScript (Map a b) where
  getTypeScriptType _ = "HaskellMap<" <> (getTypeScriptType (Proxy :: Proxy a)) <> ", " <> (getTypeScriptType (Proxy :: Proxy b)) <> ">"
  getTypeScriptDeclarations _ = [TSRawDeclaration "export type HaskellMap<K, V> = Record<string, V>"]

formattingOptions :: FormattingOptions
formattingOptions = FormattingOptions
  { numIndentSpaces = 2
  , interfaceNameModifier = id
  , typeNameModifier = id
  , exportMode = ExportEach
  , typeAlternativesFormat = TypeAlias
  }

main :: IO ()
main = writeFile "generated.ts" $ formatTSDeclarations' formattingOptions (
    (getTypeScriptDeclarations (Proxy @ChainReport)) <>
      (getTypeScriptDeclarations (Proxy @Tx)) <>
      (getTypeScriptDeclarations (Proxy @TxId)) <>
      (getTypeScriptDeclarations (Proxy @UtxoIndex)) <>
      (getTypeScriptDeclarations (Proxy @AnnotatedTx)) <>
      (getTypeScriptDeclarations (Proxy @TxIn)) <>
      (getTypeScriptDeclarations (Proxy @TxOut)) <>
      (getTypeScriptDeclarations (Proxy @CurrencySymbol)) <>
      (getTypeScriptDeclarations (Proxy @Value)) <>
      (getTypeScriptDeclarations (Proxy @TokenName)) <>
      (getTypeScriptDeclarations (Proxy @(AssocMap.Map T1 T2))) <>
      (getTypeScriptDeclarations (Proxy @TxOutRef)) <>
      (getTypeScriptDeclarations (Proxy @SequenceId)) <>
      (getTypeScriptDeclarations (Proxy @DereferencedInput)) <>
      (getTypeScriptDeclarations (Proxy @BeneficialOwner)) <>
      (getTypeScriptDeclarations (Proxy @PubKeyHash)) <>
      (getTypeScriptDeclarations (Proxy @ValidatorHash)) <>
      (getTypeScriptDeclarations (Proxy @(Map T1 T2))) <>
      (getTypeScriptDeclarations (Proxy @(Interval T))) <>
      (getTypeScriptDeclarations (Proxy @Slot)) <>
      (getTypeScriptDeclarations (Proxy @(LowerBound T))) <>
      (getTypeScriptDeclarations (Proxy @(UpperBound T))) <>
      (getTypeScriptDeclarations (Proxy @(Extended T))) <>
      (getTypeScriptDeclarations (Proxy @MintingPolicy)) <>
      (getTypeScriptDeclarations (Proxy @PubKey)) <>
      (getTypeScriptDeclarations (Proxy @LedgerBytes)) <>
      (getTypeScriptDeclarations (Proxy @Signature)) <>
      (getTypeScriptDeclarations (Proxy @RedeemerPtr)) <>
      (getTypeScriptDeclarations (Proxy @ScriptTag)) <>
      (getTypeScriptDeclarations (Proxy @Redeemer)) <> 
      (getTypeScriptDeclarations (Proxy @BuiltinData)) <>
      (getTypeScriptDeclarations (Proxy @Data)) <>
      (getTypeScriptDeclarations (Proxy @BS.ByteString)) <>
      (getTypeScriptDeclarations (Proxy @DatumHash)) <>
      (getTypeScriptDeclarations (Proxy @Datum)) <>
      (getTypeScriptDeclarations (Proxy @TxInType)) <>
      (getTypeScriptDeclarations (Proxy @Validator)) <>
      (getTypeScriptDeclarations (Proxy @Address)) <>
      (getTypeScriptDeclarations (Proxy @TxKey)) <>
      (getTypeScriptDeclarations (Proxy @Script)) <>
      (getTypeScriptDeclarations (Proxy @Word8)) <>
      (getTypeScriptDeclarations (Proxy @Credential)) <>
      (getTypeScriptDeclarations (Proxy @StakingCredential))
  )
