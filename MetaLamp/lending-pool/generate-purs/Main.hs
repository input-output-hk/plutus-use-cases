{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           AaveTypes                                  (aaveTypes,
                                                             ratioBridge)
import           Cardano.Wallet.Types                       (WalletInfo)
import           Control.Applicative                        ((<|>))
import           Control.Lens                               (set, view, (&))
import           Control.Monad                              (when)
import           Control.Monad.Freer.Extras.Log             (LogLevel,
                                                             LogMessage)
import           Data.Proxy                                 (Proxy (Proxy))
import qualified Data.Text                                  as Text
import           Language.PureScript.Bridge                 (BridgePart,
                                                             Language (Haskell),
                                                             SumType,
                                                             TypeInfo (TypeInfo),
                                                             buildBridge, equal,
                                                             genericShow,
                                                             haskType,
                                                             mkSumType, order,
                                                             typeModule,
                                                             typeName,
                                                             writePSTypesWith,
                                                             (^==))
import           Language.PureScript.Bridge.CodeGenSwitches (ForeignOptions (ForeignOptions),
                                                             genForeign,
                                                             unwrapSingleConstructors)
import           Language.PureScript.Bridge.TypeParameters  (A)
import qualified PSGenerator.Common
import           Plutus.Contract.Checkpoint                 (CheckpointKey,
                                                             CheckpointStore,
                                                             CheckpointStoreItem)
import           Plutus.Contract.Resumable                  (Responses)
import           Plutus.PAB.Events.ContractInstanceState    (PartiallyDecodedResponse)
import qualified Plutus.PAB.Webserver.API                   as API
import           Plutus.PAB.Webserver.Types                 (ChainReport,
                                                             CombinedWSStreamToClient,
                                                             CombinedWSStreamToServer,
                                                             ContractActivationArgs,
                                                             ContractInstanceClientState,
                                                             ContractReport,
                                                             ContractSignatureResponse,
                                                             FullReport,
                                                             InstanceStatusToClient)
import           Plutus.V1.Ledger.Value                     (AssetClass,
                                                             TokenName (..))
import           Servant                                    ((:<|>))
import           Servant.PureScript                         (HasBridge,
                                                             Settings,
                                                             _generateSubscriberAPI,
                                                             apiModuleName,
                                                             defaultBridge,
                                                             defaultSettings,
                                                             languageBridge,
                                                             writeAPIModuleWithSettings)
import           System.Directory                           (doesDirectoryExist,
                                                             removeDirectoryRecursive)
import           Wallet.Emulator.Wallet                     (Wallet (..))

myBridge :: BridgePart
myBridge =
    PSGenerator.Common.aesonBridge <|>
    PSGenerator.Common.containersBridge <|>
    PSGenerator.Common.languageBridge <|>
    PSGenerator.Common.ledgerBridge <|>
    PSGenerator.Common.servantBridge <|>
    PSGenerator.Common.miscBridge <|>
    ratioBridge <|>
    defaultBridge

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
    languageBridge _ = buildBridge myBridge

myTypes :: [SumType 'Haskell]
myTypes =
    aaveTypes <>
    PSGenerator.Common.ledgerTypes <>
    PSGenerator.Common.playgroundTypes <>
    PSGenerator.Common.walletTypes <>
    [ (equal <*> (genericShow <*> mkSumType)) (Proxy @(FullReport A))
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @ChainReport)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(ContractReport A))
    , (equal <*> (genericShow <*> mkSumType))
          (Proxy @(ContractSignatureResponse A))
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(PartiallyDecodedResponse A))

    -- Contract request / response types
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @CheckpointStore)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @CheckpointKey)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(CheckpointStoreItem A))
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(Responses A))

    -- Logging types
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(LogMessage A))
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @LogLevel)

    -- * Web API types
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(ContractActivationArgs A))
    , (genericShow <*> mkSumType) (Proxy @(ContractInstanceClientState A))
    , (genericShow <*> mkSumType) (Proxy @InstanceStatusToClient)
    , (genericShow <*> mkSumType) (Proxy @CombinedWSStreamToClient)
    , (genericShow <*> mkSumType) (Proxy @CombinedWSStreamToServer)
    , (genericShow <*> mkSumType) (Proxy @WalletInfo)
    ]

mySettings :: Settings
mySettings =
    (defaultSettings & set apiModuleName "Plutus.PAB.Webserver")
        {_generateSubscriberAPI = False}

defaultWallet :: Wallet
defaultWallet = Wallet 1
------------------------------------------------------------

generateTo :: FilePath -> IO ()
generateTo outputDir = do
    exists <- doesDirectoryExist outputDir
    when exists $ removeDirectoryRecursive outputDir
    writePSTypesWith
        (genForeign (ForeignOptions {unwrapSingleConstructors = True}))
        outputDir
        (buildBridge myBridge)
        myTypes

main :: IO ()
main = generateTo "client/generated"
