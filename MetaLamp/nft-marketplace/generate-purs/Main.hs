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

import           Control.Applicative        ((<|>))
import           Control.Lens               (set, view, (&))
import           Control.Monad              (when)
import           Data.Proxy
import           Language.PureScript.Bridge
import           MarketplaceTypes
import qualified Plutus.PAB.Run.PSGenerator as PAB
import           Servant.PureScript         (HasBridge (..), Settings,
                                             _generateSubscriberAPI,
                                             apiModuleName, defaultSettings)
import           System.Directory           (doesDirectoryExist,
                                             removeDirectoryRecursive)

myBridge :: BridgePart
myBridge =
    PAB.pabBridge <|>
    ratioBridge

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
    languageBridge _ = buildBridge myBridge

myTypes :: [SumType 'Haskell]
myTypes =
    PAB.pabTypes <>
    marketplaceTypes

instance PAB.HasPSTypes MyBridge where
    psTypes _ = myTypes

mySettings :: Settings
mySettings =
    (defaultSettings & set apiModuleName "Plutus.PAB.Webserver")
        {_generateSubscriberAPI = False}
------------------------------------------------------------

generateTo :: FilePath -> IO ()
generateTo outputDir = do
    exists <- doesDirectoryExist outputDir
    when exists $ removeDirectoryRecursive outputDir
    PAB.generateWith myBridgeProxy outputDir

main :: IO ()
main = generateTo "client/generated"
