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

import qualified Plutus.PAB.Run.PSGenerator as PAB
import MarketplaceTypes
import           Language.PureScript.Bridge
import           Control.Applicative                        ((<|>))
import Data.Proxy
import           Servant.PureScript                         (HasBridge(..),apiModuleName, defaultSettings, Settings, _generateSubscriberAPI)
import           Control.Lens                               (set, view, (&))
import           System.Directory                           (doesDirectoryExist,
                                                             removeDirectoryRecursive)
import           Control.Monad                              (when)

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
