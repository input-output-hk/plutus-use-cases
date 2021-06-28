{-# LANGUAGE OverloadedStrings #-}

module Fixtures.Asset where

import qualified Fixtures.Aave           as AaveMock
import qualified Plutus.Contracts.AToken as AToken
import           Plutus.PAB.Simulation   (toAsset)
import           Plutus.V1.Ledger.Value  (AssetClass)

mogus :: AssetClass
mogus = toAsset "MOGUS"

usd :: AssetClass
usd = toAsset "USD"

defaultAssets :: [AssetClass]
defaultAssets = [mogus, usd]

amogus :: AssetClass
amogus = AToken.makeAToken AaveMock.aaveHash mogus

ausd :: AssetClass
ausd = AToken.makeAToken AaveMock.aaveHash usd
