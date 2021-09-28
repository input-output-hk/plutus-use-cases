{-# LANGUAGE OverloadedStrings #-}

module Fixtures.Asset where

import qualified Fixtures.Aave                               as AaveMock
import qualified Plutus.Contracts.LendingPool.OnChain.AToken as AToken
import           Plutus.PAB.Simulation                       (toAsset)
import           Plutus.V1.Ledger.Value                      (AssetClass)

euro :: AssetClass
euro = toAsset "EURO"

usd :: AssetClass
usd = toAsset "USD"

defaultAssets :: [AssetClass]
defaultAssets = [euro, usd]

aeuro :: AssetClass
aeuro = AToken.makeAToken AaveMock.aaveHash euro

ausd :: AssetClass
ausd = AToken.makeAToken AaveMock.aaveHash usd
