{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Contrib.Easy.Util (queryUtxos)
import Cardano.Contrib.Easy.Context (getDefaultTestnetContext, IsNetworkCtx (toFullNetworkContext, networkCtxNetwork, networkCtxConn))
import Cardano.Api (deserialiseAddress, AsType( AsAddressAny ) )
import Plutus.Contracts.Coins.CoinsStateMachine

main :: IO ()
main = putStrLn "ok"