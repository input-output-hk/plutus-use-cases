{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications    #-}

module Main
    ( main
    ) where

import PabContracts.SimulatorPabContracts (MutualBetContracts (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run (runWith)

main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @MutualBetContracts)
