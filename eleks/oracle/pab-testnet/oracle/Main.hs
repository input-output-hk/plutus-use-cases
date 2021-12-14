{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImportQualifiedPost   #-}

module Main
    ( main
    ) where

import PabContracts.OracleContracts (OracleContracts)
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run (runWith)


main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @OracleContracts)
