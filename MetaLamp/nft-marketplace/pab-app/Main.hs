{-# LANGUAGE TypeApplications #-}

import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Run (runWith)
import Plutus.PAB.MarketplaceContracts (MarketplaceContracts)

main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @MarketplaceContracts)
