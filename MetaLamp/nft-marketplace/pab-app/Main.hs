{-# LANGUAGE TypeApplications #-}

import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.MarketplaceContracts     (MarketplaceContracts)
import           Plutus.PAB.Run                      (runWith)

main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @MarketplaceContracts)
