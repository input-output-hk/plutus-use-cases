{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fno-specialize #-}
module Mlabs.Lending.Utils where

import           PlutusTx.Prelude ((.), error)
import qualified PlutusTx.Prelude     as Plutus
import           Ledger               hiding (singleton)

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = Plutus.fromMaybe (error ()) (findOwnInput ctx)


