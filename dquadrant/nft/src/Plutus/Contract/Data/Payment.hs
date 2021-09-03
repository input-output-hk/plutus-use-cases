{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Plutus.Contract.Data.Payment
where

import PlutusTx.Prelude
import qualified PlutusTx.AssocMap as AssocMap
import GHC.Generics
import Ledger
import Ledger.Value
import PlutusTx
import Ledger.Ada
import Data.Aeson (ToJSON, FromJSON)
import Prelude (Show)
import PlutusTx.Prelude ((<>))



-- Map from PubKeyHash to Value.
--

newtype Payment = Payment ( AssocMap.Map PubKeyHash Value ) deriving(Generic,ToJSON,FromJSON)