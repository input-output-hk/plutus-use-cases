{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- | Implements a custom currency with a monetary policy that allows
--   the forging of a fixed amount of units.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Plutus.Contract.Blockchain.Param
where

import Control.Monad ( Monad((>>), (>>=)), void )
import           GHC.Generics              (Generic)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract hiding (when)
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton,fee)
import qualified PlutusTx.Prelude  as PlutusPrelude
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema, adaCurrency)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import qualified Prelude
import           Prelude                (Semigroup (..), Show (show), String)
import           Text.Printf            (printf)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8
import Data.Aeson (FromJSON, ToJSON, Value (Bool))
import  Ledger.Ada
import qualified Ledger.Ada as Ada
import Data.Semigroup (Last)
import PlutusTx.These
import qualified Data.ByteString as Builtins
import qualified Data.ByteString.UTF8 as U8
import qualified Plutus.Contract as Extras



data ByteParam =ByteParam
    {
        spCurrency        :: !ByteString ,
        spToken           :: !ByteString ,
        spCost   :: !Integer
    } deriving(GHC.Generics.Generic ,ToJSON,FromJSON,ToSchema)

