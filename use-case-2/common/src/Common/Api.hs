{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Api where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Int
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Semigroup (First(..))
import Data.Text (Text)
import Data.Vessel

commonStuff :: String
commonStuff = "Here is a string defined in Common.Api"

type DexV = Vessel Q

-- Note: This is view
data Q (v :: (* -> *) -> *) where
  Q_Counter :: Q (IdentityV (First (Maybe Int32)))
  Q_ContractList :: Q (IdentityV (First (Maybe [Text])))

data Api :: * -> * where
  Api_IncrementCounter :: Api ()
  -- TODO make api for swap here instead
  -- Api_ListWalletAccounts :: Api ()

deriveJSONGADT ''Api
deriveArgDict ''Api

deriveArgDict ''Q
deriveJSONGADT ''Q
deriveGEq ''Q
deriveGCompare ''Q
deriveGShow ''Q
