
-- Details:--
--  - 'OffChain' contains the instance endpoints and client functionality
module Contracts.MutualBetNew
  ( module Types
  , module OnChain
  , module OffChain
  , module Currency
  ) where

import           Contracts.MutualBetNew.Types         as Types
import           Contracts.MutualBetNew.OffChain      as OffChain
import           Contracts.MutualBetNew.OnChain       as OnChain
import           Contracts.MutualBetNew.Currency      as Currency