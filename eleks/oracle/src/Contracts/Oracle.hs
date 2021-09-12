
-- Details:--
--  - 'OffChain' contains the instance endpoints and client functionality
--  - 'OnChain' contains the validation logic
--  - 'Types' conains a few common datatypes for working with this contract
--    oracle.
module Contracts.Oracle
  ( module OnChain
  , module OffChain
  , module Types
  , module RequestToken
  ) where

import           Contracts.Oracle.OffChain     as OffChain
import           Contracts.Oracle.OnChain      as OnChain
import           Contracts.Oracle.Types        as Types
import           Contracts.Oracle.RequestToken as RequestToken