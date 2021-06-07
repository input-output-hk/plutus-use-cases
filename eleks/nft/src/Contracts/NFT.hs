-- | A decentralized exchange for arbitrary token pairs following the
-- [Uniswap protocol](https://uniswap.org/whitepaper.pdf).
--
-- Details:
--
--  - 'OffChain' contains the instance endpoints and client functionality
--  - 'OnChain' contains the validation logic
--  - 'Types' conains a few common datatypes for working with this contract
--  - 'Pool' contains functions needed by both on-chain and off-chain code
--    related to working with liquidity pools.
module Contracts.NFT
  (module OnChain
  , module OffChain
  , module Types
  ) where

import           Contracts.NFT.OffChain as OffChain
import           Contracts.NFT.OnChain  as OnChain
import           Contracts.NFT.Types    as Types