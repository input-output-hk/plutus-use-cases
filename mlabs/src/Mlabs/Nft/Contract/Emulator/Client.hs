-- | Client functions to test contracts in EmulatorTrace monad.
module Mlabs.Nft.Contract.Emulator.Client (
  callUserAct,
  callStartNft,
) where

import Prelude

import Data.Functor (void)
import Data.Monoid (Last (..))
import Plutus.Trace.Emulator (EmulatorRuntimeError (..), EmulatorTrace, activateContractWallet, observableState, throwError, waitNSlots)
import Wallet.Emulator (Wallet)

import Mlabs.Nft.Contract.Api (Buy (..), SetPrice (..), StartParams)
import Mlabs.Nft.Contract.Server (authorEndpoints, userEndpoints)
import Mlabs.Nft.Logic.Types qualified as Types
import Mlabs.Plutus.Contract (callEndpoint')

---------------------------------------------------------
-- call endpoints (for debug and testing)

-- | Calls user act
callUserAct :: Types.NftId -> Wallet -> Types.UserAct -> EmulatorTrace ()
callUserAct nid wal act = do
  hdl <- activateContractWallet wal (userEndpoints nid)
  void $ case act of
    Types.BuyAct {..} -> callEndpoint' hdl (Buy act'price act'newPrice)
    Types.SetPriceAct {..} -> callEndpoint' hdl (SetPrice act'newPrice)

-- | Calls initialisation of state for Nft pool
callStartNft :: Wallet -> StartParams -> EmulatorTrace Types.NftId
callStartNft wal sp = do
  hdl <- activateContractWallet wal authorEndpoints
  void $ callEndpoint' hdl sp
  void $ waitNSlots 10
  Last nid <- observableState hdl
  maybe err pure nid
  where
    err = throwError $ GenericError "No NFT started in emulator"
