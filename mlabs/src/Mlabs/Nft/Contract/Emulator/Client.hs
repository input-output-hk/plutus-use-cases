-- | Client functions to test contracts in EmulatorTrace monad.
module Mlabs.Nft.Contract.Emulator.Client(
    callUserAct
  , callStartNft
) where

import Prelude

import Data.Functor (void)
import Data.Monoid (Last(..))

import Mlabs.Plutus.Contract
import Mlabs.Nft.Logic.Types
import Mlabs.Nft.Contract.Api
import Mlabs.Nft.Contract.Server

import Plutus.Trace.Emulator (waitNSlots, throwError, EmulatorTrace, observableState, activateContractWallet, EmulatorRuntimeError(..))
import qualified Wallet.Emulator as Emulator

---------------------------------------------------------
-- call endpoints (for debug and testing)

-- | Calls user act
callUserAct :: NftId -> Emulator.Wallet -> UserAct -> EmulatorTrace ()
callUserAct nid wal act = do
  hdl <- activateContractWallet wal (userEndpoints nid)
  void $ case act of
    BuyAct{..}      -> callEndpoint' hdl (Buy act'price act'newPrice)
    SetPriceAct{..} -> callEndpoint' hdl (SetPrice act'newPrice)

-- | Calls initialisation of state for Nft pool
callStartNft :: Emulator.Wallet -> StartParams -> EmulatorTrace NftId
callStartNft wal sp = do
  hdl <- activateContractWallet wal authorEndpoints
  void $ callEndpoint' hdl sp
  void $ waitNSlots 10
  Last nid <- observableState hdl
  maybe err pure nid
  where
    err = throwError $ GenericError "No NFT started in emulator"


