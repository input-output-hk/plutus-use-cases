{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Mlabs.Governance.Contract.Simulator.Handler (
  GovernanceContracts (..),
  handlers,
  wallets,
  govTokenName,
  govAmount,
) where

import Control.Monad (forM_, when)
import PlutusTx.Prelude
import Prelude (Show, show)

import Mlabs.Governance.Contract.Api (GovernanceSchema)
import Mlabs.Governance.Contract.Server (governanceEndpoints)
import Mlabs.Governance.Contract.Validation (AssetClassGov (..))

import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import Data.Monoid (Last (..))
import Data.OpenApi.Schema qualified as OpenApi
import Data.Text (Text, pack)
import GHC.Generics (Generic)

import Control.Monad.Freer (interpret)
import Plutus.Contract (Contract, EmptySchema, awaitTxConfirmed, mapError, ownPubKeyHash, submitTx, tell)

import Ledger (CurrencySymbol, PubKeyHash, getCardanoTxId)
import Ledger.Constraints (mustPayToPubKey)
import Mlabs.Utils.Wallet (walletFromNumber)
import Plutus.Contracts.Currency as Currency
import Plutus.V1.Ledger.Value qualified as Value
import Wallet.Emulator.Types (Wallet, walletPubKeyHash)

import Plutus.PAB.Core (EffectHandlers)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (contractHandler), HasDefinitions (..), SomeBuiltin (..), endpointsToSchemas, handleBuiltin)
import Plutus.PAB.Simulator ()
import Plutus.PAB.Simulator as Simulator

import Prettyprinter (Pretty (..), viaShow)

-- FIXME this was passed as `BootstrapCfg` before update from calling side,
--       but now coz `bootstrapGovernance` moved here, had to hardcode them till can figure out better way
wallets :: [Wallet]
wallets = walletFromNumber <$> [1 .. 3] -- wallets participating, wallet #1 is admin
govTokenName :: Value.TokenName
govTokenName = "GOVToken" -- name of GOV token to be paid in exchange of xGOV tokens
govAmount :: Integer
govAmount = 100

-- data BootstrapCfg = BootstrapCfg
--   { wallets :: [Wallet]
--   , govTokenName :: TokenName
--   , govAmount :: Integer
--   }

-- todo Additional Init contract TBD
data GovernanceContracts
  = Bootstrap
  | Governance AssetClassGov
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty GovernanceContracts where
  pretty = viaShow

type BootstrapContract = Contract (Last CurrencySymbol) EmptySchema Text ()

instance HasDefinitions GovernanceContracts where
  -- FIXME couldn't understand what should be here, demo works both like this or [Bootstrap]
  --       didn't try other variants
  getDefinitions = []
  getSchema = \case
    Bootstrap -> endpointsToSchemas @EmptySchema
    Governance _ -> endpointsToSchemas @GovernanceSchema
  getContract = \case
    Bootstrap -> SomeBuiltin bootstrapGovernance
    Governance params -> SomeBuiltin $ governanceEndpoints params

handlers :: EffectHandlers (Builtin GovernanceContracts) (SimulatorState (Builtin GovernanceContracts))
handlers = mkSimulatorHandlers def def handler
  where
    handler :: SimulatorContractHandler (Builtin GovernanceContracts)
    handler = interpret (contractHandler handleBuiltin)

-- FIXME before update it was possible to pass any initialization contract from Main to `handlers`
--       don't know how to achieve it now, had to move all config values
--       and initialization contract itself here for now just to make things work
--       maybe at least BootstrapCfg could passed from outside via `Bootstrap`
-- Bootstrap Contract which mints desired tokens
-- and distributes them ower wallets according to `BootstrapCfg`
bootstrapGovernance :: BootstrapContract
bootstrapGovernance = do
  govCur <- mapError toText mintRequredTokens
  let govCs = Currency.currencySymbol govCur
      govPerWallet = Value.singleton govCs govTokenName govAmount
  distributeGov govPerWallet
  tell $ Last $ Just govCs
  where
    mintRequredTokens ::
      Contract w EmptySchema Currency.CurrencyError Currency.OneShotCurrency
    mintRequredTokens = do
      ownPK <- ownPubKeyHash
      Currency.mintContract ownPK [(govTokenName, govAmount * length wallets)]

    distributeGov govPerWallet = do
      ownPK <- ownPubKeyHash
      forM_ wallets $ \w -> do
        let pkh = walletPKH w
        when (pkh /= ownPK) $ do
          tx <- submitTx $ mustPayToPubKey pkh govPerWallet
          awaitTxConfirmed $ getCardanoTxId tx

    toText = pack . show

walletPKH :: Wallet -> PubKeyHash
walletPKH = walletPubKeyHash
