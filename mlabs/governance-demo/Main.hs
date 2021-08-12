-- | Simulator demo for Governance
module Main (
  main,
) where

import PlutusTx.Prelude
import Prelude (IO, undefined, getLine, show)

import Control.Monad (when, forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Result(Success), encode, FromJSON, fromJSON)
import Data.Monoid (Last(..))
import Data.Functor (void)
import Data.Text (Text, pack)


import           Mlabs.Governance.Contract.Api (StartGovernance(..))
import           Mlabs.Governance.Contract.Validation (GovParams(..), AssetClassNft(..), AssetClassGov(..))
import           Mlabs.Governance.Contract.Simulator.Handler (BootstrapContract)
import qualified Mlabs.Governance.Contract.Simulator.Handler as Handler
import           Mlabs.Governance.Contract.Simulator.Handler (GovernanceContracts (..))

import Ledger (CurrencySymbol, TokenName, pubKeyHash, PubKeyHash, txId)
import Ledger.Constraints (mustPayToPubKey) 
import Plutus.V1.Ledger.Value qualified as Value

import           Plutus.PAB.Effects.Contract.Builtin      (Builtin)
import           Plutus.PAB.Simulator qualified as Simulator
import qualified Plutus.PAB.Webserver.Server as PWS
import           Wallet.Emulator.Types               (Wallet (..), walletPubKey)

import           Plutus.Contract       (Contract, ContractInstanceId, EmptySchema, tell, mapError, ownPubKey, submitTx, awaitTxConfirmed)
import           Plutus.Contracts.Currency as Currency


cfg = BootstrapCfg 
  { wallets      = Wallet <$> [1..3]
  , nftTokenName = "NFTToken"
  , govTokenName = "GOVToken"
  , govAmount    = 100
  }

-- | Main function to run simulator
main :: IO ()
main = 
  void $ Handler.runSimulation (bootstrapGovernance cfg) $ do
    Simulator.logString @(Builtin GovernanceContracts) "Starting Governance PAB webserver"
    shutdown       <- PWS.startServerDebug
    Simulator.logString @(Builtin GovernanceContracts) "Bootstraping Governance Contract"
    let (admin:_) = (wallets cfg)
    cidInit        <- Simulator.activateContract admin Bootstrap
    (nftCs, govCs) <- waitForLast cidInit
    void $ Simulator.waitUntilFinished cidInit
    let governance = Governance $ GovParams
                      (AssetClassNft nftCs $ nftTokenName cfg) 
                      (AssetClassGov govCs $ govTokenName cfg)
    forM_ (wallets cfg) $ 
       \w -> Simulator.activateContract w governance
    Simulator.logString @(Builtin GovernanceContracts) "Governance simulation ready\nPress Enter to stop and exit"
    void $ liftIO getLine
    shutdown

data BootstrapCfg = BootstrapCfg 
  { wallets      :: [Wallet]
  , nftTokenName :: TokenName
  , govTokenName :: TokenName
  , govAmount    :: Integer
  }

bootstrapGovernance :: BootstrapCfg -> BootstrapContract
bootstrapGovernance BootstrapCfg{..} = do
    (nftCur, govCur) <- mapError toText $ mintRequredTokens
    let nftCs = Currency.currencySymbol nftCur
        govCs = Currency.currencySymbol govCur
        govPerWallet = Value.singleton govCs govTokenName govAmount
    distributeGov govPerWallet
    tell $ Last $ Just (nftCs, govCs)
  where
    mintRequredTokens :: 
      Contract w EmptySchema Currency.CurrencyError (Currency.OneShotCurrency, Currency.OneShotCurrency)
    mintRequredTokens  = do
      ownPK <- pubKeyHash <$> ownPubKey
      nftCurrency <- Currency.mintContract ownPK [(nftTokenName , 1)]
      govCurrency <- Currency.mintContract ownPK [(govTokenName, govAmount * length wallets)]
      return (nftCurrency, govCurrency)

    distributeGov govPerWallet = do
      ownPK <- pubKeyHash <$> ownPubKey
      forM_ wallets $ \w -> do
        let pkh  = pubKeyHash $ walletPubKey w
        when (pkh /= ownPK) $ do
            tx <- submitTx $ mustPayToPubKey pkh govPerWallet
            awaitTxConfirmed $ txId tx

    toText = pack . show

  
waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
    flip Simulator.waitForState cid $ \json -> case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing