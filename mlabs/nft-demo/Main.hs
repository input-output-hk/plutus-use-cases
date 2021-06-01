-- | Simulator demo for NFTs
module Main where

import Prelude
import GHC.Generics

import Control.Monad.IO.Class
import Data.Functor
import Control.Monad.Freer.Extras.Log (LogMsg)
import PlutusTx.Prelude (ByteString)
import Control.Monad.Freer (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error (Error)
import Data.Aeson (Result(..), fromJSON)
import Data.Row (type (.\\))
import Data.Text.Prettyprint.Doc (Pretty (..), viaShow)

import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg (..))
import Plutus.PAB.Simulator (Simulation, SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Types (PABError (..))

import Mlabs.Nft.Logic.Types
import qualified Mlabs.Nft.Contract.Nft as Nft
import qualified Mlabs.Data.Ray as R

import Data.Text (Text)
import Playground.Contract

import Plutus.Contract
import Data.Monoid (Last(..))
import qualified Data.Text as T

import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import Mlabs.System.Console.PrettyLogger
import Mlabs.System.Console.Utils

import Wallet.Emulator.Wallet qualified as Wallet

-- | Shortcut for Simulator monad for NFT case
type Sim a = Simulation (Builtin NftContracts) a

-- | Main function to run simulator
main :: IO ()
main = withSimulator handlers $ do
  let users = [1, 2, 3]
  logMlabs
  test "Init users" users (pure ())

  nid <- callStartNft user1
  cids <- mapM (callUser nid) [user1, user2, user3]
  let [u1, u2, u3] = cids

  test "User 2 buys" [1, 2] $ do
    setPrice u1 (Just 100)
    buy u2 100 Nothing

  test "User 3 buys" [1, 2, 3] $ do
    setPrice u2 (Just 500)
    buy u3 500 (Just 1000)
  where
    withSimulator hs act = void $ Simulator.runSimulationWith hs $ do
      Simulator.logString @(Builtin NftContracts) "Starting NFT PAB webserver. Press enter to exit."
      shutdown <- PAB.Server.startServerDebug
      void $ act
      void $ liftIO getLine
      shutdown

    printBalance n = logBalance ("WALLET " <> show n) =<< Simulator.valueAt (Wallet.walletAddress (Wallet n))

    test msg wals act = do
      void $ act
      logAction msg
      mapM_ printBalance wals
      next

    next = do
      logNewLine
      void $ Simulator.waitNSlots 10

------------------------------------------------------------------------
-- handlers

-- | Instanciates start NFT endpoint in the simulator to the given wallet
callStartNft :: Wallet -> Sim NftId
callStartNft wal = do
  wid <- Simulator.activateContract wal StartNft
  nftId <- waitForLast wid
  void $ Simulator.waitUntilFinished wid
  pure nftId

-- | Instanciates user actions endpoint in the simulator to the given wallet
callUser :: NftId -> Wallet -> Sim ContractInstanceId
callUser nid wal = do
  Simulator.activateContract wal $ User nid

-- | Waits for the given value to be written to the state of the service.
-- We use it to share data between endpoints. One endpoint can write parameter to state with tell
-- and in another endpoint we wait for the state-change.
waitForLast :: FromJSON a => ContractInstanceId -> Simulator.Simulation t a
waitForLast cid =
  flip Simulator.waitForState cid $ \json -> case fromJSON json of
    Success (Last (Just x)) -> Just x
    _                       -> Nothing

-- | NFT schemas
data NftContracts
  = StartNft           -- ^ author can start NFT and provide NftId
  | User NftId         -- ^ we read NftId and instanciate schema for the user actions
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty NftContracts where
  pretty = viaShow

handleNftContracts ::
  ( Member (Error PABError) effs
  , Member (LogMsg (PABMultiAgentMsg (Builtin NftContracts))) effs
  ) =>
  ContractEffect (Builtin NftContracts)
    ~> Eff effs
handleNftContracts = Builtin.handleBuiltin getSchema getContract
  where
    getSchema = \case
      StartNft -> Builtin.endpointsToSchemas @(Nft.AuthorSchema .\\ BlockchainActions)
      User _   -> Builtin.endpointsToSchemas @(Nft.UserSchema   .\\ BlockchainActions)
    getContract = \case
      StartNft  -> SomeBuiltin startNftContract
      User nid  -> SomeBuiltin (Nft.userEndpoints nid)

handlers :: SimulatorEffectHandlers (Builtin NftContracts)
handlers =
  Simulator.mkSimulatorHandlers @(Builtin NftContracts) []
    $ interpret handleNftContracts

startNftContract :: Contract (Last NftId) Nft.AuthorSchema Text ()
startNftContract = mapError (T.pack . show) $ Nft.startNft startParams

-------------------------------------------------------------
-- Script helpers

-- | Call buy NFT endpoint
buy :: ContractInstanceId -> Integer -> Maybe Integer -> Sim ()
buy cid price newPrice = do
  void $ Simulator.callEndpointOnInstance cid "buy-act" (Nft.BuyAct price newPrice)
  void $ Simulator.waitNSlots 1

-- | Call set price for NFT endpoint
setPrice :: ContractInstanceId -> Maybe Integer -> Sim ()
setPrice cid newPrice = do
  void $ Simulator.callEndpointOnInstance cid "set-price-act" (Nft.SetPriceAct newPrice)
  void $ Simulator.waitNSlots 1

-------------------------------------------------------------
-- constants

-- Users for testing
user1, user2, user3 :: Wallet
user1 = Wallet 1
user2 = Wallet 2
user3 = Wallet 3

-- | Content of NFT
nftContent :: ByteString
nftContent = "Mona Lisa"

-- | NFT initial parameters
startParams :: Nft.StartParams
startParams = Nft.StartParams
  { sp'content = nftContent
  , sp'share   = 1 R.% 10
  , sp'price   = Nothing
  }

