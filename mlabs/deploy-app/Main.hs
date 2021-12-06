module Main where

import PlutusTx.Prelude hiding (error)
import System.Environment (getArgs)
import System.Exit (die)
import Prelude (IO, String, error, print, undefined)

import Mlabs.Emulator.Types (UserId (..))
import Mlabs.NftStateMachine.Contract.Forge as F
import Mlabs.NftStateMachine.Contract.StateMachine as SM
import Mlabs.NftStateMachine.Logic.Types (Act (..), Nft (..), NftId (..), UserAct (..), initNft, toNftId)

import Cardano.Api
import Cardano.Api.Shelley

import Cardano.Ledger.Alonzo.Data qualified as Alonzo
import Codec.Serialise
import Ledger.Typed.Scripts.Validators as VS
import Plutus.V1.Ledger.Api (MintingPolicy, TxOutRef, Validator)
import Plutus.V1.Ledger.Api qualified as Plutus
import PlutusTx

import Data.Aeson as Json
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS

import Data.ByteString as DB
import Mlabs.Deploy.Governance
import Mlabs.Deploy.Nft

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["Nft"] ->
      serializeNft
        "56b4d636bfea5cb0628ba202214a9cca42997545da87dfd436e6e3d8d7ba3b28"
        0
        "4cebc6f2a3d0111ddeb09ac48e2053b83b33b15f29182f9b528c6491"
        "MonaLisa"
        "./../.github/workflows/nft_delivery"
    ["Governance"] -> serializeGovernance
    _ ->
      die "Unknown deployment task type"
