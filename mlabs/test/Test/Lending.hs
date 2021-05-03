-- | Test suite for lending exchange
module Test.Lending(
  tests
) where

import Prelude

import Test.Tasty
import Test.Tasty.HUnit

import qualified Plutus.V1.Ledger.Ada as Ada
import qualified Plutus.V1.Ledger.Value as Ledger
import qualified Data.Map as M
import qualified PlutusTx.AssocMap as PM

import Plutus.Contract.Test hiding (tx)
import qualified Plutus.Trace.Emulator as Trace

import qualified Mlabs.Lending as L
import qualified Mlabs.Lending.Coin as L

import Test.Utils

--  | Test suite for lending exchange
tests :: TestTree
tests = testGroup "Lending"
  [ testCreate
  ]

-- | Tests for creation of the coin and exchange platform.
testCreate :: TestTree
testCreate = testCase "Create lending pool" $ testNoErrors initConfig createScript

------------------------------------------------------------------------------------

-- | Script that creates lendex and one coin for lending.
createScript :: Trace.EmulatorTrace ()
createScript = do
  mTheLendex <- L.callStart w1
  next
  case mTheLendex of
    Just theLendex -> do
      L.callCreate theLendex w1 $ L.CreateParams
        { cpCoin = L.mkCoin currency token
        }
      next
    Nothing -> throwError "No lendex was created"
  where
    currency = Ledger.currencySymbol "T"
    token = Ledger.tokenName "token"

------------------------------------------------------------------------------------
-- init blockchain state

-- | Wallets that are used for testing.
w1, w2, w3, w4 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3
w4 = Wallet 4

-- | Initial config
initConfig :: Trace.EmulatorConfig
initConfig = cfg
  where
    cfg = Trace.EmulatorConfig $ Left $ M.fromList
            [ (w1, v1)
            , (w2, v1)
            , (w3, v1)
            , (w4, v1)
            ]

    v1 = val 1000
    val x = Ledger.Value $ PM.fromList [ (Ada.adaSymbol,  PM.singleton Ada.adaToken x) ]

