module Test.Lending(
  tests
) where

import Prelude

import Test.Tasty
import Test.Tasty.HUnit


import Control.Monad (void)
import qualified Plutus.V1.Ledger.Ada as Ada
import qualified Plutus.V1.Ledger.Value as Ledger
import qualified Data.Map as M
import qualified PlutusTx.AssocMap as PM

--------------------------------------------------------------------------------

import Plutus.Contract.Test hiding (tx)
import qualified Plutus.Trace.Emulator as Trace

import qualified Mlabs.Lending as L
import qualified Mlabs.Lending.Coin as L

tests :: TestTree
tests = testGroup "Lending"
  [ testCreate
  ]

testCreate :: TestTree
testCreate = testCase "Create lending pool" $ testOk initConfig createScript

------------------------------------------------------------------------------------

currency :: Ledger.CurrencySymbol
currency = Ledger.currencySymbol "T"

token :: Ledger.TokenName
token = Ledger.tokenName "token"

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


testOk :: Trace.EmulatorConfig -> Trace.EmulatorTrace () -> IO ()
testOk cfg trace = case err of
  Just e  -> assertFailure $ show e
  Nothing -> pure ()
  where
    err = (\(_, merr, _) -> merr) $ Trace.runEmulatorTrace cfg trace

------------------------------------------------------------------------------------
-- init blockchain state

w1, w2, w3, w4 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3
w4 = Wallet 4

initConfig :: Trace.EmulatorConfig
initConfig = cfg
  where
    cfg = Trace.EmulatorConfig $ Left $ M.fromList
            [ (w1, v1)
            , (w2, v2)
            , (w3, v2)
            , (w4, v2)
            ]

    v1 = val 1000 10
    v2 = val 1000 0

    val x y = Ledger.Value $ PM.fromList
      [ (Ada.adaSymbol,  PM.singleton Ada.adaToken x)
      , (currency, PM.singleton token y)]

------------------------------------------------------------------------------------
-- utils

throwError :: String -> Trace.EmulatorTrace a
throwError msg = Trace.throwError (Trace.GenericError msg)

next :: Trace.EmulatorTrace ()
next = void Trace.nextSlot

wait :: Integer -> Trace.EmulatorTrace ()
wait = void . Trace.waitNSlots . fromInteger

