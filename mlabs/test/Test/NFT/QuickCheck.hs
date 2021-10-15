module Test.NFT.QuickCheck where

import PlutusTx.Prelude hiding (fmap, length, (<$>), (<*>), mconcat)
import Prelude (
  fmap,
  (<$>),
  (<*>),
  mconcat,
  div
 )

import Test.Tasty (TestTree, testGroup)
import Plutus.Contract.Test (Wallet (..), checkPredicateInner, TracePredicate)
import Plutus.Trace.Emulator (EmulatorRuntimeError(..), throwError)
import Control.Monad.Reader (lift)
import Test.QuickCheck qualified as QC
import Data.Bool (bool)
import Test.Tasty.QuickCheck (testProperty)

import Mlabs.Emulator.Scene (Scene, checkScene)
import Mlabs.NFT.Validation
import Test.NFT.Init

positiveInteger :: QC.Gen Integer
positiveInteger = fmap ((* 100) . QC.getPositive) (QC.resize 5 QC.arbitrary)

wallets :: [Wallet]
wallets = [w1, w2, w3]

mkScript :: ([Wallet], Integer) -> Script
mkScript ([w1', w2', w3'], price) = do
  userAct w1' $ SetPriceAct (Just price) "NFT"
  userAct w2' $ BuyAct price Nothing "NFT"
  userAct w2' $ SetPriceAct (Just (price * 2)) "NFT"
  userAct w3' $ BuyAct (price * 2) Nothing "NFT"
mkScript _ = lift $ throwError $ GenericError "Unreachable"

mkScene :: ([Wallet], Integer) -> Scene
mkScene ([w1', w2', w3'], price) =
  mconcat
    [ w1' `ownsAda` (  price1 + share1 + share2)
    , w2' `ownsAda` (- price1 - share1 + price2)
    , w3' `ownsAda` (- price2 - share2)
    ]
  where
    price1 = price - share1
    share1 = price `div` 10
    price2 = price * 2 - share2
    share2 = (price * 2) `div` 10
mkScene _ = noChangesScene    

testContract :: QC.Property
testContract = QC.forAll ((,) <$> QC.shuffle wallets <*> positiveInteger) testProp

testProp :: ([Wallet], Integer) -> QC.Property
testProp inp = toProp (checkScene $ mkScene inp) (head $ fst inp, mkScript inp)

toProp :: TracePredicate -> (Wallet, Script) -> QC.Property
toProp assertions script =
  QC.property $
  either (const False) (const True) $
  checkPredicateInner checkOptions assertions (uncurry runScript script) (const $ Right ()) (bool (Left ()) (Right ()))

test :: TestTree
test = testGroup "QuickCheck" [testProperty "Contract" testContract]
