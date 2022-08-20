{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Governance.Contract (
  test,
) where

import Data.Functor (void)
import Data.Text (Text)
import PlutusTx.Prelude hiding (error)
import Prelude (error)

-- import Data.Monoid ((<>), mempty)

import Plutus.Contract.Test as PT (
  Wallet,
  assertContractError,
  assertFailedTransaction,
  assertNoFailedTransactions,
  checkPredicateOptions,
  valueAtAddress,
  walletFundsChange,
  (.&&.),
 )

import Mlabs.Plutus.Contract (callEndpoint')
import Plutus.Trace.Emulator (ContractInstanceTag)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.Trace.Emulator.Types (ContractHandle)

import Control.Monad.Freer (Eff, Members)
import Data.Semigroup (Last)
import Data.Text as T (isInfixOf)
import Test.Tasty (TestTree, testGroup)

import Mlabs.Governance.Contract.Api (
  Deposit (..),
  GovernanceSchema,
  Withdraw (..),
 )
import Mlabs.Governance.Contract.Server qualified as Gov
import Test.Governance.Init as Test
import Test.Utils (concatPredicates, next)

import Ledger.Index (ValidationError (..))

import Plutus.Trace.Effects.RunContract (RunContract, StartContract)

--import Control.Monad.Writer (Monoid(mempty))

theContract :: Gov.GovernanceContract ()
theContract = Gov.governanceEndpoints Test.acGOV

type Handle = ContractHandle (Maybe (Last Integer)) GovernanceSchema Text
setup ::
  (Members [RunContract, StartContract] effs) =>
  Wallet ->
  (Wallet, Gov.GovernanceContract (), ContractInstanceTag, Eff effs Handle)
setup wallet = (wallet, theContract, Trace.walletInstanceTag wallet, Trace.activateContractWallet wallet theContract)

test :: TestTree
test =
  testGroup
    "Contract"
    [ testGroup
        "Deposit"
        [ testDepositHappyPath
        , testInsuficcientGOVFails
        , testCantDepositWithoutGov
        , testCantDepositNegativeAmount1
        , testCantDepositNegativeAmount2
        ]
    , testGroup
        "Withdraw"
        []
    ]

-- testFullWithdraw
-- , testPartialWithdraw
-- , testCantWithdrawNegativeAmount

-- deposit tests
testDepositHappyPath :: TestTree
testDepositHappyPath =
  let (wallet, _, _, activateWallet) = setup Test.fstWalletWithGOV
      depoAmt1 = 10
      depoAmt2 = 40
      depoAmt = depoAmt1 + depoAmt2
   in checkPredicateOptions
        Test.checkOptions
        "Deposit"
        ( assertNoFailedTransactions
            .&&. walletFundsChange
              wallet
              ( Test.gov (negate depoAmt)
                  <> Test.xgov wallet depoAmt
              )
            .&&. valueAtAddress Test.scriptAddress (== Test.gov depoAmt)
        )
        $ do
          hdl <- activateWallet
          void $ callEndpoint' @Deposit hdl (Deposit depoAmt1)
          next
          void $ callEndpoint' @Deposit hdl (Deposit depoAmt2)
          next

testInsuficcientGOVFails :: TestTree
testInsuficcientGOVFails =
  let (wallet, contract, tag, activateWallet) = setup Test.fstWalletWithGOV
      errCheck = ("InsufficientFunds" `T.isInfixOf`) -- todo probably matching some concrete error type will be better
   in checkPredicateOptions
        Test.checkOptions
        "Cant deposit more GOV than wallet owns"
        ( assertNoFailedTransactions
            .&&. assertContractError contract tag errCheck "Should fail with `InsufficientFunds`"
            .&&. walletFundsChange wallet mempty
        )
        $ do
          hdl <- activateWallet
          void $ callEndpoint' @Deposit hdl (Deposit 1000) -- TODO get value from wallet
          next

testCantDepositWithoutGov :: TestTree
testCantDepositWithoutGov =
  let (wallet, contract, tag, activateWallet) = setup Test.walletNoGOV
      errCheck = ("InsufficientFunds" `T.isInfixOf`)
   in checkPredicateOptions
        Test.checkOptions
        "Cant deposit with no GOV in wallet"
        ( assertNoFailedTransactions
            .&&. assertContractError contract tag errCheck "Should fail with `InsufficientFunds`"
            .&&. walletFundsChange wallet mempty
        )
        $ do
          hdl <- activateWallet
          void $ callEndpoint' @Deposit hdl (Deposit 50)
          next

{- A bit special case at the moment:
   if we try to deposit negative amount without making (positive) deposit beforehand,
   transaction will have to burn xGOV tokens:
   (see in `deposit`: `xGovValue = Validation.xgovSingleton params.nft (coerce ownPkh) amnt`)
   But without prior deposit wallet won't have xGOV tokens to burn,
   so `Contract` will throw `InsufficientFunds` exception
-}
testCantDepositNegativeAmount1 :: TestTree
testCantDepositNegativeAmount1 =
  let (wallet, contract, tag, activateWallet) = setup Test.fstWalletWithGOV
      errCheck = ("InsufficientFunds" `T.isInfixOf`)
   in checkPredicateOptions
        Test.checkOptions
        "Cant deposit negative GOV case 1"
        ( assertNoFailedTransactions
            .&&. assertContractError contract tag errCheck "Should fail with `InsufficientFunds`"
            .&&. walletFundsChange wallet mempty
        )
        $ do
          hdl <- activateWallet
          void $ callEndpoint' @Deposit hdl (Deposit (negate 2))
          next

testCantDepositNegativeAmount2 :: TestTree
testCantDepositNegativeAmount2 = checkPredicateOptions Test.checkOptions msg predicates actions
  where
    msg = "Cannot deposit negative GOV case 2"

    actions = do
      hdl <- activateWallet
      void $ callEndpoint' @Deposit hdl (Deposit depoAmt)
      next
      void $ callEndpoint' @Deposit hdl (Deposit (negate 2))
      next

    (wallet, _, _, activateWallet) = setup Test.fstWalletWithGOV

    depoAmt = 20

    predicates =
      concatPredicates
        [ assertFailedTransaction errCheck
        , walletFundsChange wallet $ mconcat [Test.gov (negate depoAmt), Test.xgov wallet depoAmt]
        , valueAtAddress Test.scriptAddress (== Test.gov depoAmt)
        ]
      where
        errCheck _ e _ = case e of
          NegativeValue _ -> True
          _ -> False

-- withdraw tests
testFullWithdraw :: TestTree
testFullWithdraw = checkPredicateOptions Test.checkOptions msg predicates actions
  where
    msg = "Full withdraw"
    depoAmt = 50
    (wallet, _, _, activateWallet) = setup Test.fstWalletWithGOV
    predicates =
      concatPredicates
        [ assertNoFailedTransactions
        , walletFundsChange wallet mempty
        ]
    actions = do
      hdl <- activateWallet
      next
      void $ callEndpoint' @Deposit hdl (Deposit depoAmt)
      next
      void $ callEndpoint' @Withdraw hdl (Withdraw $ Test.xgovEP wallet depoAmt)
      next

testPartialWithdraw :: TestTree
testPartialWithdraw =
  let (wallet, _, _, activateWallet) = setup Test.fstWalletWithGOV
      depoAmt = 50
      withdrawAmt = 20
      diff = depoAmt - withdrawAmt
   in checkPredicateOptions
        Test.checkOptions
        "Partial withdraw"
        ( assertNoFailedTransactions
            .&&. walletFundsChange wallet (Test.gov (negate diff) <> Test.xgov wallet diff)
            .&&. valueAtAddress Test.scriptAddress (== Test.gov diff)
        )
        $ do
          hdl <- activateWallet
          next
          void $ callEndpoint' @Deposit hdl (Deposit depoAmt)
          next
          void $ callEndpoint' @Withdraw hdl (Withdraw $ Test.xgovEP wallet withdrawAmt)
          next

{- What behaviour expected here:
    - failed transaction
    - contract error
    - withdraw all available
    ?
-}
testCantWithdrawMoreThandeposited :: TestTree
testCantWithdrawMoreThandeposited = error "TBD"

testCantWithdrawNegativeAmount :: TestTree
testCantWithdrawNegativeAmount =
  let (wallet, _, _, activateWallet) = setup Test.fstWalletWithGOV
      errCheck _ e _ = case e of NegativeValue _ -> True; _ -> False
      depoAmt = 50
   in checkPredicateOptions
        Test.checkOptions
        "Cant withdraw negative xGOV amount"
        ( assertFailedTransaction errCheck
            .&&. walletFundsChange
              wallet
              ( Test.gov (negate depoAmt)
                  <> Test.xgov wallet depoAmt
              )
            .&&. valueAtAddress Test.scriptAddress (== Test.gov depoAmt)
        )
        $ do
          hdl <- activateWallet
          void $ callEndpoint' @Deposit hdl (Deposit depoAmt)
          next
          void $ callEndpoint' @Withdraw hdl (Withdraw $ Test.xgovEP wallet (negate 1))
          next

testCanWithdrawOnlyxGov :: TestTree
testCanWithdrawOnlyxGov = error "TBD"
