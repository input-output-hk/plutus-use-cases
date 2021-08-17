{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Governance.Contract (
  test,
) where

import Data.Functor (void)
import Data.Text (Text)
import PlutusTx.Prelude hiding (error)
import Prelude (
  Bool (..),
  const,
  error,
 )

-- import Data.Monoid ((<>), mempty)

import Plutus.Contract.Test (
  Wallet,
  assertContractError,
  assertDone,
  assertFailedTransaction,
  assertNoFailedTransactions,
  checkPredicateOptions,
  not,
  valueAtAddress,
  walletFundsChange,
  (.&&.),
 )

import Mlabs.Plutus.Contract (callEndpoint')
import Plutus.Trace.Emulator (ContractInstanceTag)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.Trace.Emulator.Types (ContractHandle)

import Control.Monad (replicateM_)
import Control.Monad.Freer (Eff, Member)
import Data.Semigroup (Last)
import Data.Text as T (isInfixOf)
import Test.Tasty (TestTree, testGroup)

import Mlabs.Governance.Contract.Api (
  Deposit (..),
  GovernanceSchema,
  StartGovernance,
  Withdraw (..),
 )
import Mlabs.Governance.Contract.Emulator.Client qualified as Gov (callDeposit, startGovernance)
import Mlabs.Governance.Contract.Server qualified as Gov
import Test.Governance.Init as Test
import Test.Utils (next, wait)

import Ledger.Index (ValidationError (..))

import Plutus.Trace.Effects.RunContract (RunContract)

theContract :: Gov.GovernanceContract ()
theContract = Gov.governanceEndpoints Test.params

startGovernanceByAdmin :: Gov.GovernanceContract () -> Trace.EmulatorTrace ()
startGovernanceByAdmin contract = do
  hdl <- Trace.activateContractWallet Test.adminWallet contract
  void $ callEndpoint' @StartGovernance hdl Test.startGovernance
  wait 5

type Handle = ContractHandle (Maybe (Last Integer)) GovernanceSchema Text
setup ::
  (Member RunContract effs) =>
  Wallet ->
  (Wallet, Gov.GovernanceContract (), ContractInstanceTag, Eff effs Handle)
setup wallet = (wallet, theContract, Trace.walletInstanceTag wallet, Trace.activateContractWallet wallet theContract)

test :: TestTree
test =
  testGroup
    "Contract"
    [ testGroup
        "Start Governance"
        [ testStartGovernance
        ]
    , testGroup
        "Deposit"
        [ testDepositHappyPath
        , testInsuficcientGOVFails
        , testCantDepositWithoutGov
        -- , testCantDepositNegativeAmount
        ]
        -- , testGroup
        --     "Withdraw"
        --     [ testFullWithdraw
        --     , testPartialWithdraw
        --     , testCantWithdrawNegativeAmount
        --     ]
    ]

-- start tests
testStartGovernance :: TestTree
testStartGovernance =
  checkPredicateOptions
    Test.checkOptions
    "Start governance"
    ( assertNoFailedTransactions
        .&&. walletFundsChange Test.adminWallet (Test.nft (negate 1))
        .&&. valueAtAddress Test.scriptAddress (== Test.nft 1)
    )
    $ startGovernanceByAdmin theContract

-- deposit tests
testDepositHappyPath :: TestTree
testDepositHappyPath =
  let (wallet, contract, _, activateWallet) = setup Test.fstWalletWithGOV
      depoAmt = 50
   in checkPredicateOptions
        Test.checkOptions
        "Deopsit"
        ( assertNoFailedTransactions
            .&&. walletFundsChange
              wallet
              ( Test.gov (negate depoAmt)
                  <> Test.xgov wallet depoAmt
              )
            .&&. valueAtAddress Test.scriptAddress (== Test.nft 1 <> Test.gov depoAmt)
        )
        $ do
          startGovernanceByAdmin contract
          hdl <- activateWallet
          void $ callEndpoint' @Deposit hdl (Deposit depoAmt)
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
            .&&. valueAtAddress Test.scriptAddress (== Test.nft 1)
        )
        $ do
          startGovernanceByAdmin contract
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
            .&&. valueAtAddress Test.scriptAddress (== Test.nft 1)
        )
        $ do
          startGovernanceByAdmin contract
          hdl <- activateWallet
          void $ callEndpoint' @Deposit hdl (Deposit 50)
          next

testCantDepositNegativeAmount :: TestTree
testCantDepositNegativeAmount =
  let (_, contract, _, activateWallet) = setup Test.fstWalletWithGOV
      errCheck _ e _ = case e of NegativeValue _ -> True; _ -> False
   in checkPredicateOptions
        Test.checkOptions
        "Cant deposit negative GOV amount"
        ( assertFailedTransaction errCheck
            .&&. valueAtAddress Test.scriptAddress (== Test.nft 1)
        )
        $ do
          startGovernanceByAdmin contract
          hdl <- activateWallet
          void $ callEndpoint' @Deposit hdl (Deposit (negate 2))
          next

-- withdraw tests
testFullWithdraw :: TestTree
testFullWithdraw =
  let (wallet, contract, _, activateWallet) = setup Test.fstWalletWithGOV
      depoAmt = 50
   in checkPredicateOptions
        Test.checkOptions
        "Full withdraw"
        ( assertNoFailedTransactions
            .&&. walletFundsChange wallet mempty
            .&&. valueAtAddress Test.scriptAddress (== Test.nft 1)
        )
        $ do
          startGovernanceByAdmin contract
          hdl <- activateWallet
          next
          void $ callEndpoint' @Deposit hdl (Deposit depoAmt)
          next
          void $ callEndpoint' @Withdraw hdl (Withdraw $ Test.xgov wallet depoAmt)
          next

testPartialWithdraw :: TestTree
testPartialWithdraw =
  let (wallet, contract, _, activateWallet) = setup Test.fstWalletWithGOV
      depoAmt = 50
      withdrawAmt = 20
      diff = depoAmt - withdrawAmt
   in checkPredicateOptions
        Test.checkOptions
        "Partial withdraw"
        ( assertNoFailedTransactions
            .&&. walletFundsChange wallet (Test.gov (negate diff) <> Test.xgov wallet diff)
            .&&. valueAtAddress Test.scriptAddress (== Test.gov diff <> Test.nft 1)
        )
        $ do
          startGovernanceByAdmin contract
          hdl <- activateWallet
          next
          void $ callEndpoint' @Deposit hdl (Deposit depoAmt)
          next
          void $ callEndpoint' @Withdraw hdl (Withdraw $ Test.xgov wallet withdrawAmt)
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
  let (wallet, contract, _, activateWallet) = setup Test.fstWalletWithGOV
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
            .&&. valueAtAddress Test.scriptAddress (== Test.gov depoAmt <> Test.nft 1)
        )
        $ do
          startGovernanceByAdmin contract
          hdl <- activateWallet
          void $ callEndpoint' @Deposit hdl (Deposit depoAmt)
          next
          void $ callEndpoint' @Withdraw hdl (Withdraw $ Test.xgov wallet (negate 1))
          next

testCanWithdrawOnlyxGov :: TestTree
testCanWithdrawOnlyxGov = error "TBD"
