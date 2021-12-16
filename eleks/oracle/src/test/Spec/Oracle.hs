{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Spec.Oracle
    ( tests,
      requestOracleTrace
    ) where

import Control.Lens
import Control.Monad (void)
import Data.Default (Default (def))
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Sort (sort)
import Data.Text (Text)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (PaymentPrivateKey(..))
import Ledger.CardanoWallet qualified as CW
import Ledger.Index (ValidationError (ScriptFailure))
import Ledger.Scripts (ScriptError (EvaluationError))
import Plutus.Contract hiding (currentSlot)
import Plutus.Contract.Test hiding (not)
import Types.Game
import Contracts.Oracle
import Ledger.TimeSlot (SlotConfig)
import Ledger.Value qualified as Value
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Monoid (inv)
import Test.Tasty
import Wallet.Emulator.Wallet (walletToMockWallet)

auctionEmulatorCfg :: Trace.EmulatorConfig
auctionEmulatorCfg =
    let initialDistribution = defaultDist
    in (def & Trace.initialChainState .~ Left initialDistribution) & Trace.slotConfig .~ slotCfg

options :: CheckOptions
options = set emulatorConfig auctionEmulatorCfg defaultCheckOptions

slotCfg :: SlotConfig
slotCfg = def

oracleParams :: OracleParams
oracleParams = OracleParams
    { opFees = 2_500_000
    , opSigner = encodeKeyToDto $ unPaymentPrivateKey oraclePrivateKey
    }

oracleRequestToken :: OracleRequestToken
oracleRequestToken = OracleRequestToken
    { ortOperator = mockWalletPaymentPubKeyHash oracleWallet
    , ortFee = opFees oracleParams
    }
oracle ::  Oracle
oracle = Oracle
    { oRequestTokenSymbol = requestTokenSymbol oracleRequestToken
    , oOperator = mockWalletPaymentPubKeyHash oracleWallet
    , oOperatorKey = mockWalletPaymentPubKey oracleWallet
    , oFee = opFees oracleParams
    }

game1Id :: Integer
game1Id = 1

game2Id :: Integer
game2Id = 2

oracleContract :: Contract (Last OracleContractState) OracleSchema Text ()
oracleContract = runOracle oracleParams

requestOracleTokenContract :: Oracle -> GameId -> Contract Text EmptySchema Text ()
requestOracleTokenContract oracl gameId = requestOracleForAddress oracl gameId

redeemOracleContract :: Oracle -> Contract Text RedeemOracleSchema Text ()
redeemOracleContract oracl = redeemOracle oracl

oracleWallet, oracleClientWallet, otherWallet :: Wallet
oracleWallet = w1
oracleClientWallet = w2
otherWallet = w3

oraclePrivateKey :: PaymentPrivateKey
oraclePrivateKey = CW.paymentPrivateKey $ fromMaybe(error $ "priv key") $ walletToMockWallet oracleWallet

requestOracleTestState :: OracleData
requestOracleTestState = OracleData
    { ovGame = game1Id
    , ovRequestAddress = mockWalletPaymentPubKeyHash oracleClientWallet
    , ovSignedMessage = Nothing
    }

requestOracleTrace :: Trace.EmulatorTrace ()
requestOracleTrace = do
    _ <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    _ <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle game1Id)
    void $ Trace.waitNSlots 3

{-
signOracleTestState :: OracleData
signOracleTestState = OracleData
    { ovGame = gameId
    , ovRequestAddress = walletPubKeyHash oracleClientWallet
    , ovSignedMessage = Just $ signMessage OracleSignedMessage{ osmGameId = gameId, osmWinnerId = 0, osmGameStatus = NS } oraclePrivateKey ""
    }
-}

updateOracleTrace :: Trace.EmulatorTrace ()
updateOracleTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 20
    _ <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle game1Id)
    void $ Trace.waitNSlots 3
    let updateParams = UpdateOracleParams{ uoGameId = game1Id, uoWinnerId = 0, uoGameStatus = NS }
    Trace.callEndpoint @"update" oracleHdl updateParams
    void $ Trace.waitNSlots 1

invalidUpdateOracleTrace :: Trace.EmulatorTrace ()
invalidUpdateOracleTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 20
    _ <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle game1Id)
    void $ Trace.waitNSlots 3
    let updateParamsNs = UpdateOracleParams{ uoGameId = game1Id, uoWinnerId = 0, uoGameStatus = NS }
    Trace.callEndpoint @"update" oracleHdl updateParamsNs
    void $ Trace.waitNSlots 5
    --cannot complete finish game if it was not started
    let updateParamsFt = UpdateOracleParams{ uoGameId = game1Id, uoWinnerId = 1, uoGameStatus = FT }
    Trace.callEndpoint @"update" oracleHdl updateParamsFt
    void $ Trace.waitNSlots 5

updateOracleFromNotStartedToLiveTrace :: Trace.EmulatorTrace ()
updateOracleFromNotStartedToLiveTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 20
    _ <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle game1Id)
    void $ Trace.waitNSlots 3
    let updateParamsLive = UpdateOracleParams{ uoGameId = game1Id, uoWinnerId = 0, uoGameStatus = LIVE }
    Trace.callEndpoint @"update" oracleHdl updateParamsLive
    void $ Trace.waitNSlots 5
    --cannot complete game without in progress state
    let updateParamsFt = UpdateOracleParams{ uoGameId = game1Id, uoWinnerId = 1, uoGameStatus = FT }
    Trace.callEndpoint @"update" oracleHdl updateParamsFt
    void $ Trace.waitNSlots 5

redeemOracleTrace :: Trace.EmulatorTrace ()
redeemOracleTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    _ <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle game1Id)
    _ <- Trace.waitNSlots 5
    let updateParams = UpdateOracleParams{ uoGameId = game1Id, uoWinnerId = 1, uoGameStatus = FT }
    Trace.callEndpoint @"update" oracleHdl updateParams
    _ <- Trace.waitNSlots 5
    redeemOracleHdl <- Trace.activateContractWallet oracleClientWallet (redeemOracleContract oracle)
    let redeemOracleParams = RedeemOracleParams { roGame = game1Id }
    Trace.callEndpoint @"redeem" redeemOracleHdl redeemOracleParams
    void $ Trace.waitNSlots 1

{-
redeemFailIfNotSignedTrace :: Trace.EmulatorTrace ()
redeemFailIfNotSignedTrace = do
    _ <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    _ <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle game1Id)
    _ <- Trace.waitNSlots 5
    redeemOracleHdl <- Trace.activateContractWallet oracleClientWallet (redeemOracleContract oracle)
    let redeemOracleParams = redeemOracleParams { roGame = game1Id }
    Trace.callEndpoint @"redeem" redeemOracleHdl redeemOracleParams
    void $ Trace.waitNSlots 1
-}

redeemOracleNotOwnerFailTrace :: Trace.EmulatorTrace ()
redeemOracleNotOwnerFailTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    _ <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle game1Id)
    _ <- Trace.waitNSlots 5
    let updateParams = UpdateOracleParams{ uoGameId = game1Id,  uoWinnerId = 1, uoGameStatus = FT }
    Trace.callEndpoint @"update" oracleHdl updateParams
    _ <-  Trace.waitNSlots 5
    redeemOracleHdl <- Trace.activateContractWallet otherWallet (redeemOracleContract oracle)
    let redeemOracleParams = RedeemOracleParams { roGame = game1Id }
    Trace.callEndpoint @"redeem" redeemOracleHdl redeemOracleParams
    void $ Trace.waitNSlots 1

getActiveGamesTrace:: Trace.EmulatorTrace ()
getActiveGamesTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    _ <-Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle game1Id)
    void $ Trace.waitNSlots 3
    _ <- Trace.activateContract oracleClientWallet (requestOracleTokenContract oracle game2Id) ("request2")
    void $ Trace.waitNSlots 3
    Trace.callEndpoint @"games" oracleHdl ()

getOnlyActiveGamesTrace:: Trace.EmulatorTrace ()
getOnlyActiveGamesTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    _ <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle game1Id)
    _ <- Trace.waitNSlots 5
    let updateParams = UpdateOracleParams{ uoGameId = game1Id,  uoWinnerId = 1, uoGameStatus = FT }
    Trace.callEndpoint @"update" oracleHdl updateParams
    _ <- Trace.waitNSlots 5
    Trace.callEndpoint @"games" oracleHdl ()
    void $ Trace.waitNSlots 3

getInProgressGameAsActiveTrace:: Trace.EmulatorTrace ()
getInProgressGameAsActiveTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    _ <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle game1Id)
    _ <- Trace.waitNSlots 5
    let updateParams = UpdateOracleParams{ uoGameId = game1Id,  uoWinnerId = 0, uoGameStatus = LIVE }
    Trace.callEndpoint @"update" oracleHdl updateParams
    _ <- Trace.waitNSlots 5
    Trace.callEndpoint @"games" oracleHdl ()
    void $ Trace.waitNSlots 3

tests :: TestTree
tests =
    testGroup "oracle"
        [
        checkPredicateOptions options "request oracle token"
        (
        assertNoFailedTransactions
        .&&. valueAtAddress (oracleAddress oracle)
            (== (Value.assetClassValue (requestTokenClassFromOracle oracle) 1
                <> Ada.toValue(Ledger.minAdaTxOut)
                )
            )
        .&&. walletFundsChange oracleWallet (Ada.toValue (oFee oracle))
        .&&. walletFundsChange oracleClientWallet (inv (Ada.toValue (oFee oracle + Ledger.minAdaTxOut)))
        .&&. dataAtAddress (oracleAddress oracle) (== [requestOracleTestState])
        )
        requestOracleTrace
        ,
        checkPredicateOptions options "Should update oracle data"
        (
            assertNoFailedTransactions
            .&&. valueAtAddress (oracleAddress oracle)
                (== (Value.assetClassValue (requestTokenClassFromOracle oracle) 1
                    <> Ada.toValue (Ledger.minAdaTxOut)
                    )
                )
            .&&. walletFundsChange oracleWallet (Ada.toValue (oFee oracle))
            -- .&&. dataAtAddress (oracleAddress oracle) (== signOracleTestState)
        )
        updateOracleTrace
        ,
        checkPredicateOptions options "Should update oracle data from NS to LIVE"
        (
            assertNoFailedTransactions
            .&&. valueAtAddress (oracleAddress oracle)
                (== (Value.assetClassValue (requestTokenClassFromOracle oracle) 1
                    <> Ada.toValue (Ledger.minAdaTxOut))
                )
            .&&. walletFundsChange oracleWallet (Ada.toValue (oFee oracle))
        )
        updateOracleFromNotStartedToLiveTrace
        ,
        checkPredicateOptions options "Should fail on incorrect update oracle data"
        (
            assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["update data is invalid", "PT5"] _) -> True; _ -> False  })
        )
        invalidUpdateOracleTrace
        ,
        checkPredicateOptions options "Should redeem oracle data"
        (
            assertNoFailedTransactions
            .&&. assertNotDone (redeemOracleContract oracle)
                (Trace.walletInstanceTag oracleClientWallet)
                "redeem contract should not fail"
            .&&. valueAtAddress (oracleAddress oracle)
                (== (Ada.toValue 0)
                )
            .&&. walletFundsChange oracleClientWallet (
                inv (Ada.toValue $ oFee oracle)
                )
        )
        redeemOracleTrace
        ,
        checkPredicateOptions options "Only request owner could redeem oracle data"
        (
            assertContractError (redeemOracleContract oracle) (Trace.walletInstanceTag otherWallet) (\case { "no oracle request" -> True; _ -> False}) "failed to find oracle token"
        )
        redeemOracleNotOwnerFailTrace
        ,
        checkPredicateOptions options "Should get active game"
        (
            assertNoFailedTransactions
            .&&. assertAccumState oracleContract (Trace.walletInstanceTag oracleWallet)
                    (\case Last (Just (Games gameIds)) ->
                                sort gameIds == [game1Id, game2Id];
                           _ -> False)
                    "should get active game"
        )
        getActiveGamesTrace
        ,
        checkPredicateOptions options "Should not get completed games as active"
        (
            assertNoFailedTransactions
            .&&. assertAccumState oracleContract (Trace.walletInstanceTag oracleWallet)
                    (\case Last (Just (Games gameIds)) ->
                                gameIds == [];
                           _ -> False)
                    "should not get redeemd"
        )
        getOnlyActiveGamesTrace
        ,
        checkPredicateOptions options "Get in progress games as active"
        (
            assertNoFailedTransactions
            .&&. assertAccumState oracleContract (Trace.walletInstanceTag oracleWallet)
                    (\case Last (Just (Games gameIds)) ->
                                gameIds == [game1Id];
                           _ -> False)
                    "should get in progress game"
        )
        getInProgressGameAsActiveTrace
        ]


