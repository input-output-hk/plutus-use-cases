{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Spec.Oracle
    ( tests
    ) where

import           Control.Lens
import           Types.Game   
import           Control.Monad                      (void, when)
import qualified Control.Monad.Freer                as Freer
import qualified Control.Monad.Freer.Error          as Freer
import           Control.Monad.Freer.Extras as Extras
import           Control.Monad.Freer.Extras.Log     (LogLevel (..))
import           Control.Monad.IO.Class             (liftIO)
import           Data.Aeson                         (Result (..))
import           Data.Default                       (Default (def))
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid                        (Last (..))
import           Data.Text                          (Text, pack)
import           Data.Sort                          (sort)
import           Ledger                             (Ada, Slot (..), Value, pubKeyHash)
import qualified Ledger.Ada                         as Ada
import           Ledger.Crypto                      (PrivateKey, PubKey)
import           Ledger.Index                       (ValidationError (ScriptFailure))
import           Ledger.Scripts                     (ScriptError (EvaluationError))
import           Ledger.Oracle                      (Observation, SignedMessage, signMessage, verifySignedMessageOffChain, verifySignedMessageConstraints)
import           Plutus.Contract                    hiding (currentSlot)
import           Plutus.Contract.Test               hiding (not)
import qualified Streaming.Prelude                  as S
import qualified Wallet.Emulator.Folds              as Folds
import qualified Wallet.Emulator.Stream             as Stream
import           Wallet.Emulator.Wallet             (emptyWalletState, ownPublicKey, ownPrivateKey)

import           Ledger.TimeSlot                    (SlotConfig)
import qualified Ledger.TimeSlot                    as TimeSlot
import qualified Ledger.Value                       as Value
import           Ledger.Value                       (CurrencySymbol)
import           Plutus.Contract.Test.ContractModel
import           Contracts.MutualBet
import           Contracts.Oracle                   
import qualified Plutus.Trace.Emulator              as Trace
import           PlutusTx.Monoid                    (inv)
import           Test.Tasty
import           Types.Game

minUtxo:: Ada
minUtxo = 2_000_000

minUtxoVal:: Value
minUtxoVal = Ada.toValue minUtxo

marketUtxoVal:: Value
marketUtxoVal = Ada.toValue minUtxo

minUtxoValN:: Ada -> Value
minUtxoValN n = Ada.toValue (n * minUtxo)

getWalletPubKey:: Wallet -> PubKey
getWalletPubKey = ownPublicKey . fromMaybe (error "not a mock wallet") . emptyWalletState

getWalletPrivKey:: Wallet -> PrivateKey
getWalletPrivKey = ownPrivateKey . fromMaybe (error "not a mock wallet") . emptyWalletState

auctionEmulatorCfg :: Trace.EmulatorConfig
auctionEmulatorCfg =
    let initialDistribution = defaultDist
    in (def & Trace.initialChainState .~ Left initialDistribution) & Trace.slotConfig .~ slotCfg

options :: CheckOptions
options = set emulatorConfig auctionEmulatorCfg defaultCheckOptions

slotCfg :: SlotConfig
slotCfg = def

oracleCurrency :: CurrencySymbol
oracleCurrency = "aa"

oracleParams :: OracleParams 
oracleParams = OracleParams
    { --opSymbol = oracleCurrency,
      opFees = 2_000_000
    , opCollateral = 2_000_000
    , opSigner = encodeKeyToDto $ oraclePrivateKey
    } 

oracleRequestToken :: OracleRequestToken
oracleRequestToken = OracleRequestToken
    { ortOperator = walletPubKeyHash oracleWallet
    , ortFee = opFees oracleParams
    , ortCollateral = opCollateral oracleParams
    }
oracle ::  Oracle
oracle = Oracle
    { --oSymbol = opSymbol oracleParams,
      oRequestTokenSymbol = requestTokenSymbol oracleRequestToken
    , oOperator = walletPubKeyHash oracleWallet
    , oOperatorKey = getWalletPubKey oracleWallet
    , oFee = opFees oracleParams
    , oCollateral = opCollateral oracleParams
    }

gameId :: Integer
gameId = 1

game2Id :: Integer
game2Id = 2

oracleContract :: Contract (Last OracleContractState) OracleSchema Text ()
oracleContract = runOracle oracleParams

requestOracleTokenContract :: Oracle -> GameId -> Contract Text EmptySchema Text ()
requestOracleTokenContract oracle gameId = requestOracleForAddress oracle gameId

useOracleContract :: Oracle -> Contract Text UseOracleSchema Text ()
useOracleContract oracle = useOracle oracle

oracleWallet, oracleClientWallet, otherWallet :: Wallet
oracleWallet = w1
oracleClientWallet = w2
otherWallet = w3

oraclePrivateKey :: PrivateKey
oraclePrivateKey = getWalletPrivKey oracleWallet

winTeamId:: Integer
winTeamId = 1

requestOracleTestState :: OracleData
requestOracleTestState = OracleData
    { ovGame = gameId
    , ovRequestAddress = walletPubKeyHash oracleClientWallet
    , ovSignedMessage = Nothing
    }

requestOracleTrace :: Trace.EmulatorTrace ()
requestOracleTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle gameId)
    void $ Trace.waitNSlots 3

signOracleTestState :: OracleData
signOracleTestState = OracleData
    { ovGame = gameId
    , ovRequestAddress = walletPubKeyHash oracleClientWallet
    , ovSignedMessage = Just $ signMessage OracleSignedMessage{ osmGameId = gameId, osmWinnerId = 0, osmGameStatus = NS } oraclePrivateKey
    }

updateOracleTrace :: Trace.EmulatorTrace ()
updateOracleTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 20
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle gameId)
    void $ Trace.waitNSlots 3
    let updateParams = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = 0, uoGameStatus = NS }
    Trace.callEndpoint @"update" oracleHdl updateParams
    void $ Trace.waitNSlots 1

invalidUpdateOracleTrace :: Trace.EmulatorTrace ()
invalidUpdateOracleTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 20
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle gameId)
    void $ Trace.waitNSlots 3
    let updateParams = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = 0, uoGameStatus = NS }
    Trace.callEndpoint @"update" oracleHdl updateParams
    void $ Trace.waitNSlots 5
    --cannot complete finish game if it was not started
    let updateParams = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = 1, uoGameStatus = FT }
    Trace.callEndpoint @"update" oracleHdl updateParams
    void $ Trace.waitNSlots 5

updateOracleFromNotStartedToLiveTrace :: Trace.EmulatorTrace ()
updateOracleFromNotStartedToLiveTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 20
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle gameId)
    void $ Trace.waitNSlots 3
    let updateParams = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = 0, uoGameStatus = LIVE }
    Trace.callEndpoint @"update" oracleHdl updateParams
    void $ Trace.waitNSlots 5
    --cannot complete game without in progress state
    let updateParams = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = 1, uoGameStatus = FT }
    Trace.callEndpoint @"update" oracleHdl updateParams
    void $ Trace.waitNSlots 5

useOracleTrace :: Trace.EmulatorTrace ()
useOracleTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle gameId)
    _ <- Trace.waitNSlots 5
    let updateParams = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = 1, uoGameStatus = FT }
    Trace.callEndpoint @"update" oracleHdl updateParams
    _ <- Trace.waitNSlots 5   
    useOracleHdl <- Trace.activateContractWallet oracleClientWallet (useOracleContract oracle)
    let useOracleParams = UseOracleParams { uoGame = gameId }
    Trace.callEndpoint @"use" useOracleHdl useOracleParams
    void $ Trace.waitNSlots 1

useFailIfNotSignedTrace :: Trace.EmulatorTrace ()
useFailIfNotSignedTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle gameId)
    _ <- Trace.waitNSlots 5 
    useOracleHdl <- Trace.activateContractWallet oracleClientWallet (useOracleContract oracle)
    let useOracleParams = UseOracleParams { uoGame = gameId }
    Trace.callEndpoint @"use" useOracleHdl useOracleParams
    void $ Trace.waitNSlots 1

useOracleNotOwnerFailTrace :: Trace.EmulatorTrace ()
useOracleNotOwnerFailTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle gameId)
    _ <- Trace.waitNSlots 5
    let updateParams = UpdateOracleParams{ uoGameId = gameId,  uoWinnerId = 1, uoGameStatus = FT }
    Trace.callEndpoint @"update" oracleHdl updateParams
    _ <-  Trace.waitNSlots 5
    useOracleHdl <- Trace.activateContractWallet otherWallet (useOracleContract oracle)
    let useOracleParams = UseOracleParams { uoGame = gameId }
    Trace.callEndpoint @"use" useOracleHdl useOracleParams
    void $ Trace.waitNSlots 1

getActiveGamesTrace:: Trace.EmulatorTrace ()
getActiveGamesTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle gameId)
    void $ Trace.waitNSlots 3
    Trace.activateContract oracleClientWallet (requestOracleTokenContract oracle game2Id) ("request2")
    void $ Trace.waitNSlots 3
    Trace.callEndpoint @"games" oracleHdl ()

getOnlyActiveGamesTrace:: Trace.EmulatorTrace ()
getOnlyActiveGamesTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle gameId)
    _ <- Trace.waitNSlots 5
    let updateParams = UpdateOracleParams{ uoGameId = gameId,  uoWinnerId = 1, uoGameStatus = FT }
    Trace.callEndpoint @"update" oracleHdl updateParams
    _ <- Trace.waitNSlots 5   
    Trace.callEndpoint @"games" oracleHdl ()
    void $ Trace.waitNSlots 3

getInProgressGameAsActiveTrace:: Trace.EmulatorTrace ()
getInProgressGameAsActiveTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle gameId)
    _ <- Trace.waitNSlots 5
    let updateParams = UpdateOracleParams{ uoGameId = gameId,  uoWinnerId = 0, uoGameStatus = LIVE }
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
                <> Ada.toValue(oCollateral oracle)
                )
            )
        .&&. walletFundsChange oracleWallet (Ada.toValue (oFee oracle))
        .&&. walletFundsChange oracleClientWallet (inv (Ada.toValue (oFee oracle + oCollateral oracle)))
        .&&. dataAtAddress (oracleAddress oracle) (== [requestOracleTestState])
        )
        requestOracleTrace
        ,
        checkPredicateOptions options "Should update oracle data"
        ( 
            assertNoFailedTransactions
            .&&. valueAtAddress (oracleAddress oracle)
                (== (Value.assetClassValue (requestTokenClassFromOracle oracle) 1 
                    <> Ada.toValue (oCollateral oracle)
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
                    <> Ada.toValue (oCollateral oracle))    
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
        checkPredicateOptions options "Should use oracle data"
        ( 
            assertNoFailedTransactions
            .&&. assertNotDone (useOracleContract oracle)
                (Trace.walletInstanceTag oracleClientWallet)
                "use contract should not fail"
            .&&. valueAtAddress (oracleAddress oracle)
                (== (Ada.toValue 0) 
                )
            .&&. walletFundsChange oracleClientWallet (
                inv (Ada.toValue $ oFee oracle)
                )
        )
        useOracleTrace
        ,
        checkPredicateOptions options "Only oracle signed request can be used"
        ( 
            assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["value signed by oracle", "PT5"] _) -> True; _ -> False  })
        )
        useFailIfNotSignedTrace
        ,
        checkPredicateOptions options "Only request owner could use oracle data"
        ( 
            assertContractError (useOracleContract oracle) (Trace.walletInstanceTag otherWallet) (\case { "no oracle request" -> True; _ -> False}) "failed to find oracle token"
        )
        useOracleNotOwnerFailTrace
        ,
        checkPredicateOptions options "Should get active game"
        ( 
            assertNoFailedTransactions
            .&&. assertAccumState oracleContract (Trace.walletInstanceTag oracleWallet)
                    (\case Last (Just (Games gameIds)) -> 
                                sort gameIds == [gameId, game2Id];
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
                    "should not get used"
        )
        getOnlyActiveGamesTrace
        ,
        checkPredicateOptions options "Get in progress games as active"
        ( 
            assertNoFailedTransactions
            .&&. assertAccumState oracleContract (Trace.walletInstanceTag oracleWallet)
                    (\case Last (Just (Games gameIds)) -> 
                                gameIds == [gameId];
                           _ -> False)
                    "should get in progress game"
        )
        getInProgressGameAsActiveTrace
        ]
