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
import           Contracts.Types     
import           Control.Monad                      (void, when)
import qualified Control.Monad.Freer                as Freer
import qualified Control.Monad.Freer.Error          as Freer
import           Control.Monad.Freer.Extras as Extras
import           Control.Monad.Freer.Extras.Log     (LogLevel (..))
import           Control.Monad.IO.Class             (liftIO)
import           Data.Aeson                         (Result (..))
import           Data.Default                       (Default (def))
import           Data.Monoid                        (Last (..))
import           Data.Text                          (Text, pack)
import           Data.Sort                          (sort)
import           Ledger                             (Ada, Slot (..), Value, pubKeyHash)
import qualified Ledger.Ada                         as Ada
import           Ledger.Index                       (ValidationError (ScriptFailure))
import           Ledger.Scripts                     (ScriptError (EvaluationError))
import           Ledger.Oracle                      (Observation, SignedMessage, signMessage, verifySignedMessageOffChain, verifySignedMessageConstraints)
import           Plutus.Contract                    hiding (currentSlot)
import           Plutus.Contract.Test               hiding (not)
import qualified Streaming.Prelude                  as S
import qualified Wallet.Emulator.Folds              as Folds
import qualified Wallet.Emulator.Stream             as Stream

import           Ledger.TimeSlot                    (SlotConfig)
import qualified Ledger.TimeSlot                    as TimeSlot
import qualified Ledger.Value                       as Value
import           Ledger.Value                       (CurrencySymbol)
import           Plutus.Contract.Test.ContractModel
import           Contracts.MutualBet
import           Contracts.Oracle                   
import qualified Plutus.Trace.Emulator              as Trace
import           PlutusTx.Monoid                    (inv)
import           Pab.Game                           (getGameById)
import           Test.Tasty

-- import           Data.Text.Prettyprint.Doc
-- import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)



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
oracleParams = OracleParams{ opSymbol = oracleCurrency, opFees = 3_000_000, opSigner = walletPrivKey oracleWallet } 

oracleRequestToken :: OracleRequestToken
oracleRequestToken = OracleRequestToken
    { ortOperator = pubKeyHash $ walletPubKey oracleWallet
    , ortFee = opFees oracleParams
    }
oracle ::  Oracle
oracle = Oracle
    { --oSymbol = opSymbol oracleParams,
      oRequestTokenSymbol = requestTokenSymbol oracleRequestToken
    , oOperator = pubKeyHash $ walletPubKey oracleWallet
    , oOperatorKey = walletPubKey oracleWallet
    , oFee = opFees oracleParams
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

w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3
w4 = Wallet 4
w5 = Wallet 5
oracleWallet = w1
oracleClientWallet = w2
otherWallet = w3

winTeamId:: Integer
winTeamId = 1

requestOracleTestState :: OracleData
requestOracleTestState = OracleData
    { ovGame = gameId
    , ovWinner = 0
    , ovRequestAddress = pubKeyHash $ walletPubKey oracleClientWallet
    , ovWinnerSigned = Nothing
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
    , ovWinner = 3
    , ovRequestAddress = pubKeyHash $ walletPubKey oracleClientWallet
    , ovWinnerSigned = Just $ signMessage 1 (walletPrivKey oracleWallet) 
    }

signOracleTrace :: Trace.EmulatorTrace ()
signOracleTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 20
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle gameId)
    void $ Trace.waitNSlots 3

useOracleTrace :: Trace.EmulatorTrace ()
useOracleTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle gameId)
    _ <- Trace.waitNSlots 5
    let updateParams = UpdateOracleParams{ uoGameId = gameId,  uoWinnerId = 1 }
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
    let updateParams = UpdateOracleParams{ uoGameId = gameId,  uoWinnerId = 1 }
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
    let updateParams = UpdateOracleParams{ uoGameId = gameId,  uoWinnerId = 1 }
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
            (== (Value.assetClassValue (requestTokenClassFromOracle oracle) 1)
            )
        .&&. walletFundsChange oracleWallet ((Ada.toValue . Ada.lovelaceOf $ (oFee oracle)))
        .&&. walletFundsChange oracleClientWallet (inv (Ada.toValue . Ada.lovelaceOf $ (oFee oracle)))
        .&&. dataAtAddress (oracleAddress oracle) (== requestOracleTestState)
        )
        requestOracleTrace
        ,
        checkPredicateOptions options "Should sign oracle data"
        ( 
            assertNoFailedTransactions
            .&&. valueAtAddress (oracleAddress oracle)
                (== (Value.assetClassValue (requestTokenClassFromOracle oracle) 1))
            .&&. walletFundsChange oracleWallet (Ada.toValue . Ada.lovelaceOf $ (oFee oracle))
            -- .&&. dataAtAddress (oracleAddress oracle) (== signOracleTestState)
        )
        signOracleTrace
        ,
        checkPredicateOptions options "Should use oracle data"
        ( 
            assertNoFailedTransactions
            .&&. assertNotDone (useOracleContract oracle)
                (Trace.walletInstanceTag oracleClientWallet)
                "use contract should not fail"
            .&&. walletFundsChange oracleClientWallet (
                inv (Ada.toValue . Ada.lovelaceOf $ (oFee oracle))
                <> (Value.assetClassValue (requestTokenClassFromOracle oracle) 1)
                )
        )
        useOracleTrace
        ,
        checkPredicateOptions options "Only oracle signed request can be used"
        ( 
            assertFailedTransaction (\_ err _ -> case err of {ScriptFailure (EvaluationError ["value signed by oracle", "Pd"] _) -> True; _ -> False  })
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
        checkPredicateOptions options "Should not get used games"
        ( 
            assertNoFailedTransactions
            .&&. assertAccumState oracleContract (Trace.walletInstanceTag oracleWallet)
                    (\case Last (Just (Games gameIds)) -> 
                                gameIds == [];
                           _ -> False)
                    "should get active game"
        )
        getOnlyActiveGamesTrace
        ]
