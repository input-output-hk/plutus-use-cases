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
import           Data.Default                       (Default (def))
import           Data.Monoid                        (Last (..))
import           Data.Text                          (Text, pack)
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
oracleParams = OracleParams{ opSymbol = oracleCurrency, opFees = 3_000_000 } 

oracleRequestToken :: OracleRequestToken
oracleRequestToken = OracleRequestToken
    { ortOperator = pubKeyHash $ walletPubKey oracleWallet
    , ortFee = opFees oracleParams
    }
oracle ::  Oracle
oracle = Oracle
    { oSymbol = opSymbol oracleParams
    , oRequestTokenSymbol = requestTokenSymbol oracleRequestToken
    , oOperator = pubKeyHash $ walletPubKey oracleWallet
    , oOperatorKey = walletPubKey oracleWallet
    , oFee = opFees oracleParams
    }

gameId :: Integer
gameId = 1

oracleContract :: Contract (Last Oracle) OracleSchema Text ()
oracleContract = runOracle oracleParams

requestOracleTokenContract :: Oracle -> Contract Text EmptySchema Text ()
requestOracleTokenContract oracle = requestOracleForAddress oracle gameId

signOracleTokenContract :: Oracle -> Contract Text EmptySchema Text ()
signOracleTokenContract oracle = listenOracleRequest oracle (walletPrivKey oracleWallet) findGameByIdContract

useOracleContract :: Oracle -> Contract Text UseOracleSchema Text ()
useOracleContract oracle = useOracle oracle

findGameByIdContract :: GameId -> Contract Text EmptySchema Text (Maybe Integer)
findGameByIdContract gameId = return $ Just 1--liftIO $ (getWinnerTeamId <$> getGameById gameId)

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
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle)
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
    _ <- Trace.waitNSlots 3
    oracleSignHdl <- Trace.activateContractWallet oracleWallet (signOracleTokenContract oracle)
    _ <- Trace.waitNSlots 20
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle)
    void $ Trace.waitNSlots 3

useOracleTrace :: Trace.EmulatorTrace ()
useOracleTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    oracleSignHdl <- Trace.activateContractWallet oracleWallet (signOracleTokenContract oracle)
    _ <- Trace.waitNSlots 3
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle)
    void $ Trace.waitNSlots 40

    useOracleHdl <- Trace.activateContractWallet oracleClientWallet (useOracleContract oracle)
    let useOracleParams = UseOracleParams { uoGame = gameId }
    Trace.callEndpoint @"use" useOracleHdl useOracleParams
    void $ Trace.waitNSlots 1

useOracleNotOwnerFailTrace :: Trace.EmulatorTrace ()
useOracleNotOwnerFailTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    oracleSignHdl <- Trace.activateContractWallet oracleWallet (signOracleTokenContract oracle)
    _ <- Trace.waitNSlots 3
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle)
    void $ Trace.waitNSlots 40

    useOracleHdl <- Trace.activateContractWallet otherWallet (useOracleContract oracle)
    let useOracleParams = UseOracleParams { uoGame = gameId }
    Trace.callEndpoint @"use" useOracleHdl useOracleParams
    void $ Trace.waitNSlots 1


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
        checkPredicateOptions options "Only request owner could use oracle data"
        ( 
            assertContractError (useOracleContract oracle) (Trace.walletInstanceTag otherWallet) (\case { "no oracle request" -> True; _ -> False}) "failed to find oracle token"
        )
        useOracleNotOwnerFailTrace
        ]