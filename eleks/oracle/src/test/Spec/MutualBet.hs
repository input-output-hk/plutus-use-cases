{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Spec.MutualBet
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
import           Ledger.Oracle             (Observation, SignedMessage, signMessage)
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
import           Pab.Game                           
import qualified Plutus.Trace.Emulator              as Trace
import           PlutusTx.Monoid                    (inv)
import           Test.Tasty

slotCfg :: SlotConfig
slotCfg = def

oracleCurrency :: CurrencySymbol
oracleCurrency = "aa"

oracleParams :: OracleParams 
oracleParams = OracleParams{ opSymbol = oracleCurrency, opFees = 1_000_000 } 

oracleRequestToken :: OracleRequestToken
oracleRequestToken = OracleRequestToken
    { ortOperator = pubKeyHash $ walletPubKey oracleWallet
    , ortFee = opFees oracleParams
    }

oracleData ::  Oracle
oracleData = Oracle
    { oSymbol = opSymbol oracleParams
    , oRequestTokenSymbol = requestTokenSymbol oracleRequestToken
    , oOperator = pubKeyHash $ walletPubKey oracleWallet
    , oOperatorKey = walletPubKey oracleWallet
    , oFee = opFees oracleParams
    }

oracle ::  Oracle
oracle = Oracle
    { oSymbol = opSymbol oracleParams
    , oRequestTokenSymbol = requestTokenSymbol oracleRequestToken
    , oOperator = pubKeyHash $ walletPubKey oracleWallet
    , oOperatorKey = walletPubKey oracleWallet
    , oFee = opFees oracleParams
    }

mutualBetParams :: MutualBetParams
mutualBetParams =
    MutualBetParams
        { mbpGame = 1
        , mbpOracle = oracleData
        , mbpTeam1 = 1
        , mbpTeam2 = 2
        }

-- | 'EmulatorConfig' that includes 'theToken' in the initial distribution of Wallet 1.
auctionEmulatorCfg :: Trace.EmulatorConfig
auctionEmulatorCfg =
    let initialDistribution = defaultDist
    in (def & Trace.initialChainState .~ Left initialDistribution) & Trace.slotConfig .~ slotCfg

-- | 'CheckOptions' that includes our own 'auctionEmulatorCfg'.
options :: CheckOptions
options = set emulatorConfig auctionEmulatorCfg defaultCheckOptions

mutualBetContract :: Contract MutualBetOutput MutualBetStartSchema MutualBetError ()
mutualBetContract = mutualBetStart mutualBetParams

bettorContract :: ThreadToken -> Contract MutualBetOutput BettorSchema MutualBetError ()
bettorContract cur = mutualBetBettor slotCfg cur mutualBetParams

oracleContract :: Contract (Last Oracle) OracleSchema Text ()
oracleContract = runOracle oracleParams

signOracleTokenContract :: Oracle -> Contract Text EmptySchema Text ()
signOracleTokenContract oracle = listenOracleRequest oracle (walletPrivKey oracleWallet) findGameByIdContract

findGameByIdContract :: GameId -> Contract Text EmptySchema Text (Maybe Integer)
findGameByIdContract gameId = do 
    waitNSlots 20
    return $ Just trace1Winner 
    --liftIO $ (getWinnerTeamId <$> getGameById gameId)

w1, w2, w3, bettor1, bettor2 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3
w4 = Wallet 4
w5 = Wallet 5
betWallet = w1
bettor1 = w2
bettor2 = w3
oracleWallet = w5

trace1Bettor1Bet :: Ada
trace1Bettor1Bet = 10

trace1Bettor2Bet :: Ada
trace1Bettor2Bet = 10

trace1Winner :: Integer
trace1Winner = 1

mutualBetTrace1 :: Trace.EmulatorTrace ()
mutualBetTrace1 = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    oracleSignHdl <- Trace.activateContractWallet oracleWallet (signOracleTokenContract oracle)
    _ <- Trace.waitNSlots 5
    mutualBetHdl <- Trace.activateContractWallet betWallet mutualBetContract
    _ <- Trace.waitNSlots 5
    threadToken <- extractAssetClass mutualBetHdl
    Extras.logInfo $ "Trace thread token " ++ show threadToken
    bettor1Hdl <- Trace.activateContractWallet bettor1 (bettorContract threadToken)
    bettor2Hdl <- Trace.activateContractWallet bettor2 (bettorContract threadToken)
    _ <- Trace.waitNSlots 1
    let bet1Params = NewBetParams { nbpAmount = trace1Bettor1Bet, nbpOutcome = 1}
    Trace.callEndpoint @"bet" bettor1Hdl bet1Params
    _ <- Trace.waitNSlots 2
    let bet2Params = NewBetParams { nbpAmount = trace1Bettor2Bet, nbpOutcome = 0}
    Trace.callEndpoint @"bet" bettor2Hdl bet2Params
    _ <- Trace.waitNSlots 8


trace2WinningBid :: Ada
trace2WinningBid = 70

extractAssetClass :: Trace.ContractHandle MutualBetOutput MutualBetStartSchema MutualBetError -> Trace.EmulatorTrace ThreadToken
extractAssetClass handle = do
    t <- mutualBetThreadToken <$> Trace.observableState handle
    case t of
        Last (Just currency) -> pure currency
        _                    -> Trace.throwError (Trace.GenericError "currency not found")

trace1FinalState :: MutualBetOutput
trace1FinalState =
    MutualBetOutput
        { mutualBetState = Last $ Just $ Finished $
            [
                Bet{ amount = trace1Bettor2Bet
                , bettor = pubKeyHash (walletPubKey bettor2)
                , outcome = 0
                },
                Bet{ amount = trace1Bettor1Bet
                , bettor = pubKeyHash (walletPubKey bettor1)
                , outcome = 1
                }
            ]
        , mutualBetThreadToken = Last $ Just threadToken
        }

threadToken :: ThreadToken
threadToken =
    let con = getThreadToken :: Contract MutualBetOutput MutualBetStartSchema MutualBetError ThreadToken
        fld = Folds.instanceOutcome con (Trace.walletInstanceTag w1)
        getOutcome (Folds.Done a) = a
        getOutcome e              = error $ "not finished: " <> show e
    in
    either (error . show) (getOutcome . S.fst')
        $ Freer.run
        $ Freer.runError @Folds.EmulatorFoldErr
        $ Stream.foldEmulatorStreamM fld
        $ Stream.takeUntilSlot 10
        $ Trace.runEmulatorStream (options ^. emulatorConfig)
        $ do
            void $ Trace.activateContractWallet w1 (void con)
            Trace.waitNSlots 3

delay :: Integer -> Trace.EmulatorTrace ()
delay n = void $ Trace.waitNSlots $ fromIntegral n

tests :: TestTree
tests =
    testGroup "auction"
        [ checkPredicateOptions options "run an auction"
            (--assertDone mutualBetContract (Trace.walletInstanceTag w1) (const True) "mutual bet contract should be done"
            -- .&&. 
            assertDone (bettorContract threadToken) (Trace.walletInstanceTag bettor1) (const True) "bettor 1 contract should be done"
            .&&. assertDone (bettorContract threadToken) (Trace.walletInstanceTag bettor2) (const True) "bettor 2 contract should be done"
            .&&. assertAccumState (bettorContract threadToken) (Trace.walletInstanceTag bettor1) ((==) trace1FinalState ) "final state should be OK"
            .&&. walletFundsChange bettor1 (Ada.toValue ( trace1Bettor2Bet))
            .&&. walletFundsChange bettor2 (inv (Ada.toValue trace1Bettor2Bet))
            )
            mutualBetTrace1
        ]
