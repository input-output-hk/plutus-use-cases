{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Spec.MutualBetSM
    ( tests
    ) where

import Contracts.MutualBetSM
import Contracts.Oracle
import Control.Lens
import Control.Monad (void)
import Control.Monad.Freer qualified as Freer
import Control.Monad.Freer.Error qualified as Freer
import Control.Monad.Freer.Extras as Extras
import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Default (Default (def))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid (Last (..))
import Data.Text (Text, isInfixOf)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Ledger (Ada, minAdaTxOut)
import Ledger.Ada qualified as Ada
import Ledger.Crypto (PrivateKey, PubKey)
import Ledger.TimeSlot (SlotConfig)
import Plutus.Contract hiding (currentSlot)
import Plutus.Contract.Test hiding (not)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.Trace.Emulator.Types (ContractInstanceLog, _ContractLog, cilMessage)
import PlutusTx.Monoid (inv)
import Streaming.Prelude qualified as S
import Test.Tasty
import Types.Game
import Wallet.Emulator.Folds qualified as Folds
import Wallet.Emulator.MultiAgent (EmulatorTimeEvent, eteEvent)
import Wallet.Emulator.Stream qualified as Stream
import Wallet.Emulator.Wallet (emptyWalletState, ownPrivateKey, ownPublicKey)

slotCfg :: SlotConfig
slotCfg = def

getWalletPubKey:: Wallet -> PubKey
getWalletPubKey = ownPublicKey . fromMaybe (error "not a mock wallet") . emptyWalletState

getWalletPrivKey:: Wallet -> PrivateKey
getWalletPrivKey = ownPrivateKey . fromMaybe (error "not a mock wallet") . emptyWalletState

oracleParams :: OracleParams
oracleParams = OracleParams
    { opFees = 3_000_000
    , opSigner = encodeKeyToDto $ oraclePrivateKey
    }

oracleRequestToken :: OracleRequestToken
oracleRequestToken = OracleRequestToken
    { ortOperator = walletPubKeyHash oracleWallet
    , ortFee = opFees oracleParams
    }

oracle ::  Oracle
oracle = Oracle
    { --oSymbol = opSymbol oracleParams,
      oRequestTokenSymbol = requestTokenSymbol oracleRequestToken
    , oOperator = walletPubKeyHash oracleWallet
    , oOperatorKey = getWalletPubKey oracleWallet
    , oFee = opFees oracleParams
    }

gameId :: GameId
gameId = 1

team1Id :: TeamId
team1Id = 1

team2Id :: TeamId
team2Id = 2

mutualBetParams :: MutualBetParams
mutualBetParams =
    MutualBetParams
        { mbpGame = gameId
        , mbpOracle = oracle
        , mbpOwner = walletPubKeyHash betOwnerWallet
        , mbpTeam1 = team1Id
        , mbpTeam2 = team2Id
        , mbpMinBet = 4_000_000
        , mbpBetFee = 5_000_000
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

oracleContract :: Contract (Last OracleContractState) OracleSchema Text ()
oracleContract = runOracle oracleParams

betOwnerWallet, bettor1, bettor2, bettor3, oracleWallet :: Wallet
betOwnerWallet = w1
bettor1 = w2
bettor2 = w3
bettor3 = w4
oracleWallet = w5

oraclePrivateKey :: PrivateKey
oraclePrivateKey = getWalletPrivKey oracleWallet

trace1Bettor1Bet :: Integer
trace1Bettor1Bet = 10_000_000

trace1Bettor2Bet :: Integer
trace1Bettor2Bet = 10_000_000

trace2Bettor1Bet :: Integer
trace2Bettor1Bet = 10_000_000

trace2Bettor2Bet :: Integer
trace2Bettor2Bet = 10_000_000

trace2Bettor3Bet :: Integer
trace2Bettor3Bet = 10_000_000

cancelBettorBet :: Integer
cancelBettorBet = 10_000_000

trace2Bettor1WinShare, trace2Bettor2WinShare, trace2Bettor3WinShare :: Ada
trace2Bettor1WinShare = Ada.lovelaceOf 5_000_000
trace2Bettor2WinShare = Ada.lovelaceOf 0
trace2Bettor3WinShare = Ada.lovelaceOf 5_000_000

mutualBetSuccessTrace :: Trace.EmulatorTrace ()
mutualBetSuccessTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 5
    mutualBetHdl <- Trace.activateContractWallet betOwnerWallet mutualBetContract
    _ <- Trace.waitNSlots 5
    threadTkn <- extractAssetClass mutualBetHdl
    Extras.logInfo $ "Trace thread token " ++ show threadTkn
    bettor1Hdl <- Trace.activateContractWallet bettor1 (bettorContract threadTkn)
    bettor2Hdl <- Trace.activateContractWallet bettor2 (bettorContract threadTkn)
    _ <- Trace.waitNSlots 1
    let bet1Params = BetParams { nbpAmount = trace1Bettor1Bet, nbpWinnerId = team1Id}
    Trace.callEndpoint @"bet" bettor1Hdl bet1Params
    _ <- Trace.waitNSlots 10
    let bet2Params = BetParams { nbpAmount = trace1Bettor2Bet, nbpWinnerId = team2Id}
    Trace.callEndpoint @"bet" bettor2Hdl bet2Params
    let updateParamsLive = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = 0, uoGameStatus = LIVE }
    Trace.callEndpoint @"update" oracleHdl updateParamsLive
    void $ Trace.waitNSlots 10
    let updateParamsFt = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = team1Id, uoGameStatus = FT }
    Trace.callEndpoint @"update" oracleHdl updateParamsFt
    void $ Trace.waitNSlots 5


mutualBetSuccessTraceFinalState :: MutualBetOutput
mutualBetSuccessTraceFinalState =
    MutualBetOutput
        { mutualBetState = Last $ Just $ Finished $
            [
                Bet{ betAmount = Ada.lovelaceOf trace1Bettor2Bet
                , betBettor = walletPubKeyHash bettor2
                , betTeamId = team2Id
                , betWinShare = Ada.lovelaceOf 0
                },
                Bet{ betAmount = Ada.lovelaceOf trace1Bettor1Bet
                , betBettor = walletPubKeyHash bettor1
                , betTeamId = team1Id
                , betWinShare = Ada.lovelaceOf trace1Bettor2Bet
                }
            ]
        , mutualBetThreadToken = Last $ Just threadToken
        }

mutualBetSuccessTrace2 :: Trace.EmulatorTrace ()
mutualBetSuccessTrace2 = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 5
    mutualBetHdl <- Trace.activateContractWallet betOwnerWallet mutualBetContract
    _ <- Trace.waitNSlots 5
    threadTkn <- extractAssetClass mutualBetHdl
    bettor1Hdl <- Trace.activateContractWallet bettor1 (bettorContract threadTkn)
    bettor2Hdl <- Trace.activateContractWallet bettor2 (bettorContract threadTkn)
    bettor3Hdl <- Trace.activateContractWallet bettor3 (bettorContract threadTkn)
    _ <- Trace.waitNSlots 1
    let bet1Params = BetParams { nbpAmount = trace2Bettor1Bet, nbpWinnerId = team1Id}
    Trace.callEndpoint @"bet" bettor1Hdl bet1Params
    _ <- Trace.waitNSlots 10
    let bet2Params = BetParams { nbpAmount = trace2Bettor2Bet, nbpWinnerId = team2Id}
    Trace.callEndpoint @"bet" bettor2Hdl bet2Params
    _ <- Trace.waitNSlots 10
    let bet3Params = BetParams { nbpAmount = trace2Bettor3Bet, nbpWinnerId = team1Id}
    Trace.callEndpoint @"bet" bettor3Hdl bet3Params
    _ <- Trace.waitNSlots 10
    let updateParamsLive = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = 0, uoGameStatus = LIVE }
    Trace.callEndpoint @"update" oracleHdl updateParamsLive
    void $ Trace.waitNSlots 10
    let updateParamsFt = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = team1Id, uoGameStatus = FT }
    Trace.callEndpoint @"update" oracleHdl updateParamsFt
    void $ Trace.waitNSlots 5


mutualBetSuccessTrace2FinalState :: MutualBetOutput
mutualBetSuccessTrace2FinalState =
    MutualBetOutput
        { mutualBetState = Last $ Just $ Finished $
            [
                Bet{ betAmount = Ada.lovelaceOf trace2Bettor3Bet
                , betBettor = walletPubKeyHash bettor3
                , betTeamId = team1Id
                , betWinShare = trace2Bettor3WinShare
                },
                Bet{ betAmount = Ada.lovelaceOf trace2Bettor2Bet
                , betBettor = walletPubKeyHash bettor2
                , betTeamId = team2Id
                , betWinShare = trace2Bettor2WinShare
                },
                Bet{ betAmount = Ada.lovelaceOf trace2Bettor1Bet
                , betBettor = walletPubKeyHash bettor1
                , betTeamId = team1Id
                , betWinShare = trace2Bettor1WinShare
                }
            ]
        , mutualBetThreadToken = Last $ Just threadToken
        }

returnBetsIfAllLostTrace :: Trace.EmulatorTrace ()
returnBetsIfAllLostTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 5
    mutualBetHdl <- Trace.activateContractWallet betOwnerWallet mutualBetContract
    _ <- Trace.waitNSlots 5
    threadTkn <- extractAssetClass mutualBetHdl
    Extras.logInfo $ "Trace thread token " ++ show threadTkn
    bettor1Hdl <- Trace.activateContractWallet bettor1 (bettorContract threadTkn)
    bettor2Hdl <- Trace.activateContractWallet bettor2 (bettorContract threadTkn)
    _ <- Trace.waitNSlots 1
    let bet1Params = BetParams { nbpAmount = trace1Bettor1Bet, nbpWinnerId = team1Id}
    Trace.callEndpoint @"bet" bettor1Hdl bet1Params
    _ <- Trace.waitNSlots 2
    let bet2Params = BetParams { nbpAmount = trace1Bettor2Bet, nbpWinnerId = team1Id}
    Trace.callEndpoint @"bet" bettor2Hdl bet2Params
    let updateParams = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = 0, uoGameStatus = LIVE }
    Trace.callEndpoint @"update" oracleHdl updateParams
    void $ Trace.waitNSlots 5
    let updateParamsLive = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = team2Id, uoGameStatus = FT }
    Trace.callEndpoint @"update" oracleHdl updateParamsLive
    void $ Trace.waitNSlots 5

inProgressBetFailTrace :: Trace.EmulatorTrace ()
inProgressBetFailTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 5
    mutualBetHdl <- Trace.activateContractWallet betOwnerWallet mutualBetContract
    _ <- Trace.waitNSlots 5
    threadTkn <- extractAssetClass mutualBetHdl
    Extras.logInfo $ "Trace thread token " ++ show threadTkn
    bettor1Hdl <- Trace.activateContractWallet bettor1 (bettorContract threadTkn)

    let updateParams = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = team1Id, uoGameStatus = LIVE }
    Trace.callEndpoint @"update" oracleHdl updateParams
    _ <- Trace.waitNSlots 5
    Extras.logInfo @String "Make bet"
    let bet1Params = BetParams { nbpAmount = trace1Bettor1Bet, nbpWinnerId = team1Id}
    Trace.callEndpoint @"bet" bettor1Hdl bet1Params
    void $ Trace.waitNSlots 1

incorrectGameBetTrace :: Trace.EmulatorTrace ()
incorrectGameBetTrace = do
    _ <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 5
    mutualBetHdl <- Trace.activateContractWallet betOwnerWallet mutualBetContract
    _ <- Trace.waitNSlots 5
    threadTkn <- extractAssetClass mutualBetHdl
    Extras.logInfo $ "Trace thread token " ++ show threadTkn
    bettor1Hdl <- Trace.activateContractWallet bettor1 (bettorContract threadTkn)
    _ <- Trace.waitNSlots 1
    let bet1Params = BetParams { nbpAmount = trace1Bettor1Bet, nbpWinnerId = -1}
    Trace.callEndpoint @"bet" bettor1Hdl bet1Params
    void $ Trace.waitNSlots 2

incorrectBetAmountTrace :: Trace.EmulatorTrace ()
incorrectBetAmountTrace = do
    _ <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 5
    mutualBetHdl <- Trace.activateContractWallet betOwnerWallet mutualBetContract
    _ <- Trace.waitNSlots 5
    threadTkn <- extractAssetClass mutualBetHdl
    Extras.logInfo $ "Trace thread token " ++ show threadTkn
    bettor1Hdl <- Trace.activateContractWallet bettor1 (bettorContract threadTkn)
    _ <- Trace.waitNSlots 1
    let bet1Params = BetParams { nbpAmount = (Ada.getLovelace $ mbpMinBet mutualBetParams) - 1, nbpWinnerId = team1Id}
    Trace.callEndpoint @"bet" bettor1Hdl bet1Params
    void $ Trace.waitNSlots 2

cancelGameTrace :: Trace.EmulatorTrace ()
cancelGameTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 5
    mutualBetHdl <- Trace.activateContractWallet betOwnerWallet mutualBetContract
    _ <- Trace.waitNSlots 5
    threadTkn <- extractAssetClass mutualBetHdl
    Extras.logInfo $ "Trace thread token " ++ show threadTkn
    bettor1Hdl <- Trace.activateContractWallet bettor1 (bettorContract threadTkn)
    bettor2Hdl <- Trace.activateContractWallet bettor2 (bettorContract threadTkn)
    _ <- Trace.waitNSlots 1
    let bet1Params = BetParams { nbpAmount = trace1Bettor1Bet, nbpWinnerId = team1Id}
    Trace.callEndpoint @"bet" bettor1Hdl bet1Params
    _ <- Trace.waitNSlots 2
    let bet2Params = BetParams { nbpAmount = trace1Bettor2Bet, nbpWinnerId = team2Id}
    Trace.callEndpoint @"bet" bettor2Hdl bet2Params
    let updateParams = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = 0, uoGameStatus = CANC }
    Trace.callEndpoint @"update" oracleHdl updateParams
    void $ Trace.waitNSlots 5

cancelGameTraceState :: MutualBetOutput
cancelGameTraceState =
    MutualBetOutput
        { mutualBetState = Last $ Just $ Finished $
            [
                Bet{ betAmount = Ada.lovelaceOf trace1Bettor2Bet
                , betBettor = walletPubKeyHash bettor2
                , betTeamId = team2Id
                , betWinShare = 0
                },
                Bet{ betAmount = Ada.lovelaceOf trace1Bettor1Bet
                , betBettor = walletPubKeyHash bettor1
                , betTeamId = team1Id
                , betWinShare = 0
                }
            ]
        , mutualBetThreadToken = Last $ Just threadToken
        }

cancelBetTrace :: Trace.EmulatorTrace ()
cancelBetTrace = do
    _ <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 5
    mutualBetHdl <- Trace.activateContractWallet betOwnerWallet mutualBetContract
    _ <- Trace.waitNSlots 5
    threadTkn <- extractAssetClass mutualBetHdl
    Extras.logInfo $ "Trace thread token " ++ show threadTkn
    bettor1Hdl <- Trace.activateContractWallet bettor1 (bettorContract threadTkn)
    bettor2Hdl <- Trace.activateContractWallet bettor2 (bettorContract threadTkn)
    _ <- Trace.waitNSlots 1
    let bet1Params = BetParams { nbpAmount = cancelBettorBet, nbpWinnerId = team1Id}
    Trace.callEndpoint @"bet" bettor1Hdl bet1Params
    _ <- Trace.waitNSlots 5
    let bet2Params = BetParams { nbpAmount = cancelBettorBet, nbpWinnerId = team2Id}
    Trace.callEndpoint @"bet" bettor2Hdl bet2Params
    _ <- Trace.waitNSlots 5
    let cancelBet1Params = BetParams { nbpAmount = cancelBettorBet, nbpWinnerId = team1Id}
    Trace.callEndpoint @"cancelBet" bettor1Hdl cancelBet1Params
    void $ Trace.waitNSlots 5

cancelBetWhenDuplicateTrace :: Trace.EmulatorTrace ()
cancelBetWhenDuplicateTrace = do
    _ <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 5
    mutualBetHdl <- Trace.activateContractWallet betOwnerWallet mutualBetContract
    _ <- Trace.waitNSlots 5
    threadTkn <- extractAssetClass mutualBetHdl
    Extras.logInfo $ "Trace thread token " ++ show threadTkn
    bettor1Hdl <- Trace.activateContractWallet bettor1 (bettorContract threadTkn)
    bettor2Hdl <- Trace.activateContractWallet bettor2 (bettorContract threadTkn)
    _ <- Trace.waitNSlots 1
    let bet1Params = BetParams { nbpAmount = cancelBettorBet, nbpWinnerId = team1Id}
    Trace.callEndpoint @"bet" bettor1Hdl bet1Params
    _ <- Trace.waitNSlots 5
    Trace.callEndpoint @"bet" bettor1Hdl bet1Params
    _ <- Trace.waitNSlots 5
    let bet2Params = BetParams { nbpAmount = cancelBettorBet, nbpWinnerId = team2Id}
    Trace.callEndpoint @"bet" bettor2Hdl bet2Params
    _ <- Trace.waitNSlots 5
    let cancelBet1Params = BetParams { nbpAmount = cancelBettorBet, nbpWinnerId = team1Id}
    Trace.callEndpoint @"cancelBet" bettor1Hdl cancelBet1Params
    void $ Trace.waitNSlots 5

cancelBetLiveGameFailTrace :: Trace.EmulatorTrace ()
cancelBetLiveGameFailTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 5
    mutualBetHdl <- Trace.activateContractWallet betOwnerWallet mutualBetContract
    _ <- Trace.waitNSlots 5
    threadTkn <- extractAssetClass mutualBetHdl
    Extras.logInfo $ "Trace thread token " ++ show threadTkn
    bettor1Hdl <- Trace.activateContractWallet bettor1 (bettorContract threadTkn)
    bettor2Hdl <- Trace.activateContractWallet bettor2 (bettorContract threadTkn)
    _ <- Trace.waitNSlots 1
    let bet1Params = BetParams { nbpAmount = cancelBettorBet, nbpWinnerId = team1Id}
    Trace.callEndpoint @"bet" bettor1Hdl bet1Params
    _ <- Trace.waitNSlots 5
    let bet2Params = BetParams { nbpAmount = cancelBettorBet, nbpWinnerId = team2Id}
    Trace.callEndpoint @"bet" bettor2Hdl bet2Params
    let updateParams = UpdateOracleParams{ uoGameId = gameId, uoWinnerId = 0, uoGameStatus = LIVE }
    Trace.callEndpoint @"update" oracleHdl updateParams
    _ <- Trace.waitNSlots 5
    let cancelBet1Params = BetParams { nbpAmount = cancelBettorBet, nbpWinnerId = team1Id}
    Trace.callEndpoint @"cancelBet" bettor1Hdl cancelBet1Params
    void $ Trace.waitNSlots 5

extractAssetClass :: Trace.ContractHandle MutualBetOutput MutualBetStartSchema MutualBetError -> Trace.EmulatorTrace ThreadToken
extractAssetClass handle = do
    t <- mutualBetThreadToken <$> Trace.observableState handle
    case t of
        Last (Just currency) -> pure currency
        _                    -> Trace.throwError (Trace.GenericError "currency not found")

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
            Trace.waitNSlots 2

expectContractLog :: ToJSON a => Text -> Maybe a -> Bool
expectContractLog expectedText logM = case logM of
                    Nothing -> False
                    Just message -> do
                        let text = toStrict . toLazyText . encodeToTextBuilder $ message
                        isInfixOf expectedText text
expectStateChangeFailureLog :: [EmulatorTimeEvent ContractInstanceLog] -> Bool
expectStateChangeFailureLog = expectContractLog "TransitionFailed" . listToMaybe . reverse . mapMaybe (preview (eteEvent . cilMessage . _ContractLog))

oracleTokenMinAda:: Ada
oracleTokenMinAda = minAdaTxOut

tests :: TestTree
tests =
    testGroup "mutual bet state machine"
        [
        checkPredicateOptions options "success games 1 winner 1 lost"
        (assertDone mutualBetContract (Trace.walletInstanceTag betOwnerWallet) (const True) "mutual bet contract should be done"
          .&&. assertDone (bettorContract threadToken) (Trace.walletInstanceTag bettor1) (const True) "bettor 1 contract should be done"
          .&&. assertDone (bettorContract threadToken) (Trace.walletInstanceTag bettor2) (const True) "bettor 2 contract should be done"
          .&&. assertAccumState (bettorContract threadToken) (Trace.walletInstanceTag bettor1) ((==) mutualBetSuccessTraceFinalState ) "final state should be OK"
          .&&. walletFundsChange bettor1 (Ada.toValue $ Ada.lovelaceOf trace1Bettor2Bet - (mbpBetFee mutualBetParams))
          .&&. walletFundsChange bettor2 (inv (Ada.toValue $ Ada.lovelaceOf trace1Bettor2Bet + (mbpBetFee mutualBetParams)))
          .&&. walletFundsChange betOwnerWallet ((Ada.toValue $ (2 * mbpBetFee mutualBetParams) - opFees oracleParams))
        )
        mutualBetSuccessTrace
        ,
        checkPredicateOptions options "success games 2 winner 1 lost"
        (assertDone mutualBetContract (Trace.walletInstanceTag betOwnerWallet) (const True) "mutual bet contract should be done"
        .&&. assertDone (bettorContract threadToken) (Trace.walletInstanceTag bettor1) (const True) "bettor 1 contract should be done"
        .&&. assertDone (bettorContract threadToken) (Trace.walletInstanceTag bettor2) (const True) "bettor 2 contract should be done"
        .&&. assertDone (bettorContract threadToken) (Trace.walletInstanceTag bettor3) (const True) "bettor 3 contract should be done"
        .&&. assertAccumState (bettorContract threadToken) (Trace.walletInstanceTag bettor1) ((==) mutualBetSuccessTrace2FinalState) "final state should be OK"
        .&&. walletFundsChange bettor1 (Ada.toValue $ trace2Bettor1WinShare - (mbpBetFee mutualBetParams))
        .&&. walletFundsChange bettor2 (inv (Ada.toValue $ Ada.lovelaceOf trace2Bettor2Bet + (mbpBetFee mutualBetParams)))
        .&&. walletFundsChange bettor3 (Ada.toValue $ trace2Bettor3WinShare - (mbpBetFee mutualBetParams))
        .&&. walletFundsChange betOwnerWallet ((Ada.toValue $ (3 * mbpBetFee mutualBetParams) - opFees oracleParams))
        )
        mutualBetSuccessTrace2
        ,
        checkPredicateOptions options "return bets if everyone lost"
        (assertDone mutualBetContract (Trace.walletInstanceTag betOwnerWallet) (const True) "mutual bet contract should be done"
        .&&. assertDone (bettorContract threadToken) (Trace.walletInstanceTag bettor1) (const True) "bettor 1 contract should be done"
        .&&. assertDone (bettorContract threadToken) (Trace.walletInstanceTag bettor2) (const True) "bettor 2 contract should be done"
        .&&. walletFundsChange bettor1 (inv (Ada.toValue $ mbpBetFee mutualBetParams))
        .&&. walletFundsChange bettor2 (inv (Ada.toValue $ mbpBetFee mutualBetParams))
        .&&. walletFundsChange betOwnerWallet ((Ada.toValue $ (2 * mbpBetFee mutualBetParams) - opFees oracleParams))
        )
        returnBetsIfAllLostTrace
        ,
        checkPredicateOptions options "cancel trace"
        (assertDone mutualBetContract (Trace.walletInstanceTag betOwnerWallet) (const True) "mutual bet contract should be done"
        .&&. assertDone (bettorContract threadToken) (Trace.walletInstanceTag bettor1) (const True) "bettor 1 contract should be done"
        .&&. assertDone (bettorContract threadToken) (Trace.walletInstanceTag bettor2) (const True) "bettor 2 contract should be done"
        .&&. assertAccumState (bettorContract threadToken) (Trace.walletInstanceTag bettor1) ((==) cancelGameTraceState ) "final state should be OK"
        .&&. walletFundsChange bettor1 (inv (Ada.toValue $ mbpBetFee mutualBetParams))
        .&&. walletFundsChange bettor2 (inv (Ada.toValue $ mbpBetFee mutualBetParams))
        .&&. walletFundsChange betOwnerWallet ((Ada.toValue $ (2 * mbpBetFee mutualBetParams) - opFees oracleParams))
        )
        cancelGameTrace
        ,
        checkPredicateOptions options "in progress game bet should fail"
        (assertNotDone mutualBetContract (Trace.walletInstanceTag betOwnerWallet) "mutual bet contract should not be done"
        .&&. assertNotDone (bettorContract threadToken) (Trace.walletInstanceTag bettor1) "bettor 1 contract should not be done"
        .&&. assertInstanceLog (Trace.walletInstanceTag $ bettor1) expectStateChangeFailureLog
        .&&. walletFundsChange betOwnerWallet (inv (Ada.toValue $ opFees oracleParams + oracleTokenMinAda + minAdaTxOut))
        )
        inProgressBetFailTrace
        ,
        checkPredicateOptions options "fail bet if game do not exists"
        (
        assertInstanceLog (Trace.walletInstanceTag $ bettor1) expectStateChangeFailureLog
        .&&. walletFundsChange bettor1 (Ada.toValue . Ada.lovelaceOf $ 0)
        )
        incorrectGameBetTrace
        ,
        checkPredicateOptions options "fail bet if amount less than min fee"
        (
        assertInstanceLog (Trace.walletInstanceTag $ bettor1) expectStateChangeFailureLog
        .&&. walletFundsChange bettor1 (Ada.toValue . Ada.lovelaceOf $ 0)
        )
        incorrectBetAmountTrace
        ,
        checkPredicateOptions options "cancel bet"
        (
        walletFundsChange bettor1 (inv (Ada.toValue $ mbpBetFee mutualBetParams))
        .&&. walletFundsChange bettor2 (inv (Ada.toValue $ (Ada.lovelaceOf cancelBettorBet) + (mbpBetFee mutualBetParams)))
        .&&. walletFundsChange betOwnerWallet ((Ada.toValue $ (2 * mbpBetFee mutualBetParams) - opFees oracleParams - oracleTokenMinAda - minAdaTxOut))
        )
        cancelBetTrace
        ,
        checkPredicateOptions options "should cancel only one bet if bettor made duplicate bets"
        (
        walletFundsChange bettor1 (inv (Ada.toValue $ (2 * mbpBetFee mutualBetParams) + (Ada.lovelaceOf cancelBettorBet) ))
        .&&. walletFundsChange bettor2 (inv (Ada.toValue $ (Ada.lovelaceOf cancelBettorBet) + (mbpBetFee mutualBetParams)))
        .&&. walletFundsChange betOwnerWallet ((Ada.toValue $ (3 * mbpBetFee mutualBetParams) - opFees oracleParams - oracleTokenMinAda - minAdaTxOut))
        )
        cancelBetWhenDuplicateTrace
        ,
        checkPredicateOptions options "could not cancel bet for Live game"
        (
        walletFundsChange bettor2 (inv (Ada.toValue $ (Ada.lovelaceOf cancelBettorBet) + (mbpBetFee mutualBetParams)))
        .&&. walletFundsChange bettor2 (inv (Ada.toValue $ (Ada.lovelaceOf cancelBettorBet) + (mbpBetFee mutualBetParams)))
        .&&. walletFundsChange betOwnerWallet ((Ada.toValue $ (2 * mbpBetFee mutualBetParams) - opFees oracleParams - oracleTokenMinAda - minAdaTxOut))
        .&&. assertInstanceLog (Trace.walletInstanceTag $ bettor1) expectStateChangeFailureLog
        )
        cancelBetLiveGameFailTrace
        ]
