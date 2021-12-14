{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module Contracts.MutualBet.OnChain
    where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import Contracts.MutualBet.Types
import Contracts.Oracle
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bool (bool)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger hiding (MintingPolicyHash, singleton)
import Ledger.Ada as Ada
import Ledger.Constraints as Constraints
import Ledger.Constraints.OnChain as Constraints
import Ledger.Constraints.TxConstraints as Constraints
import Ledger.Contexts qualified as Validation
import Ledger.Scripts qualified as LedgerScripts
import Ledger.Tx qualified as LedgerScripts
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value as Value
import Plutus.Contract as Contract
import Plutus.Contract.Oracle (SignedMessage (..), verifySignedMessageConstraints)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Semigroup (..), Show (..), String)
import Prelude qualified as Haskell
import Schema (ToSchema)
import Types.Game

{-# INLINABLE payToWinners #-}
payToWinners :: TxInfo -> (PubKeyHash, Ada, Ada) -> Bool
payToWinners txInfo (pkh, betAmount, winShare)= valuePaidTo txInfo pkh == Ada.toValue (betAmount + winShare)

{-# INLINABLE payBettorsBack #-}
payBettorsBack :: TxInfo -> Bet -> Bool
payBettorsBack txInfo bet = valuePaidTo txInfo (betBettor bet) == Ada.toValue (betAmount bet)

{-# INLINABLE betsValueAmount #-}
-- | The combined value of bets.
betsValueAmount :: [Bet] -> Ada
betsValueAmount = fold . map (\bet -> betAmount bet)

{-# INLINABLE winBets #-}
-- | Get winning bets.
winBets :: Integer -> [Bet] -> [Bet]
winBets winnerTeamId bets = filter (\bet -> betTeamId bet == winnerTeamId) bets

{-# INLINABLE calculatePrize #-}
calculatePrize:: Bet -> Ada -> Ada -> Ada
calculatePrize bet totalBets totalWin =
    let
        totalPrize = totalBets - totalWin
        amount = betAmount bet
    in
        bool ((Ada.divide (amount * totalPrize ) totalWin)) 0 (totalWin == 0)

{-# INLINABLE calculateWinnerShare #-}
calculateWinnerShare :: Bet -> Ada -> Ada -> Ada
calculateWinnerShare bet totalBets totalWin = calculatePrize bet totalBets totalWin

{-# INLINABLE getWinners #-}
getWinners :: Integer -> [Bet] -> [(PubKeyHash, Ada, Ada)]
getWinners winnerTeamId bets =
    let
        winnerBets = winBets winnerTeamId bets
        total = betsValueAmount bets
        totalWin = betsValueAmount $ winnerBets
    in
        map (\winBet -> (betBettor winBet, betAmount winBet, calculateWinnerShare winBet total totalWin)) winnerBets

{-# INLINABLE mapWinshare #-}
mapWinshare  :: Integer -> [Bet] -> [Bet]
mapWinshare winnerTeamId bets =
    let
        winnerBets = winBets winnerTeamId bets
        total = betsValueAmount bets
        totalWin = betsValueAmount $ winnerBets
    in
    map (\bet -> bet{
        betWinShare = if betTeamId bet == winnerTeamId
            then calculateWinnerShare bet total totalWin
            else Ada.lovelaceOf 0
        }) bets

{-# INLINABLE isValidBet #-}
isValidBet ::  MutualBetParams -> Bet -> Bool
isValidBet MutualBetParams{mbpTeam1, mbpTeam2, mbpMinBet} Bet{betAmount, betTeamId, betWinShare}
    | betAmount < mbpMinBet = False
    | mbpTeam1 /= betTeamId && mbpTeam2 /= betTeamId = False
    | betWinShare /= 0 = False
    | otherwise = True

{-# INLINABLE deleteFirstOccurence #-}
deleteFirstOccurence :: Bet -> [Bet] -> [Bet]
deleteFirstOccurence bet (x:xs)
    | (bet==x) = xs
    | otherwise = x : deleteFirstOccurence bet xs

{-# INLINABLE validateBet #-}
validateBet ::
    MutualBetParams
    -> [Bet]
    -> Bet
    -> ScriptContext
    -> Bool
validateBet params bets bet ctx =
    traceIfFalse "bet is valid" (isValidBet params bet)
    && traceIfFalse "expected fee payed" (valuePaidTo info ( mbpOwner params) == Ada.toValue (mbpBetFee params))
    && traceIfFalse "bet and token locked"
        (Constraints.checkOwnOutputConstraint ctx
            (OutputConstraint (MutualBetDatum (bet:bets) BettingOpen) $
                (Ada.toValue $ betsValueAmount newBets) <>
                (Ada.toValue Ledger.minAdaTxOut) <>
                assetClassValue (mbpMutualBetId params) 1
            )
        )
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    newBets :: [Bet]
    newBets = bet : bets

{-# INLINABLE validateCancelBet #-}
validateCancelBet ::
    MutualBetParams
    -> [Bet]
    -> Bet
    -> ScriptContext
    -> Bool
validateCancelBet params bets cancelBet ctx =
    traceIfFalse "expected bet payed back" (valuePaidTo info (betBettor cancelBet) `Value.geq` Ada.toValue (betAmount cancelBet))
    && traceIfFalse "bet unclocked from contract"
        (Constraints.checkOwnOutputConstraint ctx
            (OutputConstraint (MutualBetDatum (betsWithoutCancelled) BettingOpen) $
                (Ada.toValue $ betsValueAmount betsWithoutCancelled ) <>
                (Ada.toValue Ledger.minAdaTxOut) <>
                assetClassValue (mbpMutualBetId params) 1
            )
        )
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    betsWithoutCancelled:: [Bet]
    betsWithoutCancelled = (deleteFirstOccurence cancelBet bets)

{-# INLINABLE validateStartGame #-}
validateStartGame ::
    MutualBetParams
    -> [Bet]
    -> SignedMessage OracleSignedMessage
    -> ScriptContext
    -> Bool
validateStartGame params bets signedMessage ctx =
    traceIfFalse "signed by owner" (txSignedBy info $ mbpOwner params )
    && isCurrentGameCheck params oracleMessage
    && expectGameStatus LIVE oracleMessage
    && traceIfFalse "status should be betting closed"
        (Constraints.checkOwnOutputConstraint ctx
            (OutputConstraint (MutualBetDatum bets BettingClosed) $
                (Ada.toValue $ betsValueAmount bets) <>
                (Ada.toValue Ledger.minAdaTxOut) <>
                assetClassValue (mbpMutualBetId params) 1
            )
        )
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oracleMessage:: OracleSignedMessage
    oracleMessage = extractOracleMessage params signedMessage

{-# INLINABLE validatePayout #-}
validatePayout ::
    MutualBetParams
    -> [Bet]
    -> SignedMessage OracleSignedMessage
    -> ScriptContext
    -> Bool
validatePayout params bets signedMessage ctx =
    traceIfFalse "signed by owner" (txSignedBy info $ mbpOwner params )
    && isCurrentGameCheck params oracleMessage
    && expectGameStatus FT oracleMessage
    &&  if hasWinner
        then
          traceIfFalse "payout to winners" (all (payToWinners info) winners)
        else
          traceIfFalse "pay back on draw"  (all (payBettorsBack info) bets)
    && traceIfFalse "status in datum cancelled"
        (Constraints.checkOwnOutputConstraint ctx
            (OutputConstraint (MutualBetDatum bets GameFinished) $
                (Ada.toValue Ledger.minAdaTxOut) <>
                assetClassValue (mbpMutualBetId params) 1
            )
        )
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oracleMessage:: OracleSignedMessage
    oracleMessage = extractOracleMessage params signedMessage

    winnerId :: TeamId
    winnerId = osmWinnerId oracleMessage

    hasWinner:: Bool
    hasWinner = winnerId > 0

    winners :: [(PubKeyHash, Ada, Ada)]
    winners = getWinners winnerId bets

{-# INLINABLE validateCancel #-}
validateCancel ::
    MutualBetParams
    -> [Bet]
    -> SignedMessage OracleSignedMessage
    -> ScriptContext
    -> Bool
validateCancel params bets signedMessage ctx =
    traceIfFalse "signed by owner" (txSignedBy info $ mbpOwner params )
    && traceIfFalse "pay back on cancel"  (all (payBettorsBack info) bets)
    && isCurrentGameCheck params oracleMessage
    && expectGameStatus CANC oracleMessage
    && traceIfFalse "status in datum cancelled"
        (Constraints.checkOwnOutputConstraint ctx
            (OutputConstraint (MutualBetDatum bets GameCancelled) $
                (Ada.toValue Ledger.minAdaTxOut) <>
                assetClassValue (mbpMutualBetId params) 1
            )
        )
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oracleMessage:: OracleSignedMessage
    oracleMessage = extractOracleMessage params signedMessage

{-# INLINABLE validateDeleteGame #-}
validateDeleteGame ::
    MutualBetParams
    -> SignedMessage OracleSignedMessage
    -> ScriptContext
    -> Bool
validateDeleteGame params signedMessage ctx =
    traceIfFalse "signed by owner" (txSignedBy info $ mbpOwner params )
    && (expectGameStatus FT oracleMessage || expectGameStatus CANC oracleMessage)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    oracleMessage:: OracleSignedMessage
    oracleMessage = extractOracleMessage params signedMessage

{-# INLINABLE isCurrentGameCheck #-}
isCurrentGameCheck:: MutualBetParams -> OracleSignedMessage -> Bool
isCurrentGameCheck params message = traceIfFalse "signed message not for this game" (mbpGame params == osmGameId message)

{-# INLINABLE extractOracleMessage #-}
extractOracleMessage:: MutualBetParams -> SignedMessage OracleSignedMessage -> OracleSignedMessage
extractOracleMessage params message = fromMaybe (traceError "no oracle message") (extractSignedMessage (oraclePubKey params) $ Just message)

{-# INLINABLE expectGameStatus #-}
expectGameStatus:: FixtureStatusShort -> OracleSignedMessage  -> Bool
expectGameStatus expStatus message = expStatus == (osmGameStatus message)

{-# INLINABLE oraclePubKey #-}
oraclePubKey:: MutualBetParams -> PubKey
oraclePubKey params = (oOperatorKey $ mbpOracle $ params)

{-# INLINABLE mkMutualBetValidator #-}
mkMutualBetValidator :: MutualBetParams -> MutualBetDatum -> MutualBetRedeemer -> ScriptContext -> Bool
mkMutualBetValidator params (MutualBetDatum bets BettingOpen   ) (MakeBet bet)              ctx = validateBet params bets bet ctx
-- mkMutualBetValidator params (MutualBetDatum _    _             ) (MakeBet _)                _   = traceError "betting closed"
mkMutualBetValidator params (MutualBetDatum bets BettingOpen   ) (CancelBet cancelBet)      ctx = validateCancelBet params bets cancelBet ctx
-- mkMutualBetValidator params (MutualBetDatum _    _             ) (CancelBet _)              _   = traceError "bettings closed"
mkMutualBetValidator params (MutualBetDatum bets BettingOpen   ) (StartGame oracleMessage)  ctx = validateStartGame params bets oracleMessage ctx
-- mkMutualBetValidator params (MutualBetDatum _    _             ) (StartGame _)              _   = traceError "game already started"
mkMutualBetValidator params (MutualBetDatum bets BettingClosed ) (Payout oracleMessage)     ctx = validatePayout params bets oracleMessage ctx
-- mkMutualBetValidator params (MutualBetDatum _    _             ) (Payout _)                 _   = traceError "game cannot be finished"
mkMutualBetValidator params (MutualBetDatum bets BettingOpen   ) (CancelGame oracleMessage) ctx = validateCancel params bets oracleMessage ctx
-- mkMutualBetValidator params (MutualBetDatum _    _             ) (CancelGame _            ) _   = traceError "game cannot be cancelled"
mkMutualBetValidator params (MutualBetDatum bets GameCancelled ) (DeleteGame oracleMessage) ctx = validateDeleteGame params oracleMessage ctx
mkMutualBetValidator params (MutualBetDatum bets GameFinished  ) (DeleteGame oracleMessage) ctx = validateDeleteGame params oracleMessage ctx
-- mkMutualBetValidator params (MutualBetDatum _    _             ) (DeleteGame _)             _   = traceError "cannot delete game"
mkMutualBetValidator _       _                                   _                          _   = traceError "invalid bet state"
data MutualBetTypes
instance Scripts.ValidatorTypes MutualBetTypes where
    type instance DatumType MutualBetTypes = MutualBetDatum
    type instance RedeemerType MutualBetTypes = MutualBetRedeemer

typedMutualBetValidator :: MutualBetParams -> Scripts.TypedValidator MutualBetTypes
typedMutualBetValidator mbParams = Scripts.mkTypedValidator @MutualBetTypes
    ($$(PlutusTx.compile [|| mkMutualBetValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode mbParams)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @MutualBetDatum @MutualBetRedeemer

mutualBetValidator :: MutualBetParams -> Validator
mutualBetValidator = Scripts.validatorScript . typedMutualBetValidator

mutualBetValidatorHash :: MutualBetParams -> Ledger.ValidatorHash
mutualBetValidatorHash mutualBetParams = LedgerScripts.validatorHash . mutualBetValidator $ mutualBetParams

mutualBetAddress :: MutualBetParams -> Ledger.Address
mutualBetAddress = scriptAddress . mutualBetValidator

mutualBetScriptAsShortBs :: MutualBetParams -> SBS.ShortByteString
mutualBetScriptAsShortBs = SBS.toShort . LBS.toStrict . serialise . mutualBetValidator

mutualBetPlutusScript :: MutualBetParams -> PlutusScript PlutusScriptV1
mutualBetPlutusScript = PlutusScriptSerialised . mutualBetScriptAsShortBs
