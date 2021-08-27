{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Spec.Oracle
    ( tests
    ) where

import           Control.Lens
import           Control.Monad                      (void, when)
import qualified Control.Monad.Freer                as Freer
import qualified Control.Monad.Freer.Error          as Freer
import           Control.Monad.Freer.Extras as Extras
import           Control.Monad.Freer.Extras.Log     (LogLevel (..))
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
import qualified Plutus.Trace.Emulator              as Trace
import           PlutusTx.Monoid                    (inv)

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
oracleParams = OracleParams{ opSymbol = oracleCurrency, opFees = 3_000_000, opGame = 1 } 

oracle ::  Oracle
oracle = Oracle
    { oSymbol = opSymbol oracleParams
    , oOperator = pubKeyHash $ walletPubKey oracleWallet
    , oFee = opFees oracleParams
    , oGame = opGame oracleParams
    }

oracleContract :: Contract (Last Oracle) OracleSchema Text ()
oracleContract = runOracle oracleParams

requestOracleTokenContract :: Oracle -> Contract Text EmptySchema Text ()
requestOracleTokenContract oracle = requestOracleForAddress oracle

signOracleTokenContract :: Oracle -> Contract Text EmptySchema Text ()
signOracleTokenContract oracle = listenOracleRequest oracle (walletPrivKey oracleWallet)

w1, w2, w3 :: Wallet
w1 = Wallet 1
w2 = Wallet 2
w3 = Wallet 3
w4 = Wallet 4
w5 = Wallet 5
oracleWallet = w1
oracleClientWallet = w2

winTeamId:: Integer
winTeamId = 1

requestOracleTestState :: OracleData
requestOracleTestState = OracleData
    { ovGame = oGame oracle
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
    { ovGame = oGame oracle
    , ovWinner = winTeamId
    , ovRequestAddress = pubKeyHash $ walletPubKey oracleClientWallet
    , ovWinnerSigned = Just $ signMessage 1 (walletPrivKey oracleWallet) 
    }

signOracleTrace :: Trace.EmulatorTrace ()
signOracleTrace = do
    oracleHdl <- Trace.activateContractWallet oracleWallet $ oracleContract
    _ <- Trace.waitNSlots 3
    oracleSignHdl <- Trace.activateContractWallet oracleWallet (signOracleTokenContract oracle)
    _ <- Trace.waitNSlots 20

    Extras.logInfo $ "signOracleTrace " ++  (show $ Just $ signMessage (1 :: Integer) (walletPrivKey oracleWallet) )
    Extras.logInfo @String "signOracleTrace "
    requestOracleHdl <- Trace.activateContractWallet oracleClientWallet (requestOracleTokenContract oracle)
    void $ Trace.waitNSlots 3

tests :: TestTree
tests =
    testGroup "oracle"
        [ 
        checkPredicateOptions options "request oracle token"
        (
        assertNoFailedTransactions
        .&&. valueAtAddress (oracleAddress oracle)
            (== ((Ada.toValue . Ada.lovelaceOf $ (oFee oracle)) 
                <> Value.singleton (requestTokenSymbol oracle $ oracleAddress oracle) oracleTokenName 1)
            )
        .&&. walletFundsChange oracleClientWallet (inv (Ada.toValue . Ada.lovelaceOf $ (oFee oracle)))
        .&&. dataAtAddress (oracleAddress oracle) (== requestOracleTestState)
        )
        requestOracleTrace
        ,
        checkPredicateOptions options "Should sign oracle data"
        ( 
            assertNoFailedTransactions
            .&&. valueAtAddress (oracleAddress oracle)
                (== (Value.singleton (requestTokenSymbol oracle $ oracleAddress oracle) oracleTokenName 1))
            .&&. walletFundsChange oracleWallet (Ada.toValue . Ada.lovelaceOf $ (oFee oracle))
            .&&. dataAtAddress (oracleAddress oracle) (== signOracleTestState)
        )
        signOracleTrace
        ]
