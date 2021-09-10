{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Test.TestHelper
where
import Ledger.Ada(adaSymbol,adaToken, lovelaceValueOf)
import Ledger.TimeSlot
import Ledger.Value
    ( CurrencySymbol(CurrencySymbol, unCurrencySymbol),
      TokenName(TokenName, unTokenName),
      Value,
      geq,
      singleton )
import PlutusTx.Builtins ( emptyByteString, BuiltinByteString)
import Ledger hiding(singleton,value)
import Plutus.Trace
    ( EmulatorTrace,
      ContractHandle,
      activateContractWallet, EmulatorConfig (_initialChainState, _slotConfig, _feeConfig) )
import Data.Text (Text)
import           Data.Default                             (Default (def))
import Data.Aeson (Result (Error, Success), ToJSON (toJSON), fromJSON, encode, FromJSON)
import Plutus.Trace.Emulator.ContractInstance (ContractInstanceState(ContractInstanceState))
import Plutus.Contract.Types
    ( ResumableResult(ResumableResult) )
import qualified Control.Monad.Freer.Extras as Extras
import qualified Data.Aeson.Types as AesonTypes
import Plutus.Contract.Wallet.MarketPlace
import Plutus.Contract.Wallet.MarketEndpoints
-- import Playground.Contract
-- import Playground.Contract
-- import Playground.Contract
-- import Playground.Contract
import Plutus.Contract 
import qualified Data.Map as Map
import Plutus.Contract.Wallet.EndpointModels
import Data.Functor (void)
import Plutus.Trace.Emulator
    ( EmulatorRuntimeError(GenericError),
      getContractState,
      callEndpoint,
      EmulatorConfig(EmulatorConfig) )
import qualified Plutus.Trace.Emulator as EmulatorTrace
import Plutus.Contract.Test (TracePredicate, checkPredicateOptions, defaultCheckOptions, emulatorConfig, valueAtAddress)
import Test.Tasty (TestTree)
import Control.Lens.Operators
import Data.String.Conversions (convertString)
import Wallet.Emulator
import qualified Plutus.Contract as Contract


defaultMarket :: Market
defaultMarket = Market {
    mOperator   = pubKeyHash (walletPubKey (Wallet 9)),
    mAuctionFee =1_000_000 , -- for consistency keep it 1%
    mPrimarySaleFee =2_000_000,-- for consistency keep it 2%
    mSecondarySaleFee=3_000_000 -- for consistency keep it 3%
    }

type TestSchema=
  MarketSchema
  .\/ Endpoint "filterTxOuts" TxId

filterTxOutsEp :: (AsContractError e,HasEndpoint "filterTxOuts" TxId s) => Promise  [AesonTypes.Value] s e ()
filterTxOutsEp= 
  endpoint @"filterTxOuts" (\x ->do 
    vs<-marketTxOutsByTxId defaultMarket x
    tell [toJSON vs]
    pure ())
  where
    marketTxOutsByTxId :: AsContractError e => Market ->TxId -> Contract w s e [TxOutRef]
    marketTxOutsByTxId market txIdp =
      utxoAt (marketAddress market) <&>  Map.keys .Map.filterWithKey included
      where
        included _ txOutTx =txId (txOutTxTx txOutTx) == txIdp


testEndpoints :: Contract [AesonTypes.Value] TestSchema Text ()
testEndpoints= forever
  where 
    forever = handleError errorHandler $ awaitPromise endpoints >> forever
    endpoints= marketEndpoints defaultMarket `select` filterTxOutsEp
    errorHandler :: Show a => a -> Contract w s e ()
    errorHandler e = do
        Contract.logError $ show e

defaultMarketAddress :: Address
defaultMarketAddress=marketAddress  defaultMarket

sellParamLovelace :: Value -> SellType -> Integer -> SellParams
sellParamLovelace _values sType lovelace=SellParams{
                spItems=toValueInfo _values,
                spSaleType = sType ,
                spShare=[],
                spTotalCost=ValueInfo{
                        currency= unCurrencySymbol adaSymbol,
                        token=  unTokenName adaToken,
                        value = lovelace
                }
        }

nft :: BuiltinByteString   -> Value
nft t =cardanoToken t  1

cardanoToken :: BuiltinByteString -> Integer -> Value
cardanoToken t = singleton (CurrencySymbol t)  (TokenName emptyByteString)


negNft::BuiltinByteString   -> Value
negNft t=cardanoToken t (-1)

noNft:: BuiltinByteString -> Value
noNft t =cardanoToken t  0

operator :: Wallet
operator=Wallet 9

slotNoToPosixTime:: Integer -> POSIXTime 
slotNoToPosixTime v = slotToBeginPOSIXTime def (Slot v)

wait :: EmulatorTrace()
wait=void $ EmulatorTrace.waitNSlots 3

getHandle:: Integer ->  EmulatorTrace (ContractHandle [AesonTypes.Value] TestSchema Text)
getHandle i =activateContractWallet (Wallet i) testEndpoints


lastResult :: FromJSON a =>ContractHandle [AesonTypes.Value] TestSchema Text-> EmulatorTrace  a
lastResult h=do
  (ContractInstanceState (ResumableResult  _ _ _ _  _  _  _ lastState)  _ _ ) <- getContractState h
  state <-case  lastState  of
    [] -> EmulatorTrace.throwError $ GenericError "Tried to Get last constract scatate but it's empty"
    (v : _) ->  ( Extras.logDebug    @String $ "parseJson : " ++ show v ) >> pure v
    
  case fromJSON state of
    Success p -> pure p
    Error  e  -> do Extras.logError @String $ "The datatype that was tell'ed by contract is different : " ++ show e ++" : "++(convertString $ encode state)
                    EmulatorTrace.throwError (GenericError $ e ++" : " ++ (convertString $ encode state))

assertTrue ::  String ->Bool -> EmulatorTrace  ()
assertTrue b a =
    if a then return () else EmulatorTrace.throwError (GenericError b)

throw:: String -> EmulatorTrace ()
throw s =EmulatorTrace.throwError (GenericError s)


lastUtxos :: ContractHandle [AesonTypes.Value] TestSchema Text-> EmulatorTrace [TxOutRef]
lastUtxos h = do
  _txId <-lastResult h
  callEndpoint  @"filterTxOuts" h _txId
  wait
  lastResult h

waitForLastUtxos :: ContractHandle [AesonTypes.Value] TestSchema Text-> EmulatorTrace [TxOutRef]
waitForLastUtxos h= do
  wait
  lastUtxos h


configurationWithNfts :: EmulatorConfig
configurationWithNfts = EmulatorConfig{
    _initialChainState =Left $ Map.fromList distribution 
  , _slotConfig =def
  , _feeConfig=def  

  }
  where
    distribution= [
                (Wallet 1 ,adaFunds<>nft "aa" <> nft "ab" <> nft "ac"),
                (Wallet 2 ,adaFunds<>nft "ba" <> nft "bb" <> nft "bc"),
                (Wallet 3 ,adaFunds<>nft "ca" <> nft "cb" <> nft "cc"),
                (Wallet 4 ,adaFunds<>nft "da" <> nft "db" <> nft "dc"),
                (Wallet 5 ,adaFunds<>nft "ea" <> nft "eb" <> nft "ec"),
                (Wallet 6 ,adaFunds<>nft "fa" <> nft "fb" <> nft "fc")
        ]
    adaFunds :: Value
    adaFunds = lovelaceValueOf 1_000_000_000

defaultCheck :: String -> TracePredicate -> EmulatorTrace () -> TestTree
defaultCheck =checkPredicateOptions (defaultCheckOptions & emulatorConfig .~ configurationWithNfts)

lockedByMarket :: Value -> TracePredicate
lockedByMarket valueAtleast = valueAtAddress (marketAddress defaultMarket) (\v->v`geq` valueAtleast)

walletHasAtLeast :: Value -> Integer -> TracePredicate
walletHasAtLeast v w=valueAtAddress (pubKeyAddress $ walletPubKey $ Wallet w) (\x->x `geq`v)
