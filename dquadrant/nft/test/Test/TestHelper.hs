{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module Test.TestHelper
where
import Ledger.Ada(adaSymbol,adaToken, lovelaceValueOf)
import Ledger.Value
import PlutusTx.Builtins (ByteString, emptyByteString, encodeUtf8)
import Ledger (txId, TxOutTx (txOutTxTx), pubKeyHash, Address, pubKeyAddress, TxId)
import Plutus.Trace
    ( EmulatorTrace,
      ContractHandle,
      activateContractWallet, EmulatorConfig )
import Data.Text (Text)
import Data.Aeson (Result (Error, Success), ToJSON (toJSON), fromJSON, encode)
import Plutus.Trace.Emulator.ContractInstance (ContractInstanceState(ContractInstanceState))
import Plutus.Contract.Types
    ( select,
      ResumableResult(ResumableResult) )
import qualified Control.Monad.Freer.Extras as Extras
import qualified Data.Aeson.Types as AesonTypes
import Plutus.Contract.Wallet.MarketPlace
import Plutus.Contract.Wallet.MarketEndpoints
import Playground.Contract
import Plutus.Contract (HasEndpoint, tell, logError, handleError)
import qualified Data.Map as Map
import Plutus.Contract.Wallet.EndpointModels
import Data.Functor (void)
import Wallet.Emulator (walletPubKey)
import Plutus.Trace.Emulator
    ( waitNSlots,
      EmulatorRuntimeError(GenericError),
      getContractState,
      callEndpoint,
      EmulatorConfig(EmulatorConfig) )
import qualified Plutus.Trace.Emulator as EmulatorTrace
import Plutus.Contract.Test (TracePredicate, checkPredicateOptions, defaultCheckOptions, emulatorConfig, valueAtAddress)
import Test.Tasty (TestTree)
import Control.Lens.Operators
import Data.String.Conversions (convertString)


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

filterTxOutsEp :: (AsContractError e,HasEndpoint "filterTxOuts" TxId s) => Contract [AesonTypes.Value] s e ()
filterTxOutsEp= do
  x <- endpoint @"filterTxOuts"
  vs<-marketTxOutsByTxId defaultMarket x
  tell [toJSON vs]
  pure ()
  where
    marketTxOutsByTxId :: AsContractError e => Market ->TxId -> Contract w s e [TxOutRef]
    marketTxOutsByTxId market txIdp =
      utxoAt (marketAddress market) <&>  Map.keys .Map.filterWithKey included
      where
        included _ txOutTx =txId (txOutTxTx txOutTx) == txIdp


testEndpoints :: Contract [AesonTypes.Value] TestSchema Text  ()
testEndpoints= handleError (\e -> logError e) (marketEndpoints defaultMarket `select`filterTxOutsEp) >>testEndpoints

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

nft :: ByteString  -> Value
nft t =cardanoToken t  1

cardanoToken :: ByteString -> Integer -> Value
cardanoToken t = singleton (CurrencySymbol t)  (TokenName emptyByteString)


negNft::ByteString   -> Value
negNft t=cardanoToken t (-1)

noNft:: ByteString -> Value
noNft t =cardanoToken t  0

operator :: Wallet
operator=Wallet 9


wait :: EmulatorTrace()
wait=void $ waitNSlots 4

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
    Error  e  -> do Extras.logError @String $ "AesonError : " ++ show e ++" : "++(convertString $ encode state)
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
configurationWithNfts = EmulatorConfig $ Left $ Map.fromList distribution
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
