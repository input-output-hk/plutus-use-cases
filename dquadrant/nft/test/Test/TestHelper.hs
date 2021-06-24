{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
module Test.TestHelper

where
import Ledger.Ada(adaSymbol,adaToken)
import Ledger.Value
import PlutusTx.Builtins (ByteString, emptyByteString)
import Ledger (TxId, txId, TxOutTx (txOutTxTx), pubKeyHash)
import Plutus.Trace
    ( EmulatorTrace,
      ContractHandle,
      activateContractWallet,
      callEndpoint )
import Data.Text (Text)
import Data.Aeson (Result (Error, Success), ToJSON (toJSON), fromJSON)
import Plutus.Trace.Emulator.ContractInstance (ContractInstanceState(ContractInstanceState))
import Plutus.Contract.Types
import qualified Control.Monad.Freer.Extras as Extras
import qualified Data.Aeson.Types as AesonTypes
import Plutus.Contract.Wallet.MarketPlace
import Plutus.Contract.Wallet.MarketEndpoints
import Playground.Contract
import Plutus.Contract (HasEndpoint, logInfo, tell)
import qualified Data.Map as Map
import Plutus.Contract.Wallet.EndpointModels
import Data.Functor ((<&>), void)
import Wallet.Emulator (walletPubKey)
import Plutus.Trace.Emulator (waitNSlots, EmulatorRuntimeError (GenericError), getContractState)
import qualified Plutus.Trace.Emulator as EmulatorTrace

type TestSchema=
  MarketSchema
  .\/ Endpoint "filterTxOuts" TxId

filterTxOutsEp :: (AsContractError e,HasEndpoint "filterTxOuts" TxId s) => Contract [AesonTypes.Value] s e ()
filterTxOutsEp= do
  logInfo   @String "CALL AMp TO filter"
  x <- endpoint @"filterTxOuts"
  logInfo  @String $  "Got Tx to resolve "++ show x
  vs<-marketTxOutsByTxId defaultMarket x
  tell [toJSON vs]
  logInfo $ "mapped " ++ show x ++ " to " ++ show vs
  pure ()
  where
    marketTxOutsByTxId :: AsContractError e => Market ->TxId -> Contract w s e [TxOutRef]
    marketTxOutsByTxId market txIdp =
      utxoAt (marketAddress market) <&>  Map.keys .Map.filterWithKey included
      where
        included _ txOutTx =txId (txOutTxTx txOutTx) == txIdp


testEndpoints :: Contract [AesonTypes.Value] TestSchema Text  ()
testEndpoints= (marketEndpoints defaultMarket `select`filterTxOutsEp) >>testEndpoints

defaultMarket :: Market
defaultMarket = Market
    {
    mOperator   = pubKeyHash (walletPubKey (Wallet 9)),
    mAuctionFee =1_000_000 , -- for consistency keep it 1%
    mPrimarySaleFee =2_000_000,-- for consistency keep it 2%
    mSecondarySaleFee=3_000_000 -- for consistency keep it 3%
    }

sellParamLovelace :: Value -> SellType -> Integer -> SellParams
sellParamLovelace _values sType lovelace=SellParams{
                spItems=toValueInfo _values,
                spSaleType = sType ,
                spCost=ValueInfo{
                        currency= unCurrencySymbol adaSymbol,
                        token=  unTokenName adaToken,
                        value = lovelace
                }
        }

nftAssetId :: ByteString -> AssetId
nftAssetId bs=AssetId bs emptyByteString

nft :: ByteString  -> Value
nft t =singleton (CurrencySymbol t)  (TokenName emptyByteString)  1

negNft::ByteString   -> Value
negNft t=singleton (CurrencySymbol t) (TokenName emptyByteString) (-1)

noNft:: ByteString -> Value
noNft t =singleton (CurrencySymbol t) (TokenName emptyByteString ) 0

operator :: Wallet
operator=Wallet 9


wait :: EmulatorTrace()
wait=void $ waitNSlots 3

getHandle:: Integer ->  EmulatorTrace (ContractHandle [AesonTypes.Value] TestSchema Text)
getHandle i =activateContractWallet (Wallet i) testEndpoints


lastResult :: FromJSON a =>ContractHandle [AesonTypes.Value] TestSchema Text-> EmulatorTrace  a
lastResult h1=do
  (ContractInstanceState (ResumableResult  _ _ _ _  _  _ _ s)  _ _ ) <- getContractState h1
  Extras.logInfo @String $ " resolving txId for some handle" ++ show s
  case fromJSON  ( head $ Prelude.tail s ) of
      Success v ->  return  v
      Error e   -> EmulatorTrace.throwError (GenericError e)


lastUtxos :: ContractHandle [AesonTypes.Value] TestSchema Text-> EmulatorTrace [TxOutRef]
lastUtxos h = do
  txid <-lastResult h
  callEndpoint @"filterTxOuts" h txid
  lastResult h