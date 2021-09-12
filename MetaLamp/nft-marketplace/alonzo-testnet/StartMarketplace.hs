{-# LANGUAGE OverloadedStrings #-}

import           Prelude
import           Cardano.Api 
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus
import Plutus.Contracts.NftMarketplace.OnChain.Core (marketplaceValidator)
import Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine (Marketplace(..))

import           Data.Maybe             as M
import           Data.Aeson             as A
import qualified Data.ByteString.Lazy   as LB
import qualified Data.ByteString.Short  as SBS
import           Codec.Serialise
import PlutusTx.Builtins (encodeUtf8, BuiltinString(..))
import Plutus.V1.Ledger.Crypto (PubKeyHash (..))
import Plutus.Contracts.NftMarketplace.OffChain.Serialization 

main :: IO ()
main = do
  case Plutus.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = toAlonzoData (ScriptDataNumber 42)
              (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m plutusScriptShortBs [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope "marketplace-state-machine-script.plutus" Nothing apiPlutusScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

-- Get Shelley's pubKeyHash in the testnet
-- cardano-cli address key-hash --payment-verification-key-file shelley.vkey 
-- 02962fef5905fb19ed673853ca6b4d67a774a2bb2853239e7a876fee
mockOwnerPubKeyHash :: Marketplace 
mockOwnerPubKeyHash = Marketplace {
    marketplaceOperator = M.fromMaybe (error "Can't decode pabKeyHash") $ A.decode $ 
       "{\"getPubKeyHash\" : \"02962fef5905fb19ed673853ca6b4d67a774a2bb2853239e7a876fee\"}"
      -- PubKeyHash $ deserializeByteString "02962fef5905fb19ed673853ca6b4d67a774a2bb2853239e7a876fee"
    }

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise $ marketplaceValidator mockOwnerPubKeyHash

apiPlutusScript :: PlutusScript PlutusScriptV1
apiPlutusScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

plutusScriptShortBs :: SBS.ShortByteString
plutusScriptShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor
