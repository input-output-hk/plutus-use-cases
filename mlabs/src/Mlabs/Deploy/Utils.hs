module Mlabs.Deploy.Utils where

import PlutusTx.Prelude hiding (error)
import Prelude (IO, String, error, print, undefined)

import Data.Aeson as Json
import Data.ByteString as DB
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS

import Cardano.Api
import Cardano.Api.Shelley

import Cardano.Ledger.Alonzo.Data qualified as Alonzo
import Codec.Serialise
import Ledger.Typed.Scripts.Validators as VS
import Plutus.V1.Ledger.Api (MintingPolicy, TxOutRef, Validator)
import Plutus.V1.Ledger.Api qualified as Plutus
import PlutusTx

validatorToPlutus file validator = do
  -- taken from here
  -- https://github.com/input-output-hk/Alonzo-testnet/blob/main/resources/plutus-sources/plutus-example/app/plutus-minting-purple-example.hs
  let (validatorPurpleScript, validatorAsSBS) = serializeValidator validator
  case Plutus.defaultCostModelParams of
    Just m ->
      let Alonzo.Data pData = toAlonzoData (ScriptDataNumber 42)
          (logout, e) =
            Plutus.evaluateScriptCounting Plutus.Verbose m validatorAsSBS [pData]
       in do
            print ("Log output" :: String) >> print logout
            case e of
              Left evalErr -> print ("Eval Error" :: String) >> print evalErr
              Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
    Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope file Nothing validatorPurpleScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()

policyToPlutus file policy =
  validatorToPlutus
    file
    $ Plutus.Validator $ Plutus.unMintingPolicyScript policy

serializeValidator :: Validator -> (PlutusScript PlutusScriptV1, SBS.ShortByteString)
serializeValidator validator =
  let sbs :: SBS.ShortByteString
      sbs = SBS.toShort . LB.toStrict . serialise $ validator

      purpleScript :: PlutusScript PlutusScriptV1
      purpleScript = PlutusScriptSerialised sbs
   in (purpleScript, sbs)

writeData file isData = LB.writeFile file (toSchemeJson isData)

toSchemeJson :: ToData a => a -> LB.ByteString
toSchemeJson =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
