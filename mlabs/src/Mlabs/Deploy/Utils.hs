module Mlabs.Deploy.Utils (
  validatorToPlutus,
  policyToPlutus,
  writeData,
  toSchemeJson,
) where

import PlutusTx.Prelude hiding (error)
import Prelude (FilePath, IO, String, error, print)

import Data.Aeson as Json (encode)
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS

import Cardano.Api.Shelley (
  Error (displayError),
  PlutusScript (..),
  PlutusScriptV1,
  ScriptData (ScriptDataNumber),
  ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
  fromPlutusData,
  scriptDataToJson,
  toAlonzoData,
  writeFileTextEnvelope,
 )

import Cardano.Ledger.Alonzo.Data qualified as Alonzo
import Codec.Serialise (serialise)
import Plutus.V1.Ledger.Api (Validator)
import Plutus.V1.Ledger.Api qualified as Plutus
import PlutusTx (ToData, toData)

validatorToPlutus :: FilePath -> Validator -> IO ()
validatorToPlutus file validator = do
  -- taken from here
  -- https://github.com/input-output-hk/Alonzo-testnet/blob/main/resources/plutus-sources/plutus-example/app/plutus-minting-purple-example.hs
  let (validatorPurpleScript, validatorAsSBS) = serializeValidator validator
  case Plutus.defaultCostModelParams of
    Just m ->
      let getAlonzoData d = case toAlonzoData d of
            Alonzo.Data pData -> pData
          (logout, e) =
            Plutus.evaluateScriptCounting
              Plutus.Verbose
              m
              validatorAsSBS
              [getAlonzoData (ScriptDataNumber 42)]
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

policyToPlutus :: FilePath -> Plutus.MintingPolicy -> IO ()
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

writeData :: ToData a => FilePath -> a -> IO ()
writeData file isData = LB.writeFile file (toSchemeJson isData)

toSchemeJson :: ToData a => a -> LB.ByteString
toSchemeJson =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
