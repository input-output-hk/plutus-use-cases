module Mlabs.Deploy.Governance where

import PlutusTx.Prelude hiding (error)
import Prelude (IO, String, error, print, undefined)

import Mlabs.Governance.Contract.Validation

import Ledger
import Ledger.Typed.Scripts.Validators as VS
import Plutus.V1.Ledger.Api qualified as Plutus
import Plutus.V1.Ledger.Scripts qualified as Scripts

import Mlabs.Deploy.Utils

outDir = "/home/mike/dev/mlabs/contract_deploy/node_mnt/plutus_files"

-- serializeGovernance txId txIx ownerPkh content outDir = do
serializeGovernance = do
  let alicePkh = "4cebc6f2a3d0111ddeb09ac48e2053b83b33b15f29182f9b528c6491"
      acGov =
        AssetClassGov
          "fda1b6b487bee2e7f64ecf24d24b1224342484c0195ee1b7b943db50" -- MintingPolicy.plutus
          "GOV"
      validator = VS.validatorScript $ govInstance acGov
      policy = xGovMintingPolicy acGov
      xGovCurrSymbol = scriptCurrencySymbol policy
      fstDatum = GovernanceDatum alicePkh xGovCurrSymbol

  validatorToPlutus (outDir ++ "/GovScript.plutus") validator
  policyToPlutus (outDir ++ "/GovPolicy.plutus") policy
