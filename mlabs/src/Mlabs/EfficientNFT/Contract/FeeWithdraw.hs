module Mlabs.EfficientNFT.Contract.FeeWithdraw (feeWithdraw) where

import PlutusTx.Prelude
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Map qualified as Map
import Ledger (ChainIndexTxOut (_ciTxOutValue), PubKeyHash, Redeemer (Redeemer), scriptHashAddress)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (Any, validatorHash, validatorScript)
import Plutus.Contract qualified as Contract
import Text.Printf (printf)

import Mlabs.EfficientNFT.Contract.Aux (getAddrUtxos)
import Mlabs.EfficientNFT.Dao (daoValidator)
import Mlabs.EfficientNFT.Types (UserContract)
import Plutus.V1.Ledger.Api (toBuiltinData)

feeWithdraw :: [PubKeyHash] -> UserContract ()
feeWithdraw pkhs = do
  let daoValidator' = daoValidator pkhs
      valHash = validatorHash daoValidator'
      scriptAddr = scriptHashAddress valHash
  pkh <- Contract.ownPaymentPubKeyHash
  utxos <- getAddrUtxos scriptAddr
  let feeValues = mconcat $ map _ciTxOutValue $ Map.elems utxos
      lookup =
        Hask.mconcat
          [ Constraints.typedValidatorLookups daoValidator'
          , Constraints.otherScript (validatorScript daoValidator')
          , Constraints.unspentOutputs utxos
          , Constraints.ownPaymentPubKeyHash pkh
          ]
      tx =
        Hask.mconcat
          ( [ Constraints.mustBeSignedBy pkh
            , Constraints.mustPayToPubKey pkh feeValues
            ]
              <> fmap (\utxo -> Constraints.mustSpendScriptOutput utxo (Redeemer $ toBuiltinData ())) (Map.keys utxos)
          )
  void $ Contract.submitTxConstraintsWith @Any lookup tx
  Contract.logInfo @Hask.String $ printf "Fee withdraw successful: %s" (Hask.show feeValues)
