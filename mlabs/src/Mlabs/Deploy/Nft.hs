module Mlabs.Deploy.Nft (serializeNft) where

import PlutusTx.Prelude hiding (error)
import Prelude (IO, String)

import Mlabs.Emulator.Types (UserId (..))
import Mlabs.NftStateMachine.Contract.Forge as F
import Mlabs.NftStateMachine.Contract.StateMachine as SM
import Mlabs.NftStateMachine.Logic.Types

-- import Data.ByteString.Lazy qualified as LB
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Typed.Scripts.Validators as VS
import Plutus.V1.Ledger.Api qualified as Plutus
import PlutusTx.Ratio qualified as R

import Mlabs.Deploy.Utils

serializeNft ::
  BuiltinByteString ->
  Integer ->
  BuiltinByteString ->
  BuiltinByteString ->
  String ->
  IO ()
serializeNft txId txIx ownerPkh content outDir = do
  let txOutRef =
        Plutus.TxOutRef
          (Plutus.TxId txId)
          txIx
      userId = UserId $ PaymentPubKeyHash $ Plutus.PubKeyHash ownerPkh
      initNftDatum = initNft txOutRef userId content (R.reduce 1 2) (Just 1000)
      nftId = nft'id initNftDatum
      typedValidator = SM.scriptInstance nftId
      policy = F.currencyPolicy (validatorAddress typedValidator) nftId

  -- print $ nftId'token nftId
  -- BS.writeFile (outDir ++ "/t_name") (fromBuiltin content)
  -- validatorToPlutus (outDir ++ "/NftScript.plutus")
  --   (VS.validatorScript typedValidator)
  policyToPlutus (outDir ++ "/NftPolicy.plutus") policy

-- writeData (outDir ++ "/init-datum.json") initNftDatum
