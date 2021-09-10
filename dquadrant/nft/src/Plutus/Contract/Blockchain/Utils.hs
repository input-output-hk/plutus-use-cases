{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE TupleSections #-}
module Plutus.Contract.Blockchain.Utils
where
import PlutusTx.Prelude
import Ledger.Value as Value ( Value )
import           Ledger.Contexts          (ScriptContext (..), TxInfo (..))
import Ledger
    ( findDatum,
      findOwnInput,
      ownHash,
      valueLockedBy,
      ScriptContext(ScriptContext, scriptContextTxInfo),
      TxInInfo(TxInInfo, txInInfoResolved),
      TxInfo(TxInfo, txInfoInputs),
      TxOut(..),
      Value,
      Datum(getDatum),
      scriptHashAddress,
      Address, toValidatorHash )
import PlutusTx
import Ledger.Credential
import Ledger.Address (addressCredential)

--  The functions in this  module are not bounded to the marketplace use case.
--- These functions should probably be provided by the Plutus Library itself.


