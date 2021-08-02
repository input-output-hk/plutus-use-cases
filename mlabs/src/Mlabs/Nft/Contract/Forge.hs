-- | Validation of forge for NFTs
module Mlabs.Nft.Contract.Forge(
    currencyPolicy
  , currencySymbol
) where

import PlutusTx.Prelude

import Ledger (CurrencySymbol, Address)
import Ledger.Typed.Scripts (MonetaryPolicy)
import qualified Plutus.V1.Ledger.Value    as Value
import qualified Plutus.V1.Ledger.Scripts  as Scripts
import qualified Ledger.Typed.Scripts      as Scripts
import qualified Plutus.V1.Ledger.Contexts as Contexts
import qualified PlutusTx

import Mlabs.Nft.Logic.Types ( NftId(NftId) )

{-# INLINABLE validate #-}
-- | Validation of Minting of NFT-token. We guarantee uniqueness of NFT
-- by make the script depend on spending of concrete TxOutRef in the list of inputs.
-- TxOutRef for the input is specified inside NftId value.
--
-- Also we check that
--
-- * user mints token that corresponds to the content of NFT (token name is hash of NFT content)
-- * user spends NFT token to the StateMachine script
--
-- First argument is an address of NFT state machine script. We use it to check
-- that NFT coin was payed to script after minting.
validate :: Address -> NftId -> Contexts.ScriptContext -> Bool
validate stateAddr (NftId token oref) ctx =
     traceIfFalse "UTXO not consumed"     hasUtxo
  && traceIfFalse "wrong amount minted"   checkMintedAmount
  && traceIfFalse "Does not pay to state" paysToState
  where
    info = Contexts.scriptContextTxInfo ctx

    hasUtxo = any (\inp -> Contexts.txInInfoOutRef inp == oref) $ Contexts.txInfoInputs info

    checkMintedAmount = case Value.flattenValue (Contexts.txInfoForge info) of
      [(cur, tn, val)] -> Contexts.ownCurrencySymbol ctx == cur && token == tn && val == 1
      _ -> False

    paysToState = any hasNftToken $ Contexts.txInfoOutputs info

    hasNftToken Contexts.TxOut{..} =
         txOutAddress == stateAddr
      && txOutValue == Value.singleton (Contexts.ownCurrencySymbol ctx) token 1

-------------------------------------------------------------------------------

-- | Monetary policy of NFT
-- First argument is an address of NFT state machine script.
currencyPolicy :: Address -> NftId -> MonetaryPolicy
currencyPolicy stateAddr nid = Scripts.mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| \x y -> Scripts.wrapMonetaryPolicy (validate x y) ||])
  `PlutusTx.applyCode` (PlutusTx.liftCode stateAddr)
  `PlutusTx.applyCode` (PlutusTx.liftCode nid)

-- | Currency symbol of NFT
-- First argument is an address of NFT state machine script.
currencySymbol :: Address -> NftId -> CurrencySymbol
currencySymbol stateAddr nid = Contexts.scriptCurrencySymbol (currencyPolicy stateAddr nid)

