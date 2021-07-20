-- | Validation of forge for NFTs
module Mlabs.Nft.Contract.Forge(
    currencyPolicy
  , currencySymbol
) where

import PlutusTx.Prelude
import Ledger (CurrencySymbol, Address)

import Ledger.Typed.Scripts (MintingPolicy)
import qualified Plutus.V1.Ledger.Value as Value
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified Ledger.Typed.Scripts     as Scripts
import qualified PlutusTx                 as PlutusTx
import Plutus.V1.Ledger.Contexts

import Mlabs.Nft.Logic.Types

{-# INLINABLE validate #-}
-- | Validation of minting of NFT-token. We guarantee uniqueness of NFT
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
validate :: Address -> NftId -> () -> ScriptContext -> Bool
validate stateAddr (NftId token oref) _ ctx =
     traceIfFalse "UTXO not consumed"     hasUtxo
  && traceIfFalse "wrong amount minted"   checkMintedAmount
  && traceIfFalse "Does not pay to state" paysToState
  where
    info = scriptContextTxInfo ctx

    hasUtxo = any (\inp -> txInInfoOutRef inp == oref) $ txInfoInputs info

    checkMintedAmount = case Value.flattenValue (txInfoForge info) of
      [(cur, tn, val)] -> ownCurrencySymbol ctx == cur && token == tn && val == 1
      _ -> False

    paysToState = any hasNftToken $ txInfoOutputs info

    hasNftToken TxOut{..} =
         txOutAddress == stateAddr
      && txOutValue == Value.singleton (ownCurrencySymbol ctx) token 1

-------------------------------------------------------------------------------

-- | Minting policy of NFT
-- First argument is an address of NFT state machine script.
currencyPolicy :: Address -> NftId -> MintingPolicy
currencyPolicy stateAddr nid = Scripts.mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \x y -> Scripts.wrapMintingPolicy (validate x y) ||])
  `PlutusTx.applyCode` (PlutusTx.liftCode stateAddr)
  `PlutusTx.applyCode` (PlutusTx.liftCode nid)

-- | Currency symbol of NFT
-- First argument is an address of NFT state machine script.
currencySymbol :: Address -> NftId -> CurrencySymbol
currencySymbol stateAddr nid = scriptCurrencySymbol (currencyPolicy stateAddr nid)

