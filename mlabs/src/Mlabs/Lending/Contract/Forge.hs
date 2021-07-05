{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Mlabs.Lending.Contract.Forge(
    currencySymbol
  , currencyPolicy
) where

import Control.Monad.State.Strict (evalStateT)

import PlutusTx.Prelude
import Ledger (CurrencySymbol)

import Ledger.Typed.Scripts (MonetaryPolicy)
import qualified Plutus.V1.Ledger.Value as Value
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified Ledger.Typed.Scripts     as Scripts
import qualified PlutusTx                 as PlutusTx
import Plutus.V1.Ledger.Contexts
import Ledger.Constraints

import Mlabs.Lending.Logic.Types
import Mlabs.Lending.Logic.State

data Input = Input
  { input'lendexId :: !LendexId
  , input'state    :: !LendingPool
  , input'value    :: !Value.Value
  }

{-# INLINABLE validate #-}
-- | Validation script for monetary policy.
--
-- We allow user to forge coins just in two cases:
--
-- * mint new aTokens in exchange for real tokens on deposit to lending app
-- * burn aTokens on withdraw from lending app
--
-- For mint case we check that:
--
-- * user deposit has grown properly on user's internal wallet for lending pool state
-- * user has paid enough real tokens to get aTokens
-- * script has paid enough aTokens to user in return
--
-- For burn case we check that:
--
-- * user deposit has diminished properly on user's internal wallet for leding pool state
-- * script has paid enough real tokens to the use rin return
--
-- Note that during burn user does not pay aTokens to the app they just get burned.
-- Only app pays to user in compensation for burn.
validate :: LendexId -> ScriptContext -> Bool
validate lendexId ctx = case (getInState, getOutState) of
  (Just st1, Just st2) ->
        if (hasLendexId  st1 && hasLendexId st2)
          then all (isValidForge st1 st2) $ Value.flattenValue $ txInfoForge info
          else traceIfFalse "Bad Lendex identifier" False
  (Just _  , Nothing)  -> traceIfFalse "Failed to find LendingPool state in outputs" False
  (Nothing,  Just _)   -> traceIfFalse "Failed to find LendingPool state in inputs" False
  _                    -> traceIfFalse "Failed to find TxOut with LendingPool state" False
  where
    hasLendexId x = input'lendexId x == lendexId

    -- find datum of lending app state in the inputs
    getInState = getStateForOuts $ fmap txInInfoResolved $ txInfoInputs info

    -- find datum of lending app state in the outputs
    getOutState = getStateForOuts $ txInfoOutputs info

    getStateForOuts outs = uniqueElement $ mapMaybe stateForTxOut outs

    stateForTxOut :: TxOut -> Maybe Input
    stateForTxOut out = do
      dHash <- txOutDatumHash out
      dat   <- Scripts.getDatum <$> findDatum dHash info
      (lid, st) <- PlutusTx.fromData dat
      pure $ Input lid st (txOutValue out)

    isValidForge :: Input -> Input -> (Value.CurrencySymbol, Value.TokenName, Integer) -> Bool
    isValidForge st1 st2 (cur, token, amount) = case getTokenCoin st1 st2 cur token of
      Just coin | amount >= 0 -> isValidMint st1 st2 coin aCoin amount
      Just coin               -> isValidBurn st1 st2 coin aCoin (negate amount)
      Nothing   -> traceIfFalse "Minted token is not supported" False
      where
        aCoin = Value.AssetClass (cur, token)

    getTokenCoin st1 st2 cur token
      | isValidCurrency st1 st2 cur = fromAToken (input'state st1) token
      | otherwise                   = Nothing

    -- check if states are based on the same monetary policy script
    isValidCurrency st1 st2 cur =
      cur == lp'currency (input'state st1) && cur == lp'currency (input'state st2)

    -- checks that user deposit becomes larger on given amount of minted tokens
    -- and user pays given amount to the lending app. We go through the list of all signatures
    -- to see if anyone acts as a user (satisfy constraints).
    isValidMint (Input _ st1 stVal1) (Input _ st2 stVal2) coin aCoin amount =
      traceIfFalse "No user is allowed to mint" $ any checkUserMint users
      where
        checkUserMint uid =
             checkUserDepositDiff uid
          && checkUserPays
          && checkScriptPays uid

        -- Check that user balance has growed on user inner wallet deposit
        checkUserDepositDiff uid = traceIfFalse "User deposit has not growed after Mint" $
          checkUserDepositDiffBy (\dep1 dep2 -> dep2 - dep1 == amount) st1 st2 coin uid

        -- Check that user payed value to script.
        -- We check that state value became bigger after state transition.
        checkUserPays = traceIfFalse "User does not pay for Mint" $
          stVal2 == (stVal1 <> Value.assetClassValue coin amount)

        -- Check that user recieved aCoins
        checkScriptPays uid = traceIfFalse "User has not received aCoins for Mint" $
          checkScriptContext (mustPayToPubKey uid $ Value.assetClassValue aCoin amount :: TxConstraints () ()) ctx

    isValidBurn (Input _lendexId1 st1 _stVal1) (Input _lendexId2 st2 _stVal2) coin _aCoin amount =
      traceIfFalse "No user is allowed to burn" $ any checkUserBurn users
      where
        checkUserBurn uid =
             checkUserDepositDiff uid
          && checkScriptPays uid

        -- Check that user balance has diminished on user inner wallet deposit
        checkUserDepositDiff uid = traceIfFalse "User deposit has not diminished after Burn" $
          checkUserDepositDiffBy (\dep1 dep2 -> dep1 - dep2 == amount) st1 st2 coin uid

        -- Check that user recieved coins
        checkScriptPays uid = traceIfFalse "User does not receive for Burn" $
          checkScriptContext (mustPayToPubKey uid $ Value.assetClassValue coin amount :: TxConstraints () ()) ctx

    -- check change of the user deposit for state prior to transition (st1) and after transition (st2)
    checkUserDepositDiffBy cond st1 st2 coin uid = either (const False) id $ do
      dep1 <- getDeposit uid coin st1
      dep2 <- getDeposit uid coin st2
      pure $ cond dep1 dep2

    getDeposit uid coin st = evalStateT (getsWallet (UserId uid) coin wallet'deposit) st

    users = txInfoSignatories info
    info  = scriptContextTxInfo ctx

-------------------------------------------------------------------------------

currencyPolicy :: LendexId -> MonetaryPolicy
currencyPolicy lid = Scripts.mkMonetaryPolicyScript $
  $$(PlutusTx.compile [|| Scripts.wrapMonetaryPolicy . validate ||])
  `PlutusTx.applyCode` (PlutusTx.liftCode lid)

currencySymbol :: LendexId -> CurrencySymbol
currencySymbol lid = scriptCurrencySymbol (currencyPolicy lid)

