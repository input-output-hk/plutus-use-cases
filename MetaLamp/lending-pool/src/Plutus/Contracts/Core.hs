{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Plutus.Contracts.Core where

import           Control.Monad                    hiding (fmap)
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Scripts                   as Scripts
import qualified Ledger.Typed.Scripts             as Scripts
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import           Plutus.V1.Ledger.Value
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..),
                                                   unless)
import           Prelude                          (Semigroup (..))
import qualified Prelude

deriving anyclass instance ToSchema AssetClass

newtype Aave = Aave
    { aaveProtocolInst :: AssetClass
    } deriving stock    (Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Aave

instance Prelude.Eq Aave where
    u == v = aaveProtocolInst u Prelude.== aaveProtocolInst v

instance Prelude.Ord Aave where
    compare u v = Prelude.compare (aaveProtocolInst u) (aaveProtocolInst v)

type LendingPoolId = AssetClass

type AnyAddress = BS.ByteString

data LendingPool = LendingPool
    { lpCurrency       :: LendingPoolId,
      lpAToken         :: AssetClass,
      lpAmount         :: Integer,
      lpDebtToken      :: AssetClass,
      lpLiquidityIndex :: Integer
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''LendingPool
PlutusTx.makeLift ''LendingPool

data UserConfig = UserConfig
    { ucAddress           :: PubKeyHash,
      ucReserveId         :: LendingPoolId,
      ucUsingAsCollateral :: Bool
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''UserConfig
PlutusTx.makeLift ''UserConfig

type Factory = [LendingPoolId]
data AaveAction = CreateLendingPool LendingPool | UpdateLendingPool | CreateUser UserConfig | UpdateUser | Withdraw
    deriving Show

PlutusTx.unstableMakeIsData ''AaveAction
PlutusTx.makeLift ''AaveAction

data AaveDatum = Factory Factory | Pool LendingPool | User UserConfig | Deposit deriving stock (Show)

PlutusTx.unstableMakeIsData ''AaveDatum
PlutusTx.makeLift ''AaveDatum

data AaveScript
instance Scripts.ScriptType AaveScript where
    type instance RedeemerType AaveScript = AaveAction
    type instance DatumType AaveScript = AaveDatum

{-# INLINABLE makeAaveValidator #-}
-- Main validator
-- Each state field must have one or more associated actions(Redeemer types),
-- produced on state update, which are then validated here
-- TODO: write validations
makeAaveValidator :: Aave
                   -> AaveDatum
                   -> AaveAction
                   -> ScriptContext
                   -> Bool
makeAaveValidator _ _ (CreateLendingPool _) _ = True
makeAaveValidator _ _ UpdateLendingPool _     = True
makeAaveValidator _ _ (CreateUser _) _        = True
makeAaveValidator _ _ UpdateUser _            = True
makeAaveValidator _ _ Withdraw _              = True
makeAaveValidator _  _  _  _                  = False

aaveProtocolName :: TokenName
aaveProtocolName = "Aave"

aaveInstance :: Aave -> Scripts.ScriptInstance AaveScript
aaveInstance aave = Scripts.validator @AaveScript
    ($$(PlutusTx.compile [|| makeAaveValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode aave)
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @AaveDatum @AaveAction

aaveScript :: Aave -> Validator
aaveScript = Scripts.validatorScript . aaveInstance

aaveHash :: Aave -> Ledger.ValidatorHash
aaveHash = Scripts.validatorHash . aaveScript

aaveAddress :: Aave -> Ledger.Address
aaveAddress = Ledger.scriptAddress . aaveScript

aave :: CurrencySymbol -> Aave
aave protocol = Aave (assetClass protocol aaveProtocolName)

-- State forging validator/policy
-- TODO: Fix
validateStateForging :: Aave -> TokenName -> ScriptContext -> Bool
validateStateForging aave tn ctx = True
{-case [ i
                                          | i <- txInfoInputs $ scriptContextTxInfo ctx
                                          , let v = valueWithin i
                                          , (assetClassValueOf v aaveToken == 1) ||
                                            (assetClassValueOf v stateToken == 1)
                                          ] of
    [_]    -> True
    [_, _] -> True
    _      -> traceError "State forging without Aave input"
  where
    aaveToken = aaveProtocolInst aave
    stateToken = assetClass (ownCurrencySymbol ctx) tn
    valueWithin = txOutValue . txInInfoResolved
-}

makeStatePolicy :: TokenName -> Aave -> MonetaryPolicy
makeStatePolicy tokenName aave = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \u t -> Scripts.wrapMonetaryPolicy (validateStateForging u t) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode aave
        `PlutusTx.applyCode` PlutusTx.liftCode tokenName

makeStateCurrency :: TokenName -> Aave -> CurrencySymbol
makeStateCurrency tokenName = scriptCurrencySymbol . makeStatePolicy tokenName

makeStateToken :: TokenName -> Aave -> AssetClass
makeStateToken tokenName = flip assetClass tokenName . makeStateCurrency tokenName

poolStateToken, userStateToken :: Aave -> AssetClass
poolStateToken = makeStateToken "aaveLendingPool"
userStateToken = makeStateToken "aaveUser"
