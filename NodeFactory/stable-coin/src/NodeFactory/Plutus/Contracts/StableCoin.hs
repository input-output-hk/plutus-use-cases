{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module NodeFactory.Plutus.Contracts.StableCoin
    (
        StableCoin (..), stablecoin,
        vaultStateCoinFromStableCoinCurrency
    ) where

import           Control.Monad                    hiding (fmap)
import           Data.Monoid                      (Last (..))
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     as Value
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import qualified Plutus.Contracts.Currency        as Currency
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), unless)
import           Prelude                          (Semigroup (..))
import qualified Prelude
import           Text.Printf                      (printf)

import           NodeFactory.Plutus.Contracts.Coin

stableCoinTokenName, vaultStateTokenName :: TokenName
stableCoinTokenName = "Stable Coin Token"
vaultStateTokenName = "Vault State Token"

-- Structs

data StableCoinVault = StableCoinVault
    { owner  :: !PubKeyHash      -- owner of the of the vault
    , amount :: !Integer         -- amount of ADA locked in vault
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.unstableMakeIsData ''StableCoinVault
PlutusTx.makeLift ''StableCoinVault

data StableCoin = StableCoin
    { sCoin :: Coin
--    scOracle :: PubKeyHash -- oracle identificator
    } deriving stock    (Show, Generic)
      deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''StableCoin
PlutusTx.makeLift ''StableCoin

instance Prelude.Eq StableCoin where
    u == v = sCoin u Prelude.== sCoin v

instance Prelude.Ord StableCoin where
    compare u v = Prelude.compare (sCoin u) (sCoin v)

data StableCoinAction = Create StableCoinVault | Close
    deriving Show

PlutusTx.unstableMakeIsData ''StableCoinAction
PlutusTx.makeLift ''StableCoinAction

data StableCoinDatum = 
    Factory [StableCoinVault]
  | Vault StableCoinVault
  deriving stock (Show)

PlutusTx.unstableMakeIsData ''StableCoinDatum
PlutusTx.makeLift ''StableCoinDatum

data StableCoining
instance Scripts.ScriptType StableCoining where
    type instance DatumType StableCoining = StableCoinDatum
    type instance RedeemerType StableCoining = StableCoinAction

-- Validators

{-# INLINABLE validateCreate #-}
validateCreate :: StableCoin
            -> Coin
            -> [StableCoinVault]
            -> StableCoinVault
            -> ScriptContext
            -> Bool
validateCreate StableCoin{..} c vs v@StableCoinVault{..} ctx =
    traceIfFalse "StableCoin coin not present" inputHasVaultToken    &&
    Constraints.checkOwnOutputConstraint ctx (OutputConstraint (Factory $ v : vs) $ coin sCoin 1)
    -- Constraints.checkOwnOutputConstraint ctx (OutputConstraint (Vault v) $ coin c 1) TODO - fix checking vault coin
    -- TODO - Add constraint checking output amount of stable coin appropriate
    -- TODO - Add constraint checking input amount of ADA
  where
    ownInput :: TxOut               
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "stable coin input missing"
        Just i  -> txInInfoResolved i

    inputHasVaultToken :: Bool           -- check if input contains nft token
    inputHasVaultToken = assetClassValueOf (txOutValue ownInput) (coinAssetClass $ c) == 1

{-# INLINABLE validateCloseFactory #-}
validateCloseFactory :: StableCoin 
                    -> Coin 
                    -> [StableCoinVault] 
                    -> ScriptContext 
                    -> Bool
validateCloseFactory sc c vs ctx = 
    traceIfFalse "StableCoin coin not present" inputHasStableCoinToken
  where 
    usC :: Coin
    usC = sCoin sc

    ownInput :: TxOut               
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "stable coin input missing"
        Just i  -> txInInfoResolved i

    inputHasStableCoinToken :: Bool           -- check if input contains nft token
    inputHasStableCoinToken = assetClassValueOf (txOutValue ownInput) (coinAssetClass $ usC) == 1

{-# INLINABLE validateClosePool #-}
validateClosePool :: StableCoin 
                -> ScriptContext 
                -> Bool
validateClosePool sc ctx = hasFactoryInput
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasFactoryInput :: Bool
        hasFactoryInput =
            traceIfFalse "Stable coin factory input expected" $ coinValueOf (valueSpent info) (sCoin sc) == 1


mkStableCoinValidator :: StableCoin
                    -> Coin
                    -> StableCoinDatum
                    -> StableCoinAction
                    -> ScriptContext
                    -> Bool
mkStableCoinValidator sc c (Factory vs) (Create v)  ctx = validateCreate sc c vs v ctx
mkStableCoinValidator sc c (Factory vs) Close       ctx = validateCloseFactory sc c vs ctx
mkStableCoinValidator sc _ (Vault _)    Close       ctx = validateClosePool sc ctx
mkStableCoinValidator _  _ _            _           _   = False

stableCoinInstance :: StableCoin -> Scripts.ScriptInstance StableCoining
stableCoinInstance sc = Scripts.validator @StableCoining
    ($$(PlutusTx.compile [|| mkStableCoinValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode sc
        `PlutusTx.applyCode` PlutusTx.liftCode c)
     $$(PlutusTx.compile [|| wrap ||])
  where
    c :: Coin
    c = vaultStateCoin sc

    wrap = Scripts.wrapValidator @StableCoinDatum @StableCoinAction

-- TODO implement forging validation
-- validateLiquidityForging :: StableCoin -> TokenName -> ScriptContext -> Bool
-- validateLiquidityForging us tn ctx = 

validateLiquidityForging :: StableCoin -> TokenName -> ScriptContext -> Bool
validateLiquidityForging sc tn ctx = True -- TODO replace with real forging validation

stableCoinValidator :: StableCoin -> Validator
stableCoinValidator = Scripts.validatorScript . stableCoinInstance

stableCoinAddress :: StableCoin -> Ledger.Address
stableCoinAddress = scriptAddress . stableCoinValidator

stablecoin :: CurrencySymbol -> StableCoin
stablecoin cs = StableCoin $ Coin cs stableCoinTokenName

liquidityPolicy :: StableCoin -> MonetaryPolicy
liquidityPolicy us = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \u t -> Scripts.wrapMonetaryPolicy (validateLiquidityForging u t) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode us
        `PlutusTx.applyCode` PlutusTx.liftCode vaultStateTokenName

liquidityCurrency :: StableCoin -> CurrencySymbol
liquidityCurrency = scriptCurrencySymbol . liquidityPolicy

vaultStateCoin :: StableCoin -> Coin
vaultStateCoin = flip Coin vaultStateTokenName . liquidityCurrency

vaultStateCoinFromStableCoinCurrency :: CurrencySymbol -> Coin
vaultStateCoinFromStableCoinCurrency = vaultStateCoin . stablecoin

----

start :: HasBlockchainActions s => Contract w s Text StableCoin
start = do
    pkh <- pubKeyHash <$> ownPubKey
    cs  <- fmap Currency.currencySymbol $
           mapError (pack . show @Currency.CurrencyError) $
           Currency.forgeContract pkh [(stableCoinTokenName, 1)]
    let c    = Coin cs stableCoinTokenName
        us   = stablecoin cs
        inst = stableCoinInstance us
        tx   = mustPayToTheScript (Factory []) $ coin c 1
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo @String $ printf "started StableCoin %s at address %s" (show us) (show $ stableCoinAddress us)
    return us

ownerEndpoint :: Contract (Last (Either Text StableCoin)) BlockchainActions Void ()
ownerEndpoint = do
    e <- runError start
    tell $ Last $ Just $ case e of
        Left err -> Left err
        Right us -> Right us