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

{-# LANGUAGE StandaloneDeriving #-}
module Plutus.Contracts.LendingPool
    ( AssetClass (..)
    , Aave (..), aave
    , AaveUserSchema, AaveOwnerSchema, UserContractState (..)
    , start, create, close
    , ownerEndpoint, userEndpoints
    ) where

import           Control.Monad                    hiding (fmap)
import qualified Data.Map                         as Map
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Scripts                   as Scripts
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     as Value
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
-- TODO remove that dep Plutus.Contracts.Currency (?)
import qualified Plutus.Contracts.Currency        as Currency
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), unless)
import           Prelude                          (Semigroup (..))
import qualified Prelude
import           Text.Printf                      (printf)
import Plutus.V1.Ledger.Ada ( lovelaceValueOf )

aaveProtocolName, aaveTokenName :: TokenName
aaveProtocolName = "Aave"
aaveTokenName = "aToken"

deriving anyclass instance ToSchema AssetClass

newtype LendingPool = LendingPool
    { lpToken :: AssetClass
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''LendingPool
PlutusTx.makeLift ''LendingPool

instance Eq LendingPool where
    {-# INLINABLE (==) #-}
    x == y = lpToken x == lpToken y

newtype Aave = Aave
    { aaveProtocolInst :: AssetClass
    } deriving stock    (Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Aave

instance Prelude.Eq Aave where
    u == v = aaveProtocolInst u Prelude.== aaveProtocolInst v

instance Prelude.Ord Aave where
    compare u v = Prelude.compare (aaveProtocolInst u) (aaveProtocolInst v)

data AaveAction = Create LendingPool | Close
    deriving Show

PlutusTx.unstableMakeIsData ''AaveAction
PlutusTx.makeLift ''AaveAction

data AaveDatum =
      Factory [LendingPool]
    | Pool LendingPool Integer -- Pool consisting of lending pool link and aTokens amount
    deriving stock (Show)

PlutusTx.unstableMakeIsData ''AaveDatum
PlutusTx.makeLift ''AaveDatum

data AaveScript
instance Scripts.ScriptType AaveScript where
    type instance RedeemerType AaveScript = AaveAction
    type instance DatumType AaveScript = AaveDatum

{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINABLE validateCreate #-}
validateCreate :: Aave
               -> [LendingPool]
               -> LendingPool
               -> ScriptContext
               -> Bool
validateCreate Aave{..} lps lp@LendingPool{..} ctx =
    traceIfFalse "Aave assetClassValue not present" (assetClassValueOf (valueWithin $ findOwnInput' ctx) aaveProtocolInst == 1) &&
    notElem lp lps                                                                                      &&
    Constraints.checkOwnOutputConstraint ctx (OutputConstraint (Factory $ lp : lps) $ assetClassValue aaveProtocolInst 1)     &&
    (assetClassValueOf forged lpToken == aTokensNum) &&
    Constraints.checkOwnOutputConstraint ctx (OutputConstraint (Pool lp aTokensNum) $ assetClassValue lpToken aTokensNum)
  where
    poolOutput :: TxOut
    poolOutput = case [o | o <- getContinuingOutputs ctx, assetClassValueOf (txOutValue o) lpToken == 1] of
        [o] -> o
        _   -> traceError "expected exactly one pool output"

    forged :: Value
    forged = txInfoForge $ scriptContextTxInfo ctx

    aTokensNum :: Integer
    aTokensNum = assetClassValueOf forged lpToken

{-# INLINABLE validateCloseFactory #-}
validateCloseFactory :: Aave -> [LendingPool] -> ScriptContext -> Bool
validateCloseFactory aa lps ctx =
    traceIfFalse "Aave assetClassValue not present" (assetClassValueOf (valueWithin $ findOwnInput' ctx) aaC == 1)
    -- traceIfFalse "wrong forge value"        (txInfoForge info == negate (assetClassValue c 1)) &&
    -- traceIfFalse "factory output wrong"
    --     (Constraints.checkOwnOutputConstraint ctx $ OutputConstraint (Factory $ filter (/= fst lpLiquidity) lps) $ assetClassValue aaC 1)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- poolInput :: TxInInfo
    -- poolInput = case [ i
    --                  | i <- txInfoInputs info
    --                  , assetClassValueOf (valueWithin i) c == 1
    --                  ] of
    --     [i] -> i
    --     _   -> traceError "expected exactly one pool input"

    -- lpLiquidity :: (LendingPool, Integer)
    -- lpLiquidity = case txOutDatumHash . txInInfoResolved $ poolInput of
    --     Nothing -> traceError "pool input witness missing"
    --     Just h  -> findPoolDatum info h

    aaC :: AssetClass
    aaC = aaveProtocolInst aa

{-# INLINABLE validateClosePool #-}
validateClosePool :: Aave -> ScriptContext -> Bool
validateClosePool aa ctx = hasFactoryInput
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasFactoryInput :: Bool
    hasFactoryInput =
        traceIfFalse "Aave factory input expected" $
        assetClassValueOf (valueSpent info) (aaveProtocolInst aa) == 1


{-# INLINABLE findPoolDatum #-}
findPoolDatum :: TxInfo -> DatumHash -> (LendingPool, Integer)
findPoolDatum info h = case findDatum h info of
    Just (Datum d) -> case PlutusTx.fromData d of
        Just (Pool lp a) -> (lp, a)
        _                -> traceError "error decoding data"
    _              -> traceError "pool input datum not found"

mkAaveValidator :: Aave
                   -> AaveDatum
                   -> AaveAction
                   -> ScriptContext
                   -> Bool
mkAaveValidator aa (Factory lps) (Create lp) ctx = validateCreate aa lps lp ctx
mkAaveValidator aa (Factory lps) Close       ctx = validateCloseFactory aa lps ctx
mkAaveValidator aa (Pool _  _)   Close       ctx = validateClosePool aa ctx
mkAaveValidator _  _             _           _   = False

validateLiquidityForging :: Aave -> TokenName -> ScriptContext -> Bool
validateLiquidityForging aa tn ctx = case [ i
                                          | i <- txInfoInputs $ scriptContextTxInfo ctx
                                          , let v = valueWithin i
                                          , (assetClassValueOf v aaC == 1) ||
                                            (assetClassValueOf v lpC == 1)
                                          ] of
    [_]    -> True
    [_, _] -> True
    _      -> traceError "pool state forging without Aave input"
  where
    aaC, lpC :: AssetClass
    aaC = aaveProtocolInst aa
    lpC = assetClass (ownCurrencySymbol ctx) tn

aaveInstance :: Aave -> Scripts.ScriptInstance AaveScript
aaveInstance aa = Scripts.validator @AaveScript
    ($$(PlutusTx.compile [|| mkAaveValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode aa)
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

liquidityPolicy :: Aave -> MonetaryPolicy
liquidityPolicy aa = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \a t -> Scripts.wrapMonetaryPolicy (validateLiquidityForging a t) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode aa
        `PlutusTx.applyCode` PlutusTx.liftCode aaveTokenName

-- | Creates a Aave "factory". This factory will keep track of the existing liquidity pools and enforce that there will be at most one liquidity pool
-- for any pair of tokens at any given time.
start :: HasBlockchainActions s => Contract w s Text Aave
start = do
    pkh <- pubKeyHash <$> ownPubKey
    cs  <- fmap Currency.currencySymbol $
           mapError (pack . show @Currency.CurrencyError) $
           Currency.forgeContract pkh [(aaveProtocolName, 1)]
    let c    = assetClass cs aaveProtocolName
    let aa   = aave cs
        inst = aaveInstance aa
        tx   = mustPayToTheScript (Factory []) $ assetClassValue c 1
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo @String $ printf "started Aave %s at address %s" (show aa) (show $ aaveAddress aa)
    return aa

-- | Creates a liquidity pool for a pair of coins. The creator provides liquidity for both coins and gets liquidity tokens in return.
create :: HasBlockchainActions s => Aave -> Integer -> Contract w s Text ()
create aa aTokensNum = do
    pkh <- pubKeyHash <$> ownPubKey
    cs  <- fmap Currency.currencySymbol $
           mapError (pack . show @Currency.CurrencyError) $
           Currency.forgeContract pkh [(aaveTokenName, aTokensNum)]
    (oref, o, lps) <- findAaveFactory aa
    let c    = assetClass cs aaveTokenName
    let lp        = LendingPool c
    let aaInst   = aaveInstance aa
        aaScript = aaveScript aa
        aaDat1   = Factory $ lp : lps
        aaDat2   = Pool lp aTokensNum
        aaVal    = assetClassValue (aaveProtocolInst aa) 1
        lpVal    = lovelaceValueOf aTokensNum

        lookups  = Constraints.scriptInstanceLookups aaInst        <>
                   Constraints.otherScript aaScript                <>
                   Constraints.monetaryPolicy (liquidityPolicy aa) <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript aaDat1 aaVal                                               <>
                   Constraints.mustPayToTheScript aaDat2 lpVal                                               <>
                   Constraints.mustForgeValue (assetClassValue c aTokensNum)                 <>
                   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Create lp)

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "created liquidity pool: " ++ show lp

-- TODO fix close and validateClose
-- | Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
close :: HasBlockchainActions s => Aave -> AssetClass -> Contract w s Text ()
close aa poolCoin = do
    pkh <- pubKeyHash <$> ownPubKey
    ((oref1, o1, lps), (oref2, o2, lp, liquidity)) <- findAaveFactoryAndPool aa $ LendingPool poolCoin
    let aaInst   = aaveInstance aa
        aaScript = aaveScript aa
        aaDat    = Factory $ filter (/= lp) lps
        aaC      = aaveProtocolInst aa
        aaVal    = assetClassValue aaC 1
        psVal    = assetClassValue poolCoin 1
        redeemer = Redeemer $ PlutusTx.toData Close

        lookups  = Constraints.scriptInstanceLookups aaInst        <>
                   Constraints.otherScript aaScript                <>
                   Constraints.monetaryPolicy (liquidityPolicy aa) <>
                   Constraints.ownPubKeyHash pkh                   <>
                   Constraints.unspentOutputs (Map.singleton oref1 o1 <> Map.singleton oref2 o2)

        tx       = Constraints.mustPayToTheScript aaDat aaVal          <>
                   Constraints.mustForgeValue (negate psVal) <>
                   Constraints.mustSpendScriptOutput oref1 redeemer    <>
                   Constraints.mustSpendScriptOutput oref2 redeemer    <>
                   Constraints.mustIncludeDatum (Datum $ PlutusTx.toData $ Pool lp liquidity)

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "closed liquidity pool: " ++ show lp

getAaveDatum :: TxOutTx -> Contract w s Text AaveDatum
getAaveDatum o = case txOutDatumHash $ txOutTxOut o of
        Nothing -> throwError "datumHash not found"
        Just h -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing -> throwError "datum not found"
            Just (Datum e) -> case PlutusTx.fromData e of
                Nothing -> throwError "datum has wrong type"
                Just d  -> return d

findAaveInstance :: HasBlockchainActions s => Aave -> AssetClass -> (AaveDatum -> Maybe a) -> Contract w s Text (TxOutRef, TxOutTx, a)
findAaveInstance aa c f = do
    let addr = aaveAddress aa
    logInfo @String $ printf "looking for Aave instance at address %s containing assetClassValue %s " (show addr) (show c)
    utxos <- utxoAt addr
    go  [x | x@(_, o) <- Map.toList utxos, assetClassValueOf (txOutValue $ txOutTxOut o) c == 1]
  where
    go [] = throwError "Aave instance not found"
    go ((oref, o) : xs) = do
        d <- getAaveDatum o
        case f d of
            Nothing -> go xs
            Just a  -> do
                logInfo @String $ printf "found Aave instance with datum: %s" (show d)
                return (oref, o, a)

findAaveFactory :: HasBlockchainActions s => Aave -> Contract w s Text (TxOutRef, TxOutTx, [LendingPool])
findAaveFactory aa@Aave{..} = findAaveInstance aa aaveProtocolInst $ \case
    Factory lps -> Just lps
    Pool _ _    -> Nothing

findAavePool :: HasBlockchainActions s => Aave -> LendingPool -> Contract w s Text (TxOutRef, TxOutTx, Integer)
findAavePool aa lp = findAaveInstance aa (lpToken lp) $ \case
        Pool lp' l
            | lp == lp' -> Just l
        _               -> Nothing

findAaveFactoryAndPool :: HasBlockchainActions s
                          => Aave
                          -> LendingPool
                          -> Contract w s Text ( (TxOutRef, TxOutTx, [LendingPool])
                                               , (TxOutRef, TxOutTx, LendingPool, Integer)
                                               )
findAaveFactoryAndPool aa lpToFind = do
    (oref1, o1, lps) <- findAaveFactory aa
    case [ lp'
         | lp' <- lps
         , lp' == lpToFind
         ] of
        [lp] -> do
            (oref2, o2, a) <- findAavePool aa lp
            return ( (oref1, o1, lps)
                   , (oref2, o2, lp, a)
                   )
        _    -> throwError "liquidity pool not found"

ownerEndpoint :: Contract (Last (Either Text Aave)) BlockchainActions Void ()
ownerEndpoint = do
    e <- runError start
    tell $ Last $ Just $ case e of
        Left err -> Left err
        Right aa -> Right aa

type AaveOwnerSchema =
    BlockchainActions
        .\/ Endpoint "start" ()

-- | Schema for the endpoints for users of Aave.
type AaveUserSchema =
    BlockchainActions
        .\/ Endpoint "create" Integer
        .\/ Endpoint "close"  AssetClass
        .\/ Endpoint "stop"   ()

-- | Type of the Aave user contract state.
data UserContractState = Created
    | Closed
    | Stopped
    deriving (Show, Generic, FromJSON, ToJSON)

-- | Provides the following endpoints for users of a Aave instance:
--
--      [@create@]: Creates a liquidity pool for a pair of coins. The creator provides liquidity for both coins and gets liquidity tokens in return.
--      [@close@]: Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
--      [@stop@]: Stops the contract.
userEndpoints :: Aave -> Contract (Last (Either Text UserContractState)) AaveUserSchema Void ()
userEndpoints aa =
    stop
        `select`
    ((f (Proxy @"create") (const Created) create                 `select`
      f (Proxy @"close")  (const Closed)  close                  )
      >> userEndpoints aa)
  where
    f :: forall l a p.
         HasEndpoint l p AaveUserSchema
      => Proxy l
      -> (a -> UserContractState)
      -> (Aave -> p -> Contract (Last (Either Text UserContractState)) AaveUserSchema Text a)
      -> Contract (Last (Either Text UserContractState)) AaveUserSchema Void ()
    f _ g c = do
        e <- runError $ do
            p <- endpoint @l
            c aa p
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right a  -> Right $ g a

    stop :: Contract (Last (Either Text UserContractState)) AaveUserSchema Void ()
    stop = do
        e <- runError $ endpoint @"stop"
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right () -> Right Stopped
