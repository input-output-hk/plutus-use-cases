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


module Plutus.Contracts.LendingPool
    ( Coin (..)
    , coin, coinValueOf
    , Aave (..), aave
    , poolStateCoinFromAaveCurrency
    , AaveUserSchema, UserContractState (..)
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

aaveTokenName, poolStateTokenName :: TokenName
aaveTokenName = "Aave"
poolStateTokenName = "Pool State"

-- | A pair consisting of a 'CurrencySymbol' and a 'TokenName'.
-- Coins are the entities that can be swapped in the exchange.
data Coin = Coin
    { cCurrency :: CurrencySymbol
    , cToken    :: TokenName
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''Coin
PlutusTx.makeLift ''Coin

instance Eq Coin where
    {-# INLINABLE (==) #-}
    c == d = cCurrency c == cCurrency d && cToken c == cToken d

instance Prelude.Eq Coin where
    (==) = (==)

{-# INLINABLE compareCoins #-}
compareCoins :: Coin -> Coin -> Ordering
compareCoins c d = case compare (cCurrency c) (cCurrency d) of
    LT -> LT
    GT -> GT
    EQ -> compare (cToken c) (cToken d)

instance Prelude.Ord Coin where
    compare = compareCoins

{-# INLINABLE coinLT #-}
coinLT :: Coin -> Coin -> Bool
coinLT c d = case compareCoins c d of
    LT -> True
    _  -> False

{-# INLINABLE coin #-}
-- | @'coin' c n@ denotes the value given by @n@ units of @'Coin'@ @c@.
coin :: Coin    -- ^ The 'Coin'.
     -> Integer -- ^ The desired number coins.
     -> Value   -- ^ The 'Value' consisting of the given number of units of the given 'Coin'.
coin Coin{..} = Value.singleton cCurrency cToken

{-# INLINABLE coinValueOf #-}
-- | Calculates how many units of the specified 'Coin' are contained in the
-- given 'Value'.
coinValueOf :: Value   -- ^ The 'Value' to inspect.
            -> Coin    -- ^ The 'Coin' to look for.
            -> Integer -- ^ The number of units of the given 'Coin' contained in the given 'Value'.
coinValueOf v Coin{..} = valueOf v cCurrency cToken

{-# INLINABLE hashCoin #-}
hashCoin :: Coin -> ByteString
hashCoin Coin{..} = sha2_256 $ concatenate (unCurrencySymbol cCurrency) (unTokenName cToken)

data Sqrt =
      Imaginary
    | Exact Integer
    | Irrational Integer
    deriving stock Show

PlutusTx.unstableMakeIsData ''Sqrt
PlutusTx.makeLift ''Sqrt

{-# INLINABLE rsqrt #-}
rsqrt :: Integer -> Integer -> Sqrt
rsqrt n d
    | n * d < 0 = Imaginary
    | n == 0    = Exact 0
    | n == d    = Exact 1
    | n < 0     = rsqrt (negate n) (negate d)
    | otherwise = go 1 $ 1 + divide n d
  where
    go :: Integer -> Integer -> Sqrt
    go l u
        | l * l * d == n = Exact l
        | u == (l + 1)   = Irrational l
        | otherwise      =
              let
                m = divide (l + u) 2
              in
                if m * m * d <= n then go m u
                                  else go l m

{-# INLINABLE isqrt #-}
isqrt :: Integer -> Sqrt
isqrt n = rsqrt n 1

{-# INLINABLE calculateInitialLiquidity #-}
calculateInitialLiquidity :: Integer -> Integer -> Integer
calculateInitialLiquidity outA outB = case isqrt (outA * outB) of
    Exact l
        | l > 0 -> l
    Irrational l
        | l > 0 -> l + 1
    _           -> traceError "insufficient liquidity"

data LendingPool = LendingPool
    { lpCoin :: Coin,
      lpIssuer :: PubKeyHash
    }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''LendingPool
PlutusTx.makeLift ''LendingPool

instance Eq LendingPool where
    {-# INLINABLE (==) #-}
    x == y = lpCoin x == lpCoin y && lpIssuer x == lpIssuer y

{-# INLINABLE hashLendingPool #-}
hashLendingPool :: LendingPool -> ByteString
hashLendingPool LendingPool{..} = sha2_256 $ hashCoin lpCoin <> getPubKeyHash lpIssuer

newtype Aave = Aave
    { aaveCoin :: Coin
    } deriving stock    (Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Aave

instance Prelude.Eq Aave where
    u == v = aaveCoin u Prelude.== aaveCoin v

instance Prelude.Ord Aave where
    compare u v = Prelude.compare (aaveCoin u) (aaveCoin v)

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

{-# INLINABLE validateCreate #-}
validateCreate :: Aave
               -> Coin
               -> [LendingPool]
               -> LendingPool
               -> ValidatorCtx
               -> Bool
validateCreate Aave{..} c lps lp@LendingPool{..} ctx =
    traceIfFalse "Aave coin not present" (coinValueOf (txInInfoValue $ findOwnInput ctx) aaveCoin == 1) &&
    notElem lp lps                                                                                      &&
    Constraints.checkOwnOutputConstraint ctx (OutputConstraint (Factory $ lp : lps) $ coin aaveCoin 1)     &&
    (coinValueOf forged c == 1)
    -- TODO validate aTokens forged
    -- Constraints.checkOwnOutputConstraint ctx (OutputConstraint (Pool lp liquidity) $ coin c 1)
  where
    poolOutput :: TxOutInfo
    poolOutput = case [o | o <- getContinuingOutputs ctx, coinValueOf (txOutValue o) c == 1] of
        [o] -> o
        _   -> traceError "expected exactly one pool output"

    forged :: Value
    forged = txInfoForge $ valCtxTxInfo ctx

{-# INLINABLE validateCloseFactory #-}
validateCloseFactory :: Aave -> Coin -> [LendingPool] -> ValidatorCtx -> Bool
validateCloseFactory aa c lps ctx =
    traceIfFalse "Aave coin not present" (coinValueOf (txInInfoValue $ findOwnInput ctx) usC == 1)             &&
    traceIfFalse "wrong forge value"        (txInfoForge info == negate (coin c 1)) &&
    traceIfFalse "factory output wrong"
        (Constraints.checkOwnOutputConstraint ctx $ OutputConstraint (Factory $ filter (/= fst lpLiquidity) lps) $ coin usC 1)
  where
    info :: TxInfo
    info = valCtxTxInfo ctx

    poolInput :: TxInInfo
    poolInput = case [ i
                     | i <- txInfoInputs info
                     , coinValueOf (txInInfoValue i) c == 1
                     ] of
        [i] -> i
        _   -> traceError "expected exactly one pool input"

    lpLiquidity :: (LendingPool, Integer)
    lpLiquidity = case txInInfoWitness poolInput of
        Nothing        -> traceError "pool input witness missing"
        Just (_, _, h) -> findPoolDatum info h

    usC :: Coin
    usC = aaveCoin aa

{-# INLINABLE validateClosePool #-}
validateClosePool :: Aave -> ValidatorCtx -> Bool
validateClosePool aa ctx = hasFactoryInput
  where
    info :: TxInfo
    info = valCtxTxInfo ctx

    hasFactoryInput :: Bool
    hasFactoryInput =
        traceIfFalse "Aave factory input expected" $
        coinValueOf (valueSpent info) (aaveCoin aa) == 1


{-# INLINABLE findPoolDatum #-}
findPoolDatum :: TxInfo -> DatumHash -> (LendingPool, Integer)
findPoolDatum info h = case findDatum h info of
    Just (Datum d) -> case PlutusTx.fromData d of
        Just (Pool lp a) -> (lp, a)
        _                -> traceError "error decoding data"
    _              -> traceError "pool input datum not found"

mkAaveValidator :: Aave
                   -> Coin
                   -> AaveDatum
                   -> AaveAction
                   -> ValidatorCtx
                   -> Bool
mkAaveValidator aa c (Factory lps) (Create lp) ctx = validateCreate aa c lps lp ctx
mkAaveValidator aa c (Factory lps) Close       ctx = validateCloseFactory aa c lps ctx
mkAaveValidator aa _ (Pool _  _)   Close       ctx = validateClosePool aa ctx
mkAaveValidator _  _ _             _           _   = False

validateLiquidityForging :: Aave -> TokenName -> PolicyCtx -> Bool
validateLiquidityForging aa tn ctx = case [ i
                                          | i <- txInfoInputs $ policyCtxTxInfo ctx
                                          , let v = txInInfoValue i
                                          , (coinValueOf v aaC == 1) ||
                                            (coinValueOf v lpC == 1)
                                          ] of
    [_]    -> True
    [_, _] -> True
    _      -> traceError "pool state forging without Aave input"
  where
    aaC, lpC :: Coin
    aaC = aaveCoin aa
    lpC = Coin (ownCurrencySymbol ctx) tn

aaveInstance :: Aave -> Scripts.ScriptInstance AaveScript
aaveInstance aa = Scripts.validator @AaveScript
    ($$(PlutusTx.compile [|| mkAaveValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode aa
        `PlutusTx.applyCode` PlutusTx.liftCode c)
     $$(PlutusTx.compile [|| wrap ||])
  where
    c :: Coin
    c = poolStateCoin aa

    wrap = Scripts.wrapValidator @AaveDatum @AaveAction

aaveScript :: Aave -> Validator
aaveScript = Scripts.validatorScript . aaveInstance

aaveHash :: Aave -> Ledger.ValidatorHash
aaveHash = Scripts.validatorHash . aaveScript

aaveAddress :: Aave -> Ledger.Address
aaveAddress = ScriptAddress . aaveHash

aave :: CurrencySymbol -> Aave
aave cs = Aave $ Coin cs aaveTokenName

liquidityPolicy :: Aave -> MonetaryPolicy
liquidityPolicy aa = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \a t -> Scripts.wrapMonetaryPolicy (validateLiquidityForging a t) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode aa
        `PlutusTx.applyCode` PlutusTx.liftCode poolStateTokenName

liquidityCurrency :: Aave -> CurrencySymbol
liquidityCurrency = scriptCurrencySymbol . liquidityPolicy

poolStateCoin :: Aave -> Coin
poolStateCoin = flip Coin poolStateTokenName . liquidityCurrency

-- | Gets the 'Coin' used to identity liquidity pools.
poolStateCoinFromAaveCurrency :: CurrencySymbol -- ^ The currency identifying the Aave instance.
                                 -> Coin
poolStateCoinFromAaveCurrency = poolStateCoin . aave

-- | Creates a Aave "factory". This factory will keep track of the existing liquidity pools and enforce that there will be at most one liquidity pool
-- for any pair of tokens at any given time.
start :: HasBlockchainActions s => Contract w s Text Aave
start = do
    pkh <- pubKeyHash <$> ownPubKey
    cs  <- fmap Currency.currencySymbol $
           mapError (pack . show @Currency.CurrencyError) $
           Currency.forgeContract pkh [(aaveTokenName, 1)]
    let c    = Coin cs aaveTokenName
        aa   = aave cs
        inst = aaveInstance aa
        tx   = mustPayToTheScript (Factory []) $ coin c 1
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
           Currency.forgeContract pkh [(poolStateTokenName, 1)]
    (oref, o, lps) <- findAaveFactory aa
    let c    = Coin cs poolStateTokenName
    let lp        = LendingPool c pkh
    let usInst   = aaveInstance aa
        usScript = aaveScript aa
        usDat1   = Factory $ lp : lps
        usDat2   = Pool lp aTokensNum
        psC      = poolStateCoin aa
        usVal    = coin (aaveCoin aa) 1
        -- TODO: forge aTokens
        lpVal    = coin psC 1

        lookups  = Constraints.scriptInstanceLookups usInst        <>
                   Constraints.otherScript usScript                <>
                   Constraints.monetaryPolicy (liquidityPolicy aa) <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript usDat1 usVal                                               <>
                   Constraints.mustPayToTheScript usDat2 lpVal                                               <>
                   Constraints.mustForgeValue (coin psC 1)                              <>
                   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Create lp)

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "created liquidity pool: " ++ show lp

-- | Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
close :: HasBlockchainActions s => Aave -> Coin -> Contract w s Text ()
close aa poolCoin = do
    pkh <- pubKeyHash <$> ownPubKey
    ((oref1, o1, lps), (oref2, o2, lp, liquidity)) <- findAaveFactoryAndPool aa $ LendingPool poolCoin pkh
    pkh                                            <- pubKeyHash <$> ownPubKey
    let usInst   = aaveInstance aa
        usScript = aaveScript aa
        usDat    = Factory $ filter (/= lp) lps
        usC      = aaveCoin aa
        psC      = poolStateCoin aa
        usVal    = coin usC 1
        psVal    = coin psC 1
        redeemer = Redeemer $ PlutusTx.toData Close

        lookups  = Constraints.scriptInstanceLookups usInst        <>
                   Constraints.otherScript usScript                <>
                   Constraints.monetaryPolicy (liquidityPolicy aa) <>
                   Constraints.ownPubKeyHash pkh                   <>
                   Constraints.unspentOutputs (Map.singleton oref1 o1 <> Map.singleton oref2 o2)

        tx       = Constraints.mustPayToTheScript usDat usVal          <>
                   Constraints.mustForgeValue (negate psVal) <>
                   Constraints.mustSpendScriptOutput oref1 redeemer    <>
                   Constraints.mustSpendScriptOutput oref2 redeemer    <>
                   Constraints.mustIncludeDatum (Datum $ PlutusTx.toData $ Pool lp liquidity)

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "closed liquidity pool: " ++ show lp

getAaveDatum :: TxOutTx -> Contract w s Text AaveDatum
getAaveDatum o = case txOutType $ txOutTxOut o of
        PayToPubKey   -> throwError "unexpected out type"
        PayToScript h -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing -> throwError "datum not found"
            Just (Datum e) -> case PlutusTx.fromData e of
                Nothing -> throwError "datum has wrong type"
                Just d  -> return d

findAaveInstance :: HasBlockchainActions s => Aave -> Coin -> (AaveDatum -> Maybe a) -> Contract w s Text (TxOutRef, TxOutTx, a)
findAaveInstance aa c f = do
    let addr = aaveAddress aa
    logInfo @String $ printf "looking for Aave instance at address %s containing coin %s " (show addr) (show c)
    utxos <- utxoAt addr
    go  [x | x@(_, o) <- Map.toList utxos, coinValueOf (txOutValue $ txOutTxOut o) c == 1]
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
findAaveFactory aa@Aave{..} = findAaveInstance aa aaveCoin $ \case
    Factory lps -> Just lps
    Pool _ _    -> Nothing

findAavePool :: HasBlockchainActions s => Aave -> LendingPool -> Contract w s Text (TxOutRef, TxOutTx, Integer)
findAavePool aa lp = findAaveInstance aa (poolStateCoin aa) $ \case
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

-- | Schema for the endpoints for users of Aave.
type AaveUserSchema =
    BlockchainActions
        .\/ Endpoint "create" Integer
        .\/ Endpoint "close"  Coin
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
