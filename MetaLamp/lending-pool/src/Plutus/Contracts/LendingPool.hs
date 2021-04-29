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
    , poolStateCoinFromAaveCurrency, liquidityCoin
    , CreateParams (..)
    , CloseParams (..)
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

data LiquidityPool = LiquidityPool
    { lpCoinA :: Coin
    , lpCoinB :: Coin
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''LiquidityPool
PlutusTx.makeLift ''LiquidityPool

instance Eq LiquidityPool where
    {-# INLINABLE (==) #-}
    x == y = (lpCoinA x == lpCoinA y && lpCoinB x == lpCoinB y) ||
             (lpCoinA x == lpCoinB y && lpCoinB x == lpCoinA y)

{-# INLINABLE hashLiquidityPool #-}
hashLiquidityPool :: LiquidityPool -> ByteString
hashLiquidityPool LiquidityPool{..} = sha2_256 $ concatenate (hashCoin c) (hashCoin d)
  where
    (c, d)
        | lpCoinA `coinLT` lpCoinB = (lpCoinA, lpCoinB)
        | otherwise                = (lpCoinB, lpCoinA)

newtype Aave = Aave
    { aaveCoin :: Coin
    } deriving stock    (Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Aave

instance Prelude.Eq Aave where
    u == v = aaveCoin u Prelude.== aaveCoin v

instance Prelude.Ord Aave where
    compare u v = Prelude.compare (aaveCoin u) (aaveCoin v)

data AaveAction = Create LiquidityPool | Close
    deriving Show

PlutusTx.unstableMakeIsData ''AaveAction
PlutusTx.makeLift ''AaveAction

data AaveDatum =
      Factory [LiquidityPool]
    | Pool LiquidityPool Integer
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
               -> [LiquidityPool]
               -> LiquidityPool
               -> ValidatorCtx
               -> Bool
validateCreate Aave{..} c lps lp@LiquidityPool{..} ctx =
    traceIfFalse "Aave coin not present" (coinValueOf (txInInfoValue $ findOwnInput ctx) aaveCoin == 1) &&
    (lpCoinA /= lpCoinB)                                                                                 &&
    all (/= lp) lps                                                                                      &&
    Constraints.checkOwnOutputConstraint ctx (OutputConstraint (Factory $ lp : lps) $ coin aaveCoin 1)     &&
    (coinValueOf forged c == 1)                                                                          &&
    (coinValueOf forged liquidityCoin' == liquidity)                                                      &&
    (outA > 0)                                                                                           &&
    (outB > 0)                                                                                           &&
    Constraints.checkOwnOutputConstraint ctx (OutputConstraint (Pool lp liquidity) $
        coin lpCoinA outA <> coin lpCoinB outB <> coin c 1)
  where
    poolOutput :: TxOutInfo
    poolOutput = case [o | o <- getContinuingOutputs ctx, coinValueOf (txOutValue o) c == 1] of
        [o] -> o
        _   -> traceError "expected exactly one pool output"

    outA, outB, liquidity :: Integer
    outA      = coinValueOf (txOutValue poolOutput) lpCoinA
    outB      = coinValueOf (txOutValue poolOutput) lpCoinB
    liquidity = calculateInitialLiquidity outA outB

    forged :: Value
    forged = txInfoForge $ valCtxTxInfo ctx

    liquidityCoin' :: Coin
    liquidityCoin' = let Coin cs _ = c in Coin cs $ lpTicker lp

{-# INLINABLE validateCloseFactory #-}
validateCloseFactory :: Aave -> Coin -> [LiquidityPool] -> ValidatorCtx -> Bool
validateCloseFactory aa c lps ctx =
    traceIfFalse "Aave coin not present" (coinValueOf (txInInfoValue $ findOwnInput ctx) usC == 1)             &&
    traceIfFalse "wrong forge value"        (txInfoForge info == negate (coin c 1 <>  coin lC (snd lpLiquidity))) &&
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

    lpLiquidity :: (LiquidityPool, Integer)
    lpLiquidity = case txInInfoWitness poolInput of
        Nothing        -> traceError "pool input witness missing"
        Just (_, _, h) -> findPoolDatum info h

    lC, usC :: Coin
    lC  = Coin (cCurrency c) (lpTicker $ fst lpLiquidity)
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
findPoolDatum :: TxInfo -> DatumHash -> (LiquidityPool, Integer)
findPoolDatum info h = case findDatum h info of
    Just (Datum d) -> case PlutusTx.fromData d of
        Just (Pool lp a) -> (lp, a)
        _                -> traceError "error decoding data"
    _              -> traceError "pool input datum not found"

{-# INLINABLE lpTicker #-}
lpTicker :: LiquidityPool -> TokenName
--lpTicker = TokenName . hashLiquidityPool
lpTicker LiquidityPool{..} = TokenName $
    unCurrencySymbol (cCurrency c) `concatenate`
    unCurrencySymbol (cCurrency d) `concatenate`
    unTokenName      (cToken    c) `concatenate`
    unTokenName      (cToken    d)
  where
    (c, d)
        | lpCoinA `coinLT` lpCoinB = (lpCoinA, lpCoinB)
        | otherwise                = (lpCoinB, lpCoinA)

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

-- | Gets the liquidity token for a given liquidity pool.
liquidityCoin :: CurrencySymbol -- ^ The currency identifying the Aave instance.
              -> Coin           -- ^ One coin in the liquidity pair.
              -> Coin           -- ^ The other coin in the liquidity pair.
              -> Coin
liquidityCoin cs coinA coinB = Coin (liquidityCurrency $ aave cs) $ lpTicker $ LiquidityPool coinA coinB

-- | Paraneters for the @create@-endpoint, which creates a new liquidity pool.
data CreateParams = CreateParams
    { cpCoinA   :: Coin    -- ^ One 'Coin' of the liquidity pair.
    , cpCoinB   :: Coin    -- ^ The other 'Coin'.
    , cpAmountA :: Integer -- ^ Amount of liquidity for the first 'Coin'.
    , cpAmountB :: Integer -- ^ Amount of liquidity for the second 'Coin'.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @close@-endpoint, which closes a liquidity pool.
data CloseParams = CloseParams
    { clpCoinA :: Coin           -- ^ One 'Coin' of the liquidity pair.
    , clpCoinB :: Coin           -- ^ The other 'Coin' of the liquidity pair.
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

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
create :: HasBlockchainActions s => Aave -> CreateParams -> Contract w s Text ()
create aa CreateParams{..} = do
    when (cpCoinA == cpCoinB)               $ throwError "coins must be different"
    when (cpAmountA <= 0 || cpAmountB <= 0) $ throwError "amounts must be positive"
    (oref, o, lps) <- findAaveFactory aa
    let liquidity = calculateInitialLiquidity cpAmountA cpAmountB
        lp        = LiquidityPool {lpCoinA = cpCoinA, lpCoinB = cpCoinB}
    let usInst   = aaveInstance aa
        usScript = aaveScript aa
        usDat1   = Factory $ lp : lps
        usDat2   = Pool lp liquidity
        psC      = poolStateCoin aa
        lC       = Coin (liquidityCurrency aa) $ lpTicker lp
        usVal    = coin (aaveCoin aa) 1
        lpVal    = coin cpCoinA cpAmountA <> coin cpCoinB cpAmountB <> coin psC 1

        lookups  = Constraints.scriptInstanceLookups usInst        <>
                   Constraints.otherScript usScript                <>
                   Constraints.monetaryPolicy (liquidityPolicy aa) <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript usDat1 usVal                                               <>
                   Constraints.mustPayToTheScript usDat2 lpVal                                               <>
                   Constraints.mustForgeValue (coin psC 1 <> coin lC liquidity)                              <>
                   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Create lp)

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "created liquidity pool: " ++ show lp

-- | Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
close :: HasBlockchainActions s => Aave -> CloseParams -> Contract w s Text ()
close aa CloseParams{..} = do
    ((oref1, o1, lps), (oref2, o2, lp, liquidity)) <- findAaveFactoryAndPool aa clpCoinA clpCoinB
    pkh                                            <- pubKeyHash <$> ownPubKey
    let usInst   = aaveInstance aa
        usScript = aaveScript aa
        usDat    = Factory $ filter (/= lp) lps
        usC      = aaveCoin aa
        psC      = poolStateCoin aa
        lC       = Coin (liquidityCurrency aa) $ lpTicker lp
        usVal    = coin usC 1
        psVal    = coin psC 1
        lVal     = coin lC liquidity
        redeemer = Redeemer $ PlutusTx.toData Close

        lookups  = Constraints.scriptInstanceLookups usInst        <>
                   Constraints.otherScript usScript                <>
                   Constraints.monetaryPolicy (liquidityPolicy aa) <>
                   Constraints.ownPubKeyHash pkh                   <>
                   Constraints.unspentOutputs (Map.singleton oref1 o1 <> Map.singleton oref2 o2)

        tx       = Constraints.mustPayToTheScript usDat usVal          <>
                   Constraints.mustForgeValue (negate $ psVal <> lVal) <>
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

findAaveFactory :: HasBlockchainActions s => Aave -> Contract w s Text (TxOutRef, TxOutTx, [LiquidityPool])
findAaveFactory aa@Aave{..} = findAaveInstance aa aaveCoin $ \case
    Factory lps -> Just lps
    Pool _ _    -> Nothing

findAavePool :: HasBlockchainActions s => Aave -> LiquidityPool -> Contract w s Text (TxOutRef, TxOutTx, Integer)
findAavePool aa lp = findAaveInstance aa (poolStateCoin aa) $ \case
        Pool lp' l
            | lp == lp' -> Just l
        _               -> Nothing

findAaveFactoryAndPool :: HasBlockchainActions s
                          => Aave
                          -> Coin
                          -> Coin
                          -> Contract w s Text ( (TxOutRef, TxOutTx, [LiquidityPool])
                                               , (TxOutRef, TxOutTx, LiquidityPool, Integer)
                                               )
findAaveFactoryAndPool aa coinA coinB = do
    (oref1, o1, lps) <- findAaveFactory aa
    case [ lp'
         | lp' <- lps
         , lp' == LiquidityPool coinA coinB
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
        .\/ Endpoint "create" CreateParams
        .\/ Endpoint "close"  CloseParams
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
