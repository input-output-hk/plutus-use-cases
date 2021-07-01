{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE StandaloneDeriving         #-}
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
{-# options_ghc -fno-warn-orphans       #-}

-- | A decentralized exchange for arbitrary token pairs following the
-- [Uniswap protocol](https://uniswap.org/whitepaper.pdf).
--
module Plutus.Contracts.Uniswap
    (
      coin, coinValueOf, mkCoin
    , Uniswap (..), uniswap
    , UniswapUserSchema, UserContractState (..)
    , UniswapOwnerSchema
    , start, create, add, remove, close, swap, pools
    , ownerEndpoint, userEndpoints
    , rsqrt, isqrt
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
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     (AssetClass (..), assetClass, assetClassValue, assetClassValueOf,
                                                   symbols, unCurrencySymbol, unTokenName)
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import qualified Plutus.Contracts.Currency        as Currency
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), unless)
import           PlutusTx.Sqrt
import           Prelude                          (Semigroup (..))
import qualified Prelude                          as Haskell (Int, Semigroup(..), String, div, dropWhile, flip, show, (^))     
import           Text.Printf                      (printf)
import Plutus.Contracts.Data
-- import Plutus.Contracts.UniswapHelpers
import Plutus.Contracts.Helpers
import Plutus.Contracts.Validators
import Plutus.Contracts.LiquidityPool 
import Plutus.Contracts.PoolForgery               (create, close)


-- | Creates a Uniswap "factory". This factory will keep track of the existing liquidity pools and enforce that there will be at most one liquidity pool
-- for any pair of tokens at any given time.
-- "factory" only exists once, to keep track which pools already exist
-- it doesn't carry any value, it only keeps track of existing pools in it's DATUM
-- this factory can be identified by 1 NFT (hence the forgeContract)
start :: forall w s. Contract w s Text Uniswap
start = do
    pkh <- pubKeyHash <$> ownPubKey
    cs  <- fmap Currency.currencySymbol $
           mapError (pack . Haskell.show @Currency.CurrencyError) $
           Currency.forgeContract pkh [(uniswapTokenName, 1)]
    let c    = mkCoin cs uniswapTokenName
        us   = uniswap cs
        inst = uniswapInstance us
        tx   = mustPayToTheScript (Factory []) $ coin c 1
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo @Haskell.String $ printf "started Uniswap %s at address %s" (Haskell.show us) (Haskell.show $ uniswapAddress us)
    return us

-- | Removes some liquidity from a liquidity pool in exchange for liquidity tokens.
remove :: forall w s. Uniswap -> RemoveParams -> Contract w s Text ()
remove us RemoveParams{..} = do
    (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us rpCoinA rpCoinB
    pkh                           <- pubKeyHash <$> ownPubKey
    when (rpDiff < 1 || rpDiff >= liquidity) $ throwError "removed liquidity must be positive and less than total liquidity"
    let usInst       = uniswapInstance us
        usScript     = uniswapScript us
        dat          = Pool lp $ liquidity - rpDiff
        psC          = poolStateCoin us
        lC           = mkCoin (liquidityCurrency us) $ lpTicker lp
        psVal        = coin psC 1
        lVal         = coin lC rpDiff
        inVal        = txOutValue $ txOutTxOut o
        inA          = coinValueOf inVal rpCoinA
        inB          = coinValueOf inVal rpCoinB
        (outA, outB) = calculateRemoval inA inB liquidity rpDiff
        val          = psVal <> coin rpCoinA outA <> coin rpCoinB outB
        redeemer     = Redeemer $ PlutusTx.toData Remove

        lookups  = Constraints.typedValidatorLookups usInst          <>
                   Constraints.otherScript usScript                  <>
                   Constraints.mintingPolicy (liquidityPolicy us)   <>
                   Constraints.unspentOutputs (Map.singleton oref o) <>
                   Constraints.ownPubKeyHash pkh

        tx       = Constraints.mustPayToTheScript dat val          <>
                   Constraints.mustMintValue (negate lVal)        <>
                   Constraints.mustSpendScriptOutput oref redeemer

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "removed liquidity from pool: " ++ Haskell.show lp

-- | Adds some liquidity to an existing liquidity pool in exchange for newly minted liquidity tokens.
add :: forall w s. Uniswap -> AddParams -> Contract w s Text ()
add us AddParams{..} = do
    pkh                           <- pubKeyHash <$> ownPubKey
    (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us apCoinA apCoinB
    when (apAmountA < 0 || apAmountB < 0) $ throwError "amounts must not be negative"
    let outVal = txOutValue $ txOutTxOut o
        oldA   = coinValueOf outVal apCoinA
        oldB   = coinValueOf outVal apCoinB
        newA   = oldA + apAmountA
        newB   = oldB + apAmountB
        delL   = calculateAdditionalLiquidity oldA oldB liquidity apAmountA apAmountB
        inVal  = coin apCoinA apAmountA <> coin apCoinB apAmountB
    when (delL <= 0) $ throwError "insufficient liquidity"
    logInfo @Haskell.String $ printf "oldA = %d, oldB = %d, newA = %d, newB = %d, delL = %d" oldA oldB newA newB delL

    let usInst       = uniswapInstance us
        usScript     = uniswapScript us
        dat          = Pool lp $ liquidity + delL
        psC          = poolStateCoin us
        lC           = mkCoin (liquidityCurrency us) $ lpTicker lp
        psVal        = coin psC 1
        lVal         = coin lC delL
        val          = psVal <> coin apCoinA newA <> coin apCoinB newB
        redeemer     = Redeemer $ PlutusTx.toData Add

        lookups  = Constraints.typedValidatorLookups usInst             <>
                   Constraints.otherScript usScript                     <>
                   Constraints.mintingPolicy (liquidityPolicy us)      <>
                   Constraints.ownPubKeyHash pkh                        <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript dat val          <>
                   Constraints.mustMintValue lVal                 <>
                   Constraints.mustSpendScriptOutput oref redeemer

    logInfo @Haskell.String $ printf "val = %s, inVal = %s" (Haskell.show val) (Haskell.show inVal)
    logInfo $ Haskell.show lookups
    logInfo $ Haskell.show tx

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "added liquidity to pool: " ++ Haskell.show lp

-- | Uses a liquidity pool two swap one sort of coins in the pool against the other.
swap :: forall w s. Uniswap -> SwapParams -> Contract w s Text ()
swap us SwapParams{..} = do
    unless (spAmountA > 0 && spAmountB == 0 || spAmountA == 0 && spAmountB > 0) $ throwError "exactly one amount must be positive"
    (_, (oref, o, lp, liquidity)) <- findUniswapFactoryAndPool us spCoinA spCoinB
    let outVal = txOutValue $ txOutTxOut o
    let oldA = coinValueOf outVal spCoinA
        oldB = coinValueOf outVal spCoinB
    (newA, newB) <- if spAmountA > 0 then do
        let outB = findSwapA oldA oldB spAmountA
        when (outB == 0) $ throwError "no payout"
        return (oldA + spAmountA, oldB - outB)
                                     else do
        let outA = findSwapB oldA oldB spAmountB
        when (outA == 0) $ throwError "no payout"
        return (oldA - outA, oldB + spAmountB)
    pkh <- pubKeyHash <$> ownPubKey

    logInfo @Haskell.String $ printf "oldA = %d, oldB = %d, old product = %d, newA = %d, newB = %d, new product = %d" oldA oldB (oldA * oldB) newA newB (newA * newB)

    let inst    = uniswapInstance us
        val     = coin spCoinA newA <> coin spCoinB newB <> coin (poolStateCoin us) 1

        lookups = Constraints.typedValidatorLookups inst                 <>
                  Constraints.otherScript (Scripts.validatorScript inst) <>
                  Constraints.unspentOutputs (Map.singleton oref o)      <>
                  Constraints.ownPubKeyHash pkh

        tx      = mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData Swap) <>
                  Constraints.mustPayToTheScript (Pool lp liquidity) val

    logInfo $ Haskell.show tx
    ledgerTx <- submitTxConstraintsWith lookups tx
    logInfo $ Haskell.show ledgerTx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "swapped with: " ++ Haskell.show lp

-- | Finds all liquidity pools and their liquidity belonging to the Uniswap instance.
-- This merely inspects the blockchain and does not issue any transactions.
pools :: forall w s. Uniswap -> Contract w s Text [((Coin, Integer), (Coin, Integer))]
pools us = do
    utxos <- utxoAt (uniswapAddress us)
    go $ snd <$> Map.toList utxos
  where
    go :: [TxOutTx] -> Contract w s Text [((Coin, Integer), (Coin, Integer))]
    go []       = return []
    go (o : os) = do
        let v = txOutValue $ txOutTxOut o
        if coinValueOf v c == 1
            then do
                d <- getUniswapDatum o
                case d of
                    Factory _ -> go os
                    Pool lp _ -> do
                        let coinA = lpCoinA lp
                            coinB = lpCoinB lp
                            amtA  = coinValueOf v coinA
                            amtB  = coinValueOf v coinB
                            s     = ((coinA, amtA), (coinB, amtB))
                        logInfo $ "found pool: " ++ Haskell.show s
                        ss <- go os
                        return $ s : ss
            else go os
      where
        c :: Coin
        c = poolStateCoin us

ownerEndpoint :: Contract (Last (Either Text Uniswap)) EmptySchema Void ()
ownerEndpoint = do
    e <- runError start
    tell $ Last $ Just $ case e of
        Left err -> Left err
        Right us -> Right us

type UniswapOwnerSchema =
    Endpoint "start" ()

-- | Schema for the endpoints for users of Uniswap.
type UniswapUserSchema =
        Endpoint "create" CreateParams
        .\/ Endpoint "swap"   SwapParams
        .\/ Endpoint "close"  CloseParams
        .\/ Endpoint "remove" RemoveParams
        .\/ Endpoint "add"    AddParams
        .\/ Endpoint "pools"  ()
        .\/ Endpoint "funds"  ()
        .\/ Endpoint "stop"   ()

-- | Provides the following endpoints for users of a Uniswap instance:
--
--      [@create@]: Creates a liquidity pool for a pair of coins. The creator provides liquidity for both coins and gets liquidity tokens in return.
--      [@swap@]: Uses a liquidity pool two swap one sort of coins in the pool against the other.
--      [@close@]: Closes a liquidity pool by burning all remaining liquidity tokens in exchange for all liquidity remaining in the pool.
--      [@remove@]: Removes some liquidity from a liquidity pool in exchange for liquidity tokens.
--      [@add@]: Adds some liquidity to an existing liquidity pool in exchange for newly minted liquidity tokens.
--      [@pools@]: Finds all liquidity pools and their liquidity belonging to the Uniswap instance. This merely inspects the blockchain and does not issue any transactions.
--      [@funds@]: Gets the caller's funds. This merely inspects the blockchain and does not issue any transactions.
--      [@stop@]: Stops the contract.
userEndpoints :: Uniswap -> Contract (Last (Either Text UserContractState)) UniswapUserSchema Void ()
userEndpoints us =
    stop
        `select`
    ((f (Proxy @"create") (const Created) create                 `select`
      f (Proxy @"swap")   (const Swapped) swap                   `select`
      f (Proxy @"close")  (const Closed)  close                  `select`
      f (Proxy @"remove") (const Removed) remove                 `select`
      f (Proxy @"add")    (const Added)   add                    `select`
      f (Proxy @"pools")  Pools           (\us' () -> pools us') `select`
      f (Proxy @"funds")  Funds           (\_us () -> funds))    >> userEndpoints us)
  where
    f :: forall l a p.
        (HasEndpoint l p UniswapUserSchema,  FromJSON p)
      => Proxy l
      -> (a -> UserContractState)
      -> (Uniswap -> p -> Contract (Last (Either Text UserContractState)) UniswapUserSchema Text a)
      -> Contract (Last (Either Text UserContractState)) UniswapUserSchema Void ()
    f _ g c = do
        e <- runError $ do
            p <- endpoint @l
            c us p
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right a  -> Right $ g a

    stop :: Contract (Last (Either Text UserContractState)) UniswapUserSchema Void ()
    stop = do
        e <- runError $ endpoint @"stop"
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right () -> Right Stopped
