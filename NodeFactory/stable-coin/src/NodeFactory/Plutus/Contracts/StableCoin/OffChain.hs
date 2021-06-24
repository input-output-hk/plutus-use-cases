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

module NodeFactory.Plutus.Contracts.StableCoin.OffChain
    ( poolStateCoinFromUniswapCurrency
    , CreateParams (..)
    , CloseParams (..)
    , StableCoinUserSchema, UserContractState (..)
    , StableCoinOwnerSchema
    , start, create, close, pools
    , ownerEndpoint, userEndpoints
    ) where

import           Control.Monad                    hiding (fmap)
import qualified Data.Map                         as Map
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void, absurd)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import qualified Ledger.Typed.Scripts.Validators  as Scripts
import           Playground.Contract
import           Plutus.Contract
import qualified Plutus.Contracts.Currency        as Currency
import           NodeFactory.Plutus.Contracts.StableCoin.OnChain (mkStableCoinValidator, validateStableCoinForging)
import           NodeFactory.Plutus.Contracts.StableCoin.Types
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), dropWhile, flip, unless)
import           Prelude                          as Haskell (Int, Semigroup (..), String, div, dropWhile, flip, show,
                                                              (^))
import           Text.Printf                      (printf)

data StableCoining
instance Scripts.ValidatorTypes StableCoining where
    type instance RedeemerType StableCoining = StableCoinAction
    type instance DatumType    StableCoining = StableCoinDatum

type StableCoinOwnerSchema = Endpoint "start" ()

-- | Schema for the endpoints for users of stable coin.
type StableCoinUserSchema =
        Endpoint "create" CreateParams
        .\/ Endpoint "close"  CloseParams
        .\/ Endpoint "funds"  ()
        .\/ Endpoint "stop"   ()

-- | Type of the StableCoin user contract state.
data UserContractState =
      Funds Value
    | Created
    | Closed
    | Stopped
    deriving (Show, Generic, FromJSON, ToJSON)


scTokenName, vaultStateTokenName :: TokenName
scTokenName = "StableCoin"
vaultStateTokenName = "Vault State"

scInstance :: StableCoin -> Scripts.TypedValidator StableCoining
scInstance sc = Scripts.mkTypedValidator @StableCoining
    ($$(PlutusTx.compile [|| mkStableCoinValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode sc
        `PlutusTx.applyCode` PlutusTx.liftCode c)
     $$(PlutusTx.compile [|| wrap ||])
  where
    c :: Coin VaultState
    c = vaultStateCoin sc

    wrap = Scripts.wrapValidator @StableCoinDatum @StableCoinAction

scScript :: StableCoin -> Validator
scScript = Scripts.validatorScript . scInstance

scAddress :: StableCoin -> Ledger.Address
scAddress = Ledger.scriptAddress . scScript

stablecoin :: CurrencySymbol -> StableCoin
stablecoin cs = StableCoin $ mkCoin cs scTokenName

stableCoinPolicy :: StableCoin -> MonetaryPolicy
stableCoinPolicy sc = mkMonetaryPolicyScript $
    $$(PlutusTx.compile [|| \u t -> Scripts.wrapMonetaryPolicy (validateStableCoinForging u t) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode sc
        `PlutusTx.applyCode` PlutusTx.liftCode vaultStateTokenName

stableCoinCurrency :: StableCoin -> CurrencySymbol
stableCoinCurrency = scriptCurrencySymbol . stableCoinPolicy

vaultStateCoin :: StableCoin -> Coin VaultState
vaultStateCoin = flip mkCoin vaultStateTokenName . stableCoinCurrency

poolStateCoinFromUniswapCurrency :: CurrencySymbol -- ^ The currency identifying the StableCoin instance.
                                 -> Coin VaultState
poolStateCoinFromUniswapCurrency = vaultStateCoin . stablecoin

-- | Parameters for the @create@-endpoint, which creates a new vault.
data CreateParams = CreateParams
    { crAmount   :: Amount
    , crOwner    :: PubKeyHash  
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @close@-endpoint, which closes a vault.
data CloseParams = CloseParams
    { clOwner :: PubKeyHash
    , clAmount :: Amount
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

start :: forall w s. Contract w s Text StableCoin
start = do
    pkh <- pubKeyHash <$> ownPubKey
    cs  <- fmap Currency.currencySymbol $
           mapError (pack . show @Currency.CurrencyError) $
           Currency.forgeContract pkh [(scTokenName, 1)]
    let c    = mkCoin cs scTokenName
        sc   = stablecoin cs
        inst = scInstance sc
        tx   = mustPayToTheScript (Factory []) $ unitValue c
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo @String $ printf "started StableCoin %s at address %s" (show sc) (show $ scAddress sc)
    return sc

-- | Creates stable coin vault
create :: forall w s. StableCoin -> CreateParams -> Contract w s Text ()
create sc CreateParams{..} = do
    Plutus.Contract.when (crAmount <= 0) $ throwError "Amount of stable coin must be positive"
    (oref, o, vs) <- findStableCoinFactory sc
    let v        = Vault {owner = crOwner, amount = crAmount}
    let scInst   = scInstance sc
        scScript = scScript sc
        scDat1   = Factory $ v : vs
        scDat2   = Vault v
        vsC      = vaultStateCoin sc
        lC       = mkCoin (stableCoinCurrency sc) $ lpTicker v
        scVal    = unitValue $ sCoin sc
        vVal    = valueOf unitValue vsC

        lookups  = Constraints.typedValidatorLookups scInst        <>
                   Constraints.otherScript scScript                <>
                   Constraints.monetaryPolicy (stableCoinPolicy sc) <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript scDat1 scVal                                     <>
                   Constraints.mustPayToTheScript scDat2 vVal                                     <>
                   Constraints.mustForgeValue (unitValue vsC <> valueOf lC liquidity)              <>
                   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Create v)

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "created liquidity pool: " ++ show v

-- | Closes a stable coin vault
close :: forall w s. StableCoin -> CloseParams -> Contract w s Text ()
close sc CloseParams{..} = do
    pkh                                            <- pubKeyHash <$> ownPubKey
    let scInst   = scInstance sc
        scScript = scScript sc
        usDat    = Factory $ filter (/= v) vs
        usC      = usCoin sc
        vsC      = vaultStateCoin sc
        lC       = mkCoin (stableCoinCurrency sc) $ lpTicker v
        scVal    = unitValue usC
        psVal    = unitValue vsC
        lVal     = valueOf lC liquidity
        redeemer = Redeemer $ PlutusTx.toData Close

        lookups  = Constraints.typedValidatorLookups scInst        <>
                   Constraints.otherScript scScript                <>
                   Constraints.monetaryPolicy (stableCoinPolicy sc) <>
                   Constraints.ownPubKeyHash pkh                   

        tx       = Constraints.mustPayToTheScript usDat scVal          <>
                   Constraints.mustForgeValue (negate $ psVal <> lVal) <>
                   Constraints.mustIncludeDatum (Datum $ PlutusTx.toData $ Vault v liquidity)

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "closed liquidity pool: " ++ show v

-- | Gets the caller's funds.
funds :: forall w s. Contract w s Text Value
funds = do
    pkh <- pubKeyHash <$> ownPubKey
    os  <- map snd . Map.toList <$> utxoAt (pubKeyHashAddress pkh)
    return $ mconcat [txOutValue $ txOutTxOut o | o <- os]

getStableCoinDatum :: TxOutTx -> Contract w s Text StableCoinDatum
getStableCoinDatum o = case txOutDatumHash $ txOutTxOut o of
        Nothing -> throwError "datumHash not found"
        Just h -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing -> throwError "datum not found"
            Just (Datum e) -> case PlutusTx.fromData e of
                Nothing -> throwError "datum has wrong type"
                Just d  -> return d

findStableCoinInstance :: forall a b w s. StableCoin -> Coin b -> (StableCoinDatum -> Maybe a) -> Contract w s Text (TxOutRef, TxOutTx, a)
findStableCoinInstance sc c f = do
    let addr = scAddress sc
    logInfo @String $ printf "looking for StableCoin instance at address %s containing coin %s " (show addr) (show c)
    utxos <- utxoAt addr
    go  [x | x@(_, o) <- Map.toList utxos, isUnity (txOutValue $ txOutTxOut o) c]
  where
    go [] = throwError "StableCoin instance not found"
    go ((oref, o) : xs) = do
        d <- getStableCoinDatum o
        case f d of
            Nothing -> go xs
            Just a  -> do
                logInfo @String $ printf "found StableCoin instance with datum: %s" (show d)
                return (oref, o, a)

findStableCoinFactory :: forall w s. StableCoin -> Contract w s Text (TxOutRef, TxOutTx, [StableCoinVault])
findStableCoinFactory sc@StableCoin{..} = findStableCoinInstance sc usCoin $ \case
    Factory vs -> Just vs
    Vault _ _    -> Nothing

findStableCoinPool :: forall w s. StableCoin -> StableCoinVault -> Contract w s Text (TxOutRef, TxOutTx)
findStableCoinPool sc v = findStableCoinInstance sc (vaultStateCoin sc) $ \case
        Vault v' l
            | v == v' -> Just l
        _               -> Nothing

ownerEndpoint :: Contract (Last (Either Text StableCoin)) EmptySchema ContractError ()
ownerEndpoint = do
    e <- mapError absurd $ runError start
    tell $ Last $ Just e
    void $ waitNSlots 10

userEndpoints :: StableCoin -> Contract (Last (Either Text UserContractState)) StableCoinUserSchema Void ()
userEndpoints sc =
    stop
        `select`
    ((f (Proxy @"create") (const Created) create                 `select`
      f (Proxy @"close")  (const Closed)  close                  `select`
      f (Proxy @"funds")  Funds           (\_us () -> funds))    >> userEndpoints sc)
  where
    f :: forall l a p.
         (HasEndpoint l p StableCoinUserSchema, FromJSON p)
      => Proxy l
      -> (a -> UserContractState)
      -> (StableCoin -> p -> Contract (Last (Either Text UserContractState)) StableCoinUserSchema Text a)
      -> Contract (Last (Either Text UserContractState)) StableCoinUserSchema Void ()
    f _ g c = do
        e <- runError $ do
            p <- endpoint @l
            c sc p
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right a  -> Right $ g a

    stop :: Contract (Last (Either Text UserContractState)) StableCoinUserSchema Void ()
    stop = do
        e <- runError $ endpoint @"stop"
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right () -> Right Stopped