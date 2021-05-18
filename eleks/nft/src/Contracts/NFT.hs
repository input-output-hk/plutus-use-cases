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
{-# options_ghc -fno-warn-orphans       #-}
-- | Implements a custom currency with a monetary policy that allows
--   the forging of a fixed amount of units.
module Contracts.NFT
    ( NFTMarket (..)
    , CreateParams (..)
    , MarketUserSchema, MarketContractState (..)
    , MarketOwnerSchema
    , start, create
    , ownerEndpoint, userEndpoints
    ) where

import           Control.Monad                    hiding (fmap)
import qualified Data.Map                         as Map
import qualified Data.ByteString.Char8        as C
import qualified Data.Text               as T
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Typed.Scripts             as Scripts
import           Ledger.Value                     (AssetClass (..), assetClass, assetClassValue, assetClassValueOf, valueOf,
                                                    symbols, unCurrencySymbol, unTokenName)
import qualified Ledger.Value                     as Value
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import qualified Plutus.Contracts.Currency        as Currency
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), unless)
import           PlutusTx.Sqrt
import           Prelude                          (Semigroup (..))
import qualified Prelude
import           Text.Printf                      (printf)

marketplaceTokenName :: TokenName
marketplaceTokenName = "NFTMarketplace"
metadataTokenName = "NFTMetadata"

-- Note: An orphan instance here because of the alias above.
deriving anyclass instance ToSchema AssetClass

data NFTMetadata = NFTMetadata
    { 
      nftTokenName:: TokenName
    , nftMetaTokenName:: TokenName
    , nftMetaDescription:: ByteString
    , nftTokenSymbol :: CurrencySymbol
    , nftMetadataTokenSymbol :: CurrencySymbol
    }
    deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq NFTMetadata where
    {-# INLINABLE (==) #-}
    x == y = nftTokenSymbol x PlutusTx.Prelude.== nftTokenSymbol y

PlutusTx.makeIsDataIndexed ''NFTMetadata [('NFTMetadata, 0)]
PlutusTx.makeLift ''NFTMetadata

newtype NFTMarket = NFTMarket
    { marketId :: AssetClass
    } deriving stock    (Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)
      deriving newtype  (Prelude.Eq, Prelude.Ord)


PlutusTx.makeLift ''NFTMarket
    
data NFTMarketAction = Create NFTMetadata | Swap
    deriving Show

PlutusTx.makeIsDataIndexed ''NFTMarketAction [ ('Create , 0)
                                           , ('Swap,   1)
                                           ]
PlutusTx.makeLift ''NFTMarketAction

{-# INLINABLE findOwnInput' #-}
findOwnInput' :: ScriptContext -> TxInInfo
findOwnInput' ctx = fromMaybe (error ()) (findOwnInput ctx)

{-# INLINABLE valueWithin #-}
valueWithin :: TxInInfo -> Value
valueWithin = txOutValue . txInInfoResolved

{-# INLINABLE findMarketDatum #-}
findMarketDatum :: TxInfo -> DatumHash -> NFTMetadata
findMarketDatum info h = case findDatum h info of
    Just (Datum d) -> case PlutusTx.fromData d of
        Just (NFTMeta nft) -> nft
        _                -> traceError "error decoding data"
    _              -> traceError "market input datum not found"
    
data NFTMarketDatum =
      Factory [NFTMetadata]
    | NFTMeta NFTMetadata
    deriving stock (Show)

PlutusTx.unstableMakeIsData ''NFTMarketDatum
PlutusTx.makeLift ''NFTMarketDatum

data Market
instance Scripts.ScriptType Market where
    type instance RedeemerType Market = NFTMarketAction
    type instance DatumType Market = NFTMarketDatum
{-# INLINABLE validateCreate #-}
validateCreate :: NFTMarket
               -> [NFTMetadata]
               -> NFTMetadata
               -> ScriptContext
               -> Bool
validateCreate NFTMarket{..} nftMetas nftMeta@NFTMetadata{..} ctx =
    traceIfFalse "Marketplace not present" (assetClassValueOf (valueWithin $ findOwnInput' ctx) marketId == 1) &&
    all (/= nftMeta) nftMetas &&                                                                                 
    Constraints.checkOwnOutputConstraint ctx (OutputConstraint (Factory $ nftMeta : nftMetas) $ assetClassValue marketId 1) &&
    Constraints.checkOwnOutputConstraint ctx (OutputConstraint (NFTMeta nftMeta) $ assetClassValue (assetClass nftMetadataTokenSymbol nftMetaTokenName) 1)
  where
    marketOutput :: TxOut
    marketOutput = case [o | o <- getContinuingOutputs ctx, assetClassValueOf (txOutValue o) marketId == 1] of
        [o] -> o
        _   -> traceError "expected exactly one market output"

mkNFTMarketValidator :: NFTMarket
      -> NFTMarketDatum
      -> NFTMarketAction
      -> ScriptContext
      -> Bool
mkNFTMarketValidator market (Factory nfts) (Create nft) ctx = validateCreate market nfts nft ctx
mkNFTMarketValidator _      _              _            _   = False
 

marketInstance :: NFTMarket -> Scripts.ScriptInstance Market
marketInstance market = Scripts.validator @Market
    ($$(PlutusTx.compile [|| mkNFTMarketValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode market)
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @NFTMarketDatum @NFTMarketAction

marketScript :: NFTMarket -> Validator
marketScript = Scripts.validatorScript . marketInstance

marketAddress :: NFTMarket -> Ledger.Address
marketAddress = Ledger.scriptAddress . marketScript

-- | Parameters for the @create@-endpoint, which creates a new NFT.
data CreateParams = CreateParams
    { cpTokenName   :: TokenName    -- ^ NFT name
    , cpDescription   :: !ByteString    -- ^ metadata
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Creates a Marketplace "factory". This factory will keep track of the existing nft tokens
start :: HasBlockchainActions s => Contract w s Text NFTMarket
start = do
    pkh <- pubKeyHash <$> ownPubKey
    cs  <- fmap Currency.currencySymbol $
           mapError (pack . show @Currency.CurrencyError) $
           Currency.forgeContract pkh [(marketplaceTokenName, 1)]
    let c    = assetClass cs marketplaceTokenName
        market   = marketplace cs
        inst = marketInstance market
        tx   = mustPayToTheScript (Factory []) $ assetClassValue c 1
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo @String $ printf "started Market %s at address %s" (show market) (show $ marketAddress market)
    return market

-- | Creates a liquidity pool for a pair of coins. The creator provides liquidity for both coins and gets liquidity tokens in return.
create :: HasBlockchainActions s => NFTMarket -> CreateParams -> Contract w s Text ()
create market CreateParams{..} = do
    (oref, o, nftMetas) <- findNFTMarketFactory market

    ownPK <- pubKeyHash <$> ownPubKey
    nftTokenSymbol  <- fmap Currency.currencySymbol $
        mapError (pack . show @Currency.CurrencyError) $
        Currency.forgeContract ownPK [(cpTokenName, 1)]
    let nftTokenAssetClass = assetClass nftTokenSymbol cpTokenName
        nftValue = assetClassValue nftTokenAssetClass 1
 
    let metadataTokenName = TokenName $ C.pack $ read (show cpTokenName) ++ "Metadata"
    nftTokenMetadataSymbol <- fmap Currency.currencySymbol $
        mapError (pack . show @Currency.CurrencyError) $
        Currency.forgeContract ownPK [( metadataTokenName, 1)]
    let nftTokenMetadataAssetClass = assetClass nftTokenMetadataSymbol metadataTokenName
        nftMetadataVal    = assetClassValue nftTokenMetadataAssetClass 1
        nftMetadata       = NFTMetadata {
            nftTokenName = cpTokenName, 
            nftMetaTokenName = metadataTokenName,
            nftMetaDescription = cpDescription, 
            nftTokenSymbol = nftTokenSymbol,
            nftMetadataTokenSymbol = nftTokenMetadataSymbol }

    let marketInst = marketInstance market
        mrScript = marketScript market
        marketFactoryData   = Factory $ nftMetadata : nftMetas
        nftMetadataData   = NFTMeta nftMetadata
        marketVal    = assetClassValue (marketId market) 1
   
        lookups  = Constraints.scriptInstanceLookups marketInst        <>
                   Constraints.otherScript mrScript                <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript marketFactoryData marketVal                                  <>
                   Constraints.mustPayToTheScript nftMetadataData nftMetadataVal                                   <>
                   -- Constraints.mustPayToTheScript () nftValue                                                  <>
                   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Create nftMetadata)    <>
                   Constraints.mustPayToPubKey ownPK nftValue

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo $ "created NFT: " ++ show nftMetadata

    
marketplace :: CurrencySymbol -> NFTMarket
marketplace cs = NFTMarket $ assetClass cs marketplaceTokenName

getNFTMarketDatum :: TxOutTx -> Contract w s Text NFTMarketDatum
getNFTMarketDatum o = case txOutDatumHash $ txOutTxOut o of
        Nothing -> throwError "datumHash not found"
        Just h -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing -> throwError "datum not found"
            Just (Datum e) -> case PlutusTx.fromData e of
                Nothing -> throwError "datum has wrong type"
                Just d  -> return d

findNFTMartketInstance :: HasBlockchainActions s => NFTMarket -> AssetClass -> (NFTMarketDatum -> Maybe a) -> Contract w s Text (TxOutRef, TxOutTx, a)
findNFTMartketInstance market asset f = do
    let addr = marketAddress market
    logInfo @String $ printf "looking for NFTMarket instance at address %s containing asset %s " (show addr) (show asset)
    utxos <- utxoAt addr
    go  [x | x@(_, o) <- Map.toList utxos, assetClassValueOf (txOutValue $ txOutTxOut o) asset == 1]
  where
    go [] = throwError "NFTMarket instance not found"
    go ((oref, o) : xs) = do
        d <- getNFTMarketDatum o
        case f d of
            Nothing -> go xs
            Just a  -> do
                logInfo @String $ printf "found NFTMarket instance with datum: %s" (show d)
                return (oref, o, a)


findNFTMarketFactory :: HasBlockchainActions s => NFTMarket -> Contract w s Text (TxOutRef, TxOutTx, [NFTMetadata])
findNFTMarketFactory nftm@NFTMarket{..} = findNFTMartketInstance nftm marketId $ \case
    Factory nfts -> Just nfts
    NFTMeta _    -> Nothing

--
-- | Gets the caller's funds.
funds :: HasBlockchainActions s => Contract w s Text Value
funds = do
    pkh <- pubKeyHash <$> ownPubKey
    os  <- map snd . Map.toList <$> utxoAt (pubKeyHashAddress pkh)
    return $ mconcat [txOutValue $ txOutTxOut o | o <- os]
 
-- | Gets the caller's NFTs.
userNftTokens :: HasBlockchainActions s => NFTMarket -> Contract w s Text Value
userNftTokens market = do
    pkh <- pubKeyHash <$> ownPubKey
    (oref, o, nftMetas) <- findNFTMarketFactory market
    let marketNftSymbols =  nftMetas
    os  <- map snd . Map.toList <$> utxoAt (pubKeyHashAddress pkh)
    let values = mconcat [txOutValue $ txOutTxOut o | o <- os]
    let nftValues = [ Value.singleton (nftTokenSymbol meta) (nftTokenName meta) 1 | meta <- nftMetas, valueOf values (nftTokenSymbol meta) (nftTokenName meta) == 1 ]
    return $ fold nftValues

ownerEndpoint :: Contract (Last (Either Text NFTMarket)) BlockchainActions Void ()
ownerEndpoint = do
    e <- runError start
    tell $ Last $ Just $ case e of
        Left err -> Left err
        Right market -> Right market

type MarketOwnerSchema =
    BlockchainActions
        .\/ Endpoint "start" ()

        -- | Schema for the endpoints for users of Uniswap.
type MarketUserSchema =
    BlockchainActions
        .\/ Endpoint "create" CreateParams
        .\/ Endpoint "funds"  ()
        .\/ Endpoint "userNftTokens"  ()
        .\/ Endpoint "stop"   ()
-- | Type of the Uniswap user contract state.


data MarketContractState =
      Created
    | Funds Value
    | Tokens Value
    | Stopped
    deriving (Show, Generic, FromJSON, ToJSON)
-- | Provides the following endpoints for users of a NFT marketplace instance:
--
-- [@create@]: Creates an nft token. The creator provides liquidity for both coins and gets liquidity tokens in return.
userEndpoints :: NFTMarket -> Contract (Last (Either Text MarketContractState)) MarketUserSchema Void ()
userEndpoints market =
    stop
        `select`
    ((f (Proxy @"create") (const Created) create                 `select`
      f (Proxy @"userNftTokens") Tokens (\market'' () -> userNftTokens market'')                `select`
      f (Proxy @"funds")  Funds           (\market' () -> funds))    >> userEndpoints market)
  where
    f :: forall l a p.
         HasEndpoint l p MarketUserSchema
      => Proxy l
      -> (a -> MarketContractState)
      -> (NFTMarket -> p -> Contract (Last (Either Text MarketContractState)) MarketUserSchema Text a)
      -> Contract (Last (Either Text MarketContractState)) MarketUserSchema Void ()
    f _ g c = do
        e <- runError $ do
            p <- endpoint @l
            c market p
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right a  -> Right $ g a

    stop :: Contract (Last (Either Text MarketContractState)) MarketUserSchema Void ()
    stop = do
        e <- runError $ endpoint @"stop"
        tell $ Last $ Just $ case e of
            Left err -> Left err
            Right () -> Right Stopped