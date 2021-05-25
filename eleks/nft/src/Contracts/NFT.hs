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
{-# LANGUAGE NamedFieldPuns             #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# options_ghc -fno-warn-orphans       #-}
-- | Implements a custom currency with a monetary policy that allows
--   the forging of a fixed amount of units.
module Contracts.NFT
    ( NFTMarket (..)
    , NFTMetadataDto (..)
    , CreateParams (..)
    , SellParams (..)
    , BuyParams (..)
    , MarketUserSchema, MarketContractState (..)
    , MarketOwnerSchema
    , start, create
    , ownerEndpoint, userEndpoints
    ) where

import           Control.Monad                    hiding (fmap)
import qualified Data.Map                         as Map
import qualified Data.ByteString.Char8            as B
import qualified Data.Text                        as T
import qualified Ledger.Ada                       as Ada
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
                                                    symbols, unCurrencySymbol, unTokenName, CurrencySymbol (..))
import qualified Ledger.Value                     as Value
import qualified Ledger.Contexts                  as Validation
import           Playground.Contract
import           Plutus.Contract                  hiding (when)
import qualified Plutus.Contracts.Currency        as Currency
import qualified PlutusTx
import           PlutusTx.Prelude                 hiding (Semigroup (..), unless)
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
    , nftMetaAuthor:: ByteString
    , nftMetaFile:: ByteString
    , nftTokenSymbol :: CurrencySymbol
    , nftMetaTokenSymbol :: CurrencySymbol
    , nftSeller :: Maybe PubKeyHash
    , nftSellPrice:: Integer
    }
    deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

instance Eq NFTMetadata where
    {-# INLINABLE (==) #-}
    x == y = (nftTokenSymbol x PlutusTx.Prelude.== nftTokenSymbol y) && 
             (nftMetaTokenSymbol x PlutusTx.Prelude.== nftMetaTokenSymbol y)

PlutusTx.makeIsDataIndexed ''NFTMetadata [('NFTMetadata, 0)]
PlutusTx.makeLift ''NFTMetadata

newtype NFTMarket = NFTMarket
    { marketId :: AssetClass
    } deriving stock    (Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)
      deriving newtype  (Prelude.Eq, Prelude.Ord)


PlutusTx.makeLift ''NFTMarket
    
data NFTMarketAction = Create NFTMetadata | Sell | Buy
    deriving Show

PlutusTx.makeIsDataIndexed ''NFTMarketAction [ ('Create , 0)
                                           , ('Sell,   1)
                                           , ('Buy,   2)
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
    Constraints.checkOwnOutputConstraint ctx (OutputConstraint (NFTMeta nftMeta) $ assetClassValue (assetClass nftMetaTokenSymbol nftMetaTokenName) 1)
  where
    marketOutput :: TxOut
    marketOutput = case [o | o <- getContinuingOutputs ctx, assetClassValueOf (txOutValue o) marketId == 1] of
        [o] -> o
        _   -> traceError "expected exactly one market output"

{-# INLINABLE validateSell #-}
validateSell :: NFTMarket
               -> NFTMetadata
               -> ScriptContext
               -> Bool
validateSell NFTMarket{..} nftMeta@NFTMetadata{nftMetaTokenSymbol, nftMetaTokenName, nftTokenSymbol, nftTokenName} ctx =
    traceIfFalse "owner should sign" ownerSigned &&
    traceIfFalse "nft metadata token missing from input"          (valueOf inVal nftMetaTokenSymbol nftMetaTokenName == 1)  &&
    -- traceIfFalse "nft token missing from input"          (valueOf inVal nftTokenSymbol nftTokenName == 1)                           &&
    traceIfFalse "ouptut nftMetadata should be same" (nftMeta == outDatum)     &&
    traceIfFalse "price should be grater 0" (nftSellPrice outDatum > 0)                                      
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxInInfo
    ownInput = findOwnInput' ctx

    ownOutput :: TxOut
    ownOutput = case [ o
                     | o <- getContinuingOutputs ctx
                     , valueOf (txOutValue o) nftMetaTokenSymbol nftMetaTokenName == 1 &&
                       valueOf (txOutValue o) nftTokenSymbol nftTokenName == 1
                     ] of
        [o] -> o
        _   -> traceError "expected exactly one nft metadata output and one nft token output"

    outDatum :: (NFTMetadata)
    outDatum = case txOutDatum ownOutput of
        Nothing -> traceError "nft metadata not found"
        Just h  -> findMarketDatum info h

    inVal, outVal :: Value
    inVal  = valueWithin ownInput
    outVal = txOutValue ownOutput

    ownerSigned :: Bool
    ownerSigned = case nftSeller outDatum of
        Nothing      -> False
        Just pkh -> txSignedBy info pkh

{-# INLINABLE validateBuy #-}
validateBuy :: NFTMarket
               -> NFTMetadata
               -> ScriptContext
               -> Bool
validateBuy NFTMarket{..} nftMeta@NFTMetadata{nftMetaTokenSymbol, nftMetaTokenName, nftTokenSymbol, nftTokenName} ctx =
    traceIfFalse "nft metadata token missing from input" (valueOf inVal nftMetaTokenSymbol nftMetaTokenName == 1)  &&
    traceIfFalse "ouptut nftMetadata should be same" (nftMeta == outDatum) -- &&
  --  traceIfFalse "expected seller to get money" (getsValue (nftSeller outDatum) $ Ada.lovelaceValueOf (nftSellPrice outDatum)) -- &&   
  --  traceIfFalse "expected buyer to get nft token" (getsValue (nftSeller outDatum) $ Value.singleton nftTokenSymbol nftTokenName 1)  
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxInInfo
    ownInput = findOwnInput' ctx

    ownOutput :: TxOut
    ownOutput = case [ o
                     | o <- getContinuingOutputs ctx
                     , valueOf (txOutValue o) nftMetaTokenSymbol nftMetaTokenName == 1
                     ] of
        [o] -> o
        _   -> traceError "expected exactly one nft metadata output"

    outDatum :: (NFTMetadata)
    outDatum = case txOutDatum ownOutput of
        Nothing -> traceError "nft metadata not found"
        Just h  -> findMarketDatum info h

    inVal, outVal :: Value
    inVal  = valueWithin ownInput
    outVal = txOutValue ownOutput

    getsValue :: Maybe PubKeyHash -> Value -> Bool
    getsValue h v =
        let
        [o] = [ o'
                | o' <- txInfoOutputs info
                , txOutValue o' == v
                ]
        in
        fromMaybe False $ (==) <$> h <*> Validation.pubKeyOutput o

mkNFTMarketValidator :: NFTMarket
      -> NFTMarketDatum
      -> NFTMarketAction
      -> ScriptContext
      -> Bool
mkNFTMarketValidator market (Factory nftMetas) (Create nftMeta) ctx = validateCreate market nftMetas nftMeta ctx
mkNFTMarketValidator market (NFTMeta nftMeta)  Sell             ctx = validateSell market nftMeta ctx
mkNFTMarketValidator market (NFTMeta nftMeta)  Buy              ctx = validateBuy market nftMeta ctx
mkNFTMarketValidator _      _                  _                _   = False
 

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
    { cpTokenName   :: !String  -- ^ NFT name
    , cpDescription :: !String  -- ^ NFT description
    , cpAuthor      :: !String  -- ^ NFT author
    , cpFile        :: !String  -- ^ NFT file path
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @sell@-endpoint, which creates a new NFT.
data SellParams = SellParams
    { spTokenSymbol   :: String
    , spSellPrice :: Integer
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @buy@-endpoint, which creates a new NFT.
data BuyParams = BuyParams
    { bpTokenSymbol   :: String
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data NFTMetadataDto = NFTMetadataDto
    { nftDtoTokenName:: String
    , nftDtoMetaDescription:: String
    , nftDtoMetaAuthor:: String
    , nftDtoMetaFile:: String
    , nftDtoTokenSymbol :: String
    , nftDtoSeller :: Maybe ByteString
    , nftDtoSellPrice:: Integer
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

nftMetadataToDto:: NFTMetadata -> NFTMetadataDto
nftMetadataToDto nftMeta = NFTMetadataDto 
    { nftDtoTokenName = read.show $ nftTokenName nftMeta
    , nftDtoMetaDescription = B.unpack $ nftMetaDescription nftMeta
    , nftDtoMetaAuthor = B.unpack $ nftMetaAuthor nftMeta
    , nftDtoMetaFile = B.unpack $ nftMetaFile nftMeta
    , nftDtoTokenSymbol = B.unpack $ unCurrencySymbol $ nftTokenSymbol nftMeta
    , nftDtoSeller = getPubKeyHash <$> nftSeller nftMeta
    , nftDtoSellPrice = nftSellPrice nftMeta
    }
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

-- | Creates an NFT token
create :: HasBlockchainActions s => NFTMarket -> CreateParams -> Contract w s Text NFTMetadataDto
create market CreateParams{..} = do
    (oref, o, nftMetas) <- findNFTMarketFactory market
    let tokenName = TokenName $ B.pack cpTokenName
    ownPK <- pubKeyHash <$> ownPubKey
    nftTokenSymbol  <- fmap Currency.currencySymbol $
        mapError (pack . show @Currency.CurrencyError) $
        Currency.forgeContract ownPK [(tokenName, 1)]
    let nftTokenAssetClass = assetClass nftTokenSymbol tokenName
        nftValue = assetClassValue nftTokenAssetClass 1
 
    let metadataTokenName = TokenName $ B.pack $ read (show tokenName) ++ "Metadata"
    tokenMetadataSymbol <- fmap Currency.currencySymbol $
        mapError (pack . show @Currency.CurrencyError) $
        Currency.forgeContract ownPK [( metadataTokenName, 1)]
    let nftTokenMetadataAssetClass = assetClass tokenMetadataSymbol metadataTokenName
        nftMetadataVal    = assetClassValue nftTokenMetadataAssetClass 1
        nftMetadata       = NFTMetadata {
            nftTokenName = tokenName, 
            nftMetaTokenName = metadataTokenName,
            nftMetaDescription = B.pack cpDescription, 
            nftMetaAuthor = B.pack cpAuthor,
            nftMetaFile = B.pack cpFile,
            nftTokenSymbol = nftTokenSymbol,
            nftMetaTokenSymbol = tokenMetadataSymbol,
            nftSeller = Nothing,
            nftSellPrice = 0
            }

    let marketInst = marketInstance market
        mrScript = marketScript market
        marketFactoryData   = Factory $ nftMetadata : nftMetas
        nftMetadataData   = NFTMeta nftMetadata
        marketVal    = assetClassValue (marketId market) 1
   
        lookups  = Constraints.scriptInstanceLookups marketInst        <>
                   Constraints.otherScript mrScript                    <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript marketFactoryData marketVal                                   <>
                   Constraints.mustPayToTheScript nftMetadataData nftMetadataVal                                <>
                   -- Constraints.mustPayToTheScript () nftValue                                                <>
                   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Create nftMetadata)     <>
                   Constraints.mustPayToPubKey ownPK nftValue

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    let nftMetaDto = nftMetadataToDto nftMetadata
    logInfo $ "created NFT: " ++ show nftMetaDto
    return nftMetaDto

-- | Set token for selling
sell :: HasBlockchainActions s => NFTMarket -> SellParams -> Contract w s Text NFTMetadataDto
sell market SellParams{..} = do
    pkh                     <- pubKeyHash <$> ownPubKey
    let tokenSymbol = CurrencySymbol $ B.pack spTokenSymbol
    (_, (oref, o, nftMetadata)) <- findMarketFactoryAndNftMeta market tokenSymbol
    let marketInst = marketInstance market
        nftMetadata' = nftMetadata { nftSeller = Just pkh, nftSellPrice = spSellPrice }
        nftMetadataDatum = NFTMeta nftMetadata'
        mrScript = marketScript market
        redeemer     = Redeemer $ PlutusTx.toData Sell
        values       = Value.singleton (nftTokenSymbol nftMetadata) (nftTokenName nftMetadata) 1 <> 
                       Value.singleton (nftMetaTokenSymbol nftMetadata) (nftMetaTokenName nftMetadata) 1
        lookups  = Constraints.scriptInstanceLookups marketInst        <>
                   Constraints.otherScript mrScript                    <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript nftMetadataDatum values <>
                   Constraints.mustSpendScriptOutput oref redeemer
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo $ "selling datum: " ++ show nftMetadataDatum
    let nftMetaDto = nftMetadataToDto nftMetadata'
    logInfo $ "selling NFT: " ++ show nftMetaDto
    return nftMetaDto

-- | Byt token
buy :: HasBlockchainActions s => NFTMarket -> BuyParams -> Contract w s Text NFTMetadataDto
buy market BuyParams{..} = do
    pkh                     <- pubKeyHash <$> ownPubKey
    let tokenSymbol = CurrencySymbol $ B.pack bpTokenSymbol  
    (_, (oref, o, nftMetadata)) <- findMarketFactoryAndNftMeta market tokenSymbol
    -- when (PlutusTx.Prelude.isNothing $ nftSeller nftMetadata) $
    --     throwError $ pack $ printf "NFT is not selling"
    let marketInst = marketInstance market
        nftMetadata' = nftMetadata { nftSeller = Nothing, nftSellPrice = 0 }
        nftSeller' = fromMaybe "" $ nftSeller nftMetadata
        nftMetadataDatum = NFTMeta nftMetadata'
    logInfo @String $ printf "seller %s" (show nftSeller')
    logInfo @String $ printf "price %s" (show $ nftSellPrice nftMetadata)
    let mrScript = marketScript market
        redeemer = Redeemer $ PlutusTx.toData Buy
        nftValue = Value.singleton (nftTokenSymbol nftMetadata) (nftTokenName nftMetadata) 1
        nftMetadataValue = Value.singleton (nftMetaTokenSymbol nftMetadata) (nftMetaTokenName nftMetadata) 1
        nftSellPriceValue = Ada.lovelaceValueOf $ nftSellPrice nftMetadata
 
        lookups  = Constraints.scriptInstanceLookups marketInst        <>
                   Constraints.otherScript mrScript                    <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript nftMetadataDatum nftMetadataValue <>
                   Constraints.mustSpendScriptOutput oref redeemer                  <>
                   Constraints.mustPayToPubKey pkh nftValue                       <>
                   Constraints.mustPayToPubKey nftSeller' nftSellPriceValue
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    let nftMetaDto = nftMetadataToDto nftMetadata
    logInfo $ "buying NFT: " ++ show nftMetaDto
    return nftMetaDto

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

findNftMetadata :: HasBlockchainActions s => NFTMarket -> NFTMetadata -> Contract w s Text (TxOutRef, TxOutTx, NFTMetadata)
findNftMetadata market nftMeta = findNFTMartketInstance market (assetClass (nftMetaTokenSymbol nftMeta) (nftMetaTokenName nftMeta)) $ \case
        NFTMeta nftMeta'
            | nftMeta == nftMeta' -> Just nftMeta'
        _               -> Nothing
        
findMarketFactoryAndNftMeta :: HasBlockchainActions s
    => NFTMarket
    -> CurrencySymbol
    -> Contract w s Text ( (TxOutRef, TxOutTx, [NFTMetadata])
                         , (TxOutRef, TxOutTx, NFTMetadata)
                         )
findMarketFactoryAndNftMeta market tokenSymbol = do
    (oref1, o1, nftMetas) <- findNFTMarketFactory market
    case [ nftMeta'
         | nftMeta' <- nftMetas
         , nftTokenSymbol nftMeta' == tokenSymbol
         ] of
        [nftMeta] -> do
            (oref2, o2, nftMeta1) <- findNftMetadata market nftMeta
            return ( (oref1, o1, nftMetas)
                , (oref2, o2, nftMeta1)
                )
        _    -> throwError "nft token not found"

-- | Finds all nft metadatas belonging to the market
queryNftMetadatas :: forall w s. HasBlockchainActions s => NFTMarket -> Contract w s Text [NFTMetadata]
queryNftMetadatas market = do
    (_, _, nftMarketMetas) <- findNFTMarketFactory market
    utxos <- utxoAt (marketAddress market)
    query nftMarketMetas $ snd <$> Map.toList utxos
  where
    query :: [NFTMetadata] -> [TxOutTx] -> Contract w s Text [NFTMetadata]
    query nftMarketMetas []       = return []
    query nftMarketMetas (o : os) = do
        let v = txOutValue $ txOutTxOut o
        if any (\meta -> assetClassValueOf v (assetClass (nftMetaTokenSymbol meta) (nftMetaTokenName meta)) == 1) nftMarketMetas
            then do
                d <- getNFTMarketDatum o
                case d of
                    Factory _ -> query nftMarketMetas os
                    NFTMeta nftMeta -> do
                        logInfo $ "found nftMetadata: " ++ show nftMeta
                        nftMetas <- query nftMarketMetas os
                        return $ nftMeta : nftMetas
            else query nftMarketMetas os

--
-- | Gets the caller's funds.
funds :: HasBlockchainActions s => Contract w s Text Value
funds = do
    logInfo @String $ printf "start getting funds"
    pkh <- pubKeyHash <$> ownPubKey
    os  <- map snd . Map.toList <$> utxoAt (pubKeyHashAddress pkh)
    return $ mconcat [txOutValue $ txOutTxOut o | o <- os]
 
-- | Gets the caller's NFTs.
userNftTokens :: HasBlockchainActions s => NFTMarket -> Contract w s Text [NFTMetadataDto]
userNftTokens market = do
    logInfo @String $ printf "start userNftTokens"
    pkh <- pubKeyHash <$> ownPubKey
    let ownAddress = pubKeyHashAddress pkh
    ownerUtxos <- utxoAt ownAddress
    let os = map snd $ Map.toList ownerUtxos
    let values = mconcat [txOutValue $ txOutTxOut o | o <- os]
    logInfo @String $ printf "load owner values"
    nftMetas <- queryNftMetadatas market
    logInfo @String $ printf "load market factory"
    logInfo @String $ printf "pkh" ++ show pkh
    let ownUserTokens = [ meta | meta <- nftMetas, valueOf values (nftTokenSymbol meta) (nftTokenName meta) == 1 ]
        sellingUserTokens = flip filter nftMetas (\ nftMetadata -> case nftSeller nftMetadata of 
            Just seller -> seller == pkh
            _           -> False)
        result = map nftMetadataToDto $ ownUserTokens <> sellingUserTokens
    return result

-- | Gets the caller's NFTs.
sellingTokens :: HasBlockchainActions s => NFTMarket -> Contract w s Text [NFTMetadataDto]
sellingTokens market = do
    logInfo @String $ printf "start sellingTokens"
    nftMetas <- queryNftMetadatas market
    let result = map nftMetadataToDto $ filter (PlutusTx.Prelude.isJust . nftSeller) nftMetas
    return result

ownerEndpoint :: Contract (Last (Either Text NFTMarket)) BlockchainActions Void ()
ownerEndpoint = do
    e <- runError start
    tell $ Last $ Just $ case e of
        Left err -> Left err
        Right market -> Right market

type MarketOwnerSchema =
    BlockchainActions
        .\/ Endpoint "start" ()

-- | Schema for the endpoints for users of NFTMarket.
type MarketUserSchema =
    BlockchainActions
        .\/ Endpoint "create" CreateParams
        .\/ Endpoint "sell" SellParams
        .\/ Endpoint "buy" BuyParams
        .\/ Endpoint "funds"  ()
        .\/ Endpoint "userNftTokens"  ()
        .\/ Endpoint "sellingTokens"  ()
        .\/ Endpoint "stop"   ()


data MarketContractState =
      Created NFTMetadataDto
    | Funds Value
    | Tokens [NFTMetadataDto]
    | SellingTokens [NFTMetadataDto]
    | Selling NFTMetadataDto
    | Buyed NFTMetadataDto
    | Stopped
    deriving (Show, Generic, FromJSON, ToJSON)
-- | Provides the following endpoints for users of a NFT marketplace instance:
--
-- [@create@]: Creates an nft token.
userEndpoints :: NFTMarket -> Contract (Last (Either Text MarketContractState)) MarketUserSchema Void ()
userEndpoints market =
    stop
        `select`
    ((f (Proxy @"create") Created create                                                       `select`
      f (Proxy @"sell") Selling sell                                                           `select`
      f (Proxy @"buy") Buyed buy                                                           `select`
      f (Proxy @"userNftTokens") Tokens (\market'' () -> userNftTokens market'')               `select`
      f (Proxy @"sellingTokens") SellingTokens (\market'' () -> sellingTokens market'')        `select`
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