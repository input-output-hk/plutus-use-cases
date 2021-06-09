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
{-# LANGUAGE StandaloneDeriving         #-}

-- | Implements a custom currency with a monetary policy that allows
--   the forging of a fixed amount of units.
module Contracts.NFT.OffChain
    ( NFTMetadataDto (..)
    , CreateParams (..)
    , SellParams (..)
    , CancelSellParams (..)
    , BuyParams (..)
    , TransferParams (..)
    , MarketUserSchema, MarketContractState (..)
    , MarketOwnerSchema
    , forgeNftToken
    , start, create
    , ownerEndpoint, userEndpoints
    , marketplaceTokenName
    , marketAddress
    , nftMetadataToDto
    ) where

import           Control.Monad                    hiding (fmap)
import qualified Data.Map                         as Map
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Base64           as B64
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
import           Wallet.Emulator                  (walletPubKey)
import           Contracts.NFT.Types
import           Contracts.NFT.OnChain            (marketInstance)

marketplaceTokenName :: TokenName
marketplaceTokenName = "NFTMarketplace"
metadataTokenName = "NFTMetadata"

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
    { spTokenSymbol :: String
    , spSellPrice   :: Integer
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @cancel-sell@-endpoint, which creates a new NFT.
data CancelSellParams = CancelSellParams
    { cspTokenSymbol :: String
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @buy@-endpoint, which creates a new NFT.
data BuyParams = BuyParams
    { bpTokenSymbol :: String
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Parameters for the @transfer@-endpoint, which creates a new NFT.
data TransferParams = TransferParams
    { tpTokenSymbol     :: String
    , tpReceiverWallet  :: Integer
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data NFTMetadataDto = NFTMetadataDto
    { nftDtoTokenName:: String
    , nftDtoMetaDescription:: String
    , nftDtoMetaAuthor:: String
    , nftDtoMetaFile:: String
    , nftDtoTokenSymbol :: String
    , nftDtoSeller :: String
    , nftDtoSellPrice:: Integer
    } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

nftMetadataToDto:: NFTMetadata -> NFTMetadataDto
nftMetadataToDto nftMeta = NFTMetadataDto 
    { nftDtoTokenName = read.show $ nftTokenName nftMeta
    , nftDtoMetaDescription = B.unpack $ nftMetaDescription nftMeta
    , nftDtoMetaAuthor = B.unpack $ nftMetaAuthor nftMeta
    , nftDtoMetaFile = B.unpack $ nftMetaFile nftMeta
    , nftDtoTokenSymbol = byteStrToDto . unCurrencySymbol $ nftTokenSymbol nftMeta
    , nftDtoSeller = fromMaybe ("" :: String) $ byteStrToDto . getPubKeyHash <$> nftSeller nftMeta
    , nftDtoSellPrice = nftSellPrice nftMeta
    }

byteStrToDto :: ByteString -> String
byteStrToDto = B.unpack . B64.encode

dtoStrToByteStr :: String -> ByteString
dtoStrToByteStr = B64.decodeLenient . B.pack 

forgeNftToken:: 
    forall w s. HasBlockchainActions s 
    => TokenName
    -> PubKeyHash
    -> Contract w s Text CurrencySymbol
forgeNftToken tokenName pk = fmap Currency.currencySymbol $
    mapError (pack . show @Currency.CurrencyError) $
    Currency.forgeContract pk [(tokenName, 1)]

-- | Creates a Marketplace "factory". This factory will keep track of the existing nft tokens
start ::
    forall w w' s. 
    (HasBlockchainActions s
    )
    => (TokenName -> PubKeyHash -> Contract w s Text CurrencySymbol)
    -> Contract w s Text NFTMarket
start forgeNft = do
    pkh <- pubKeyHash <$> ownPubKey
    cs  <- forgeNft marketplaceTokenName pkh
    let c = assetClass cs marketplaceTokenName
        market   = marketplace cs
        inst = marketInstance market
        tx   = mustPayToTheScript (Factory []) $ assetClassValue c 1
    ledgerTx <- submitTxConstraints inst tx
    void $ awaitTxConfirmed $ txId ledgerTx

    logInfo @String $ printf "started Market %s at address %s" (show market) (show $ marketAddress market)
    return market

-- | Creates an NFT token
create :: 
    HasBlockchainActions s 
    => (TokenName -> PubKeyHash -> Contract w s Text CurrencySymbol)
    -> NFTMarket 
    -> CreateParams 
    -> Contract w s Text NFTMetadataDto
create forgeNft market CreateParams{..} = do
    (oref, o, nftMetas) <- findNFTMarketFactory market
    let tokenName = TokenName $ B.pack cpTokenName
    ownPK <- pubKeyHash <$> ownPubKey
    nftTokenSymbol <- forgeNft tokenName ownPK 
    let nftTokenAssetClass = assetClass nftTokenSymbol tokenName
        nftValue = assetClassValue nftTokenAssetClass 1
 
    let metadataTokenName = TokenName $ B.pack $ read (show tokenName) ++ "Metadata"
    tokenMetadataSymbol <- forgeNft metadataTokenName ownPK
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
                   Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toData $ Create nftMetadata)     <>
                   Constraints.mustPayToPubKey ownPK nftValue

    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    let nftMetaDto = nftMetadataToDto nftMetadata
    logInfo $ "created NFT: " ++ show nftMetaDto
    return nftMetaDto

-- | Set token for selling
sell :: 
    HasBlockchainActions s 
    => NFTMarket 
    -> SellParams 
    -> Contract w s Text NFTMetadataDto
sell market SellParams{..} = do
    pkh                     <- pubKeyHash <$> ownPubKey
    let tokenSymbol = CurrencySymbol $ dtoStrToByteStr spTokenSymbol
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

-- | Cacnel token selling
cancelSell :: 
    HasBlockchainActions s 
    => NFTMarket 
    -> CancelSellParams 
    -> Contract w s Text NFTMetadataDto
cancelSell market CancelSellParams{..} = do
    pkh <- pubKeyHash <$> ownPubKey
    let tokenSymbol = CurrencySymbol $ dtoStrToByteStr cspTokenSymbol
    (_, (oref, o, nftMetadata)) <- findMarketFactoryAndNftMeta market tokenSymbol
    when (PlutusTx.Prelude.isNothing $ nftSeller nftMetadata) $
        throwError $ pack $ printf "NFT is not on sale"
    let marketInst = marketInstance market
        nftMetadata' = nftMetadata { nftSeller = Nothing, nftSellPrice = 0 }
        nftSeller' = fromMaybe "" $ nftSeller nftMetadata
        nftMetadataDatum = NFTMeta nftMetadata'
    let mrScript = marketScript market
        redeemer = Redeemer $ PlutusTx.toData CancelSell
        nftValue = Value.singleton (nftTokenSymbol nftMetadata) (nftTokenName nftMetadata) 1
        nftMetadataValue = Value.singleton (nftMetaTokenSymbol nftMetadata) (nftMetaTokenName nftMetadata) 1
 
        lookups  = Constraints.scriptInstanceLookups marketInst        <>
                   Constraints.otherScript mrScript                    <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript nftMetadataDatum nftMetadataValue <>
                   Constraints.mustSpendScriptOutput oref redeemer                  <>
                   Constraints.mustPayToPubKey pkh nftValue
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    let nftMetaDto = nftMetadataToDto nftMetadata
    logInfo $ "cancel sell NFT: " ++ show nftMetaDto
    return nftMetaDto

-- | Byt token
buy :: 
    HasBlockchainActions s 
    => NFTMarket 
    -> BuyParams 
    -> Contract w s Text NFTMetadataDto
buy market BuyParams{..} = do
    pkh <- pubKeyHash <$> ownPubKey
    let tokenSymbol = CurrencySymbol $ dtoStrToByteStr bpTokenSymbol
    (_, (oref, o, nftMetadata)) <- findMarketFactoryAndNftMeta market tokenSymbol
    when (PlutusTx.Prelude.isNothing $ nftSeller nftMetadata) $
        throwError $ pack $ printf "NFT is not on sale"
    let marketInst = marketInstance market
        nftMetadata' = nftMetadata { nftSeller = Nothing, nftSellPrice = 0 }
        nftSeller' = fromMaybe "" $ nftSeller nftMetadata
        nftMetadataDatum = NFTMeta nftMetadata'
    let mrScript = marketScript market
        redeemer = Redeemer $ PlutusTx.toData $ Buy pkh
        nftValue = Value.singleton (nftTokenSymbol nftMetadata) (nftTokenName nftMetadata) 1
        nftMetadataValue = Value.singleton (nftMetaTokenSymbol nftMetadata) (nftMetaTokenName nftMetadata) 1
        nftSellPriceValue = Ada.lovelaceValueOf $ nftSellPrice nftMetadata
 
        lookups  = Constraints.scriptInstanceLookups marketInst        <>
                   Constraints.otherScript mrScript                    <>
                   Constraints.unspentOutputs (Map.singleton oref o)

        tx       = Constraints.mustPayToTheScript nftMetadataDatum nftMetadataValue <>
                   Constraints.mustSpendScriptOutput oref redeemer                  <>
                   Constraints.mustPayToPubKey pkh nftValue                         <>
                   Constraints.mustPayToPubKey nftSeller' nftSellPriceValue
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    let nftMetaDto = nftMetadataToDto nftMetadata
    logInfo $ "buying NFT: " ++ show nftMetaDto
    return nftMetaDto

-- | Transfer token
transfer :: 
    HasBlockchainActions s 
    => NFTMarket 
    -> TransferParams 
    -> Contract w s Text NFTMetadataDto
transfer market TransferParams{..} = do
    let tokenSymbol = CurrencySymbol $ dtoStrToByteStr tpTokenSymbol
    let sendToKeyHash = pubKeyHash $ walletPubKey $ Wallet tpReceiverWallet
    (_, (oref, o, nftMetadata)) <- findMarketFactoryAndNftMeta market tokenSymbol
    let nftValue = Value.singleton (nftTokenSymbol nftMetadata) (nftTokenName nftMetadata) 1
    tx <- submitTx $ mustPayToPubKey sendToKeyHash nftValue
    awaitTxConfirmed $ txId tx
    let nftMetaDto = nftMetadataToDto nftMetadata
    logInfo $ "transfer NFT: " ++ show nftMetaDto
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

findNFTMartketInstance :: 
    HasBlockchainActions s 
    => NFTMarket 
    -> AssetClass 
    -> (NFTMarketDatum -> Maybe a) 
    -> Contract w s Text (TxOutRef, TxOutTx, a)
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

findNFTMarketFactory :: 
    HasBlockchainActions s 
    => NFTMarket 
    -> Contract w s Text (TxOutRef, TxOutTx, [NFTMetadata])
findNFTMarketFactory nftm@NFTMarket{..} = findNFTMartketInstance nftm marketId $ \case
    Factory nfts -> Just nfts
    NFTMeta _    -> Nothing

findNftMetadata :: 
    HasBlockchainActions s 
    => NFTMarket 
    -> NFTMetadata 
    -> Contract w s Text (TxOutRef, TxOutTx, NFTMetadata)
findNftMetadata market nftMeta = findNFTMartketInstance market (assetClass (nftMetaTokenSymbol nftMeta) (nftMetaTokenName nftMeta)) $ \case
        NFTMeta nftMeta'
            | nftMeta == nftMeta' -> Just nftMeta'
        _               -> Nothing
        
findMarketFactoryAndNftMeta :: 
    HasBlockchainActions s
    => NFTMarket
    -> CurrencySymbol
    -> Contract w s Text ((TxOutRef, TxOutTx, [NFTMetadata])
                          ,(TxOutRef, TxOutTx, NFTMetadata))
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
queryNftMetadatas :: 
    forall w s. HasBlockchainActions s 
    => NFTMarket 
    -> Contract w s Text [NFTMetadata]
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
userPubKeyHash :: HasBlockchainActions s => Contract w s Text [Char]
userPubKeyHash = do
    logInfo @String $ printf "start getting userPubKeyHash"
    pkh <- pubKeyHash <$> ownPubKey
    return $ byteStrToDto . getPubKeyHash $ pkh
 
-- | Gets the caller's NFTs.
userNftTokens :: 
    HasBlockchainActions s 
    => NFTMarket 
    -> Contract w s Text [NFTMetadataDto]
userNftTokens market = do
    logInfo @String $ printf "start userNftTokens"
    pkh <- pubKeyHash <$> ownPubKey
    let ownAddress = pubKeyHashAddress pkh
    ownerUtxos <- utxoAt ownAddress
    let os = map snd $ Map.toList ownerUtxos
    let values = mconcat [txOutValue $ txOutTxOut o | o <- os]
    nftMetas <- queryNftMetadatas market
    let ownUserTokens = [ meta | meta <- nftMetas, valueOf values (nftTokenSymbol meta) (nftTokenName meta) == 1 ]
        sellingUserTokens = flip filter nftMetas (\ nftMetadata -> case nftSeller nftMetadata of 
            Just seller -> seller == pkh
            _           -> False)
        result = map nftMetadataToDto $ ownUserTokens <> sellingUserTokens
    return result

-- | Gets the caller's NFTs.
sellingTokens :: 
    HasBlockchainActions s
    => NFTMarket 
    -> Contract w s Text [NFTMetadataDto]
sellingTokens market = do
    logInfo @String $ printf "start sellingTokens"
    nftMetas <- queryNftMetadatas market
    let result = map nftMetadataToDto $ filter (PlutusTx.Prelude.isJust . nftSeller) nftMetas
    return result


ownerEndpoint :: 
    (TokenName 
    -> PubKeyHash 
    -> Contract (Last (Either Text NFTMarket)) MarketOwnerSchema Text CurrencySymbol
    )
    -> Contract (Last (Either Text NFTMarket)) MarketOwnerSchema Void ()
ownerEndpoint forgeNft = start' forgeNft >> ownerEndpoint forgeNft
    where 
        start' forgeNft = do
            e <- runError $ do 
                 endpoint @"start"
                 start forgeNft
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
        .\/ Endpoint "cancelSell" CancelSellParams
        .\/ Endpoint "buy" BuyParams
        .\/ Endpoint "transfer" TransferParams
        .\/ Endpoint "userPubKeyHash"  ()
        .\/ Endpoint "userNftTokens"  ()
        .\/ Endpoint "sellingTokens"  ()
        .\/ Endpoint "stop"   ()


data MarketContractState =
      Created NFTMetadataDto
    | Tokens [NFTMetadataDto]
    | SellingTokens [NFTMetadataDto]
    | Selling NFTMetadataDto
    | CancelSelling NFTMetadataDto
    | Buyed NFTMetadataDto
    | Transfered NFTMetadataDto
    | UserPubKeyHash String
    | Stopped
    deriving (Show, Generic, FromJSON, ToJSON)
-- | Provides the following endpoints for users of a NFT marketplace instance:
--
-- [@create@]: Creates an nft token.
userEndpoints ::
    (TokenName 
    -> PubKeyHash 
    -> Contract (Last (Either Text MarketContractState)) MarketUserSchema Text CurrencySymbol
    )
    -> NFTMarket 
    -> Contract (Last (Either Text MarketContractState)) MarketUserSchema Void ()
userEndpoints forgeNft market =
    stop
        `select`
    ((f (Proxy @"create") Created (\market' createParams -> create forgeNft market' createParams)                                                                  `select`
      f (Proxy @"sell") Selling sell                                                           `select`
      f (Proxy @"cancelSell") CancelSelling cancelSell                                         `select`
      f (Proxy @"buy") Buyed buy                                                               `select`
      f (Proxy @"transfer") Transfered transfer                                               `select`
      f (Proxy @"userNftTokens") Tokens (\market' () -> userNftTokens market')               `select`
      f (Proxy @"sellingTokens") SellingTokens (\market' () -> sellingTokens market')        `select`
      f (Proxy @"userPubKeyHash")  UserPubKeyHash (\market' () -> userPubKeyHash))    >> userEndpoints forgeNft market)
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