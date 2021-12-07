{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Cardano.Binary
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson.Lens
import Data.Aeson(FromJSON(..), ToJSON(..), (.:))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Dependent.Sum
import Data.Int (Int32)
import qualified Data.Map as Map
import Data.Maybe
import Data.Pool
import Data.Proxy
import Data.Scientific (coefficient)
import Data.Semigroup (First(..))
import Data.Text (Text)
import Data.Text.Read (decimal)
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Vessel
import Database.Beam (MonadBeam, Generic)
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Query
import Database.Beam.Schema.Tables (primaryKey)
import qualified Database.PostgreSQL.Simple as Pg
import Gargoyle.PostgreSQL.Connect
import Obelisk.Backend
import Obelisk.Route
import Rhyolite.Backend.App
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.Serializable
import Rhyolite.Backend.Listen
import Rhyolite.Concurrent
import Safe (lastMay, headMay)
import Snap.Util.FileServe
import Statistics.Regression
import System.Directory
import System.Exit
import System.Process

import Backend.Notification
import Backend.Schema
import Common.Api
import Common.Route
import Common.Schema
import Common.Plutus.Contracts.Uniswap.Types

import Network.HTTP.Client hiding (Proxy)
import qualified Network.WebSockets as WS
import Control.Lens

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      httpManager <- newManager defaultManagerSettings
      withDb "db" $ \pool -> do
        withResource pool runMigrations
        stopSyncUniswapUsers <- worker (1000 * 1000 * 5) $ syncUniswapUsers httpManager pool
        stopSyncPooledTokens <- worker (1000 * 1000 * 5) $ syncPooledTokens httpManager pool
        (handleListen, finalizeServeDb) <- serveDbOverWebsockets
          pool
          (requestHandler httpManager pool)
          (\(nm :: DbNotification Notification) q ->
             fmap (fromMaybe emptyV) $ mapDecomposedV (notifyHandler nm) q)
          (QueryHandler $ \q -> fmap (fromMaybe emptyV) $ mapDecomposedV (queryHandler pool) q)
          vesselFromWire
          vesselPipeline -- (tracePipeline "==> " . vesselPipeline)
        flip finally (stopSyncPooledTokens >> stopSyncUniswapUsers >> finalizeServeDb) $ serve $ \case
          BackendRoute_Listen :/ () -> handleListen
          BackendRoute_WASM :/ path -> do
            let (_, extension) = T.breakOn "." $ fromMaybe mempty $ listToMaybe $ reverse path
                mimetype = case extension of
                  ".js" -> "text/javascript"
                  ".wasm" -> "application/wasm"
                  ".module.wasm" -> "application/wasm"
                  _ -> mempty
            serveFileAs mimetype $ T.unpack $ "static/" <> T.intercalate "/" path
          _ -> return ()

          _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }

uniswapScriptAddress :: Text
uniswapScriptAddress = "addr_test1wqwtftz4r7ly8e8qkkt9h4dhjmhe5zpq4cmx4tdepsqx7psx88h7z"

data BuiltTx = BuiltTx
  { _builtTx_type :: Text
  , _builtTx_description :: Text
  , _builtTx_cborHex :: Text
  } deriving (Show, Generic)

instance FromJSON BuiltTx where
  parseJSON (Aeson.Object v) =
    BuiltTx <$> v .: "type"
            <*> v .: "description"
            <*> v .: "cborHex"
  parseJSON _ = mzero
instance ToJSON BuiltTx

-- | Handle requests / commands, a standard part of Obelisk apps.
requestHandler :: Manager -> Pool Pg.Connection -> RequestHandler Api IO
requestHandler httpManager pool = RequestHandler $ \case
  Api_Swap contractId coinA coinB amountA amountB ->
    executeSwap httpManager pool (T.unpack $ unContractInstanceId contractId) (coinA, amountA) (coinB, amountB)
  Api_Stake contractId coinA coinB amountA amountB ->
    executeStake httpManager (T.unpack $ unContractInstanceId contractId) (coinA, amountA) (coinB, amountB)
  Api_RedeemLiquidity contractId coinA coinB amount ->
    executeRemove httpManager (T.unpack $ unContractInstanceId contractId) coinA coinB amount
  Api_CallFunds cid -> callFunds httpManager cid
  Api_CallPools cid -> callPools httpManager cid
  Api_EstimateTransactionFee action -> estimateTransactionFee pool action
  Api_BuildStaticSwapTransaction walletAddress -> buildStaticSwapTransaction walletAddress

notifyHandler :: DbNotification Notification -> DexV Proxy -> IO (DexV Identity)
notifyHandler dbNotification _ = case _dbNotification_message dbNotification of
  Notification_Contract :=> Identity contract -> do
    return $ singletonV Q_ContractList $ IdentityV $ Identity $
      Map.singleton (_contract_walletId contract) $ First $ Just $ _contract_id contract
  Notification_Pool :=> Identity pool -> do
    return $ singletonV Q_Pools $ IdentityV $ Identity $
      Map.singleton (_pool_liquiditySymbol pool) $ First $ Just pool

queryHandler :: Pool Pg.Connection -> DexV Proxy -> IO (DexV Identity)
queryHandler pool v = buildV v $ \case
  -- Handle View to see list of available wallet contracts
  Q_ContractList -> \_ -> runNoLoggingT $ runDb (Identity pool) $ runBeamSerializable $ do
    contracts <- runSelectReturningList $ select $ all_ (_db_contracts db)
    return $ IdentityV $ Identity $ Map.fromList $
      fmap (\c -> (_contract_walletId c, First $ Just $ _contract_id c)) contracts
  Q_PooledTokens -> \_ -> runNoLoggingT $ runDb (Identity pool) $ runBeamSerializable $ do
    pooledTokens <- runSelectReturningList $ select $ all_ (_db_pooledTokens db)
    return $ IdentityV $ Identity $ First $ Just $ pooledTokens
  Q_Pools -> \_ -> runNoLoggingT $ runDb (Identity pool) $ runBeamSerializable $ do
    pools <- runSelectReturningList $ select $ all_ (_db_pools db)
    return $ IdentityV $ Identity $ Map.fromList $ flip fmap pools $ \p ->
      (_pool_liquiditySymbol p, First $ Just p)

-- | Query for active instances from the PAB and upsert new UniswapUser instance ids.
syncUniswapUsers :: Manager -> Pool Pg.Connection -> IO ()
syncUniswapUsers httpManager pool = do
  initReq <- parseRequest "http://localhost:8080/api/new/contract/instances"
  let req = initReq { method = "GET" }
  resp <- httpLbs req httpManager
  let val = Aeson.eitherDecode (responseBody resp) :: Either String [Aeson.Value]
  case val of
    Left err -> do
      putStrLn $ "getWallets: failed to decode response body: " ++ err
    Right objs -> do
      let walletContracts = flip mapMaybe objs $ \obj -> do
            contractInstanceId <- obj ^? key "cicContract". key "unContractInstanceId" . _String
            walletId <- obj ^? key "cicWallet". key "getWallet" . _Integer
            definition <- obj ^? key "cicDefintion". key "tag" . _String
            guard $ definition == "UniswapUser"
            return $ Contract contractInstanceId (fromIntegral walletId)
      print $ "Wallet Ids persisted: " ++ show walletContracts -- DEBUG: Logging incoming wallets/contract ids
      -- Persist participating wallet addresses to Postgresql
      runNoLoggingT $ runDb (Identity pool) $ do
        rows <- runBeamSerializable $ runInsertReturningList $ insertOnConflict
          (_db_contracts db)
          (insertValues walletContracts)
          (conflictingFields primaryKey)
          onConflictUpdateAll
        mapM_ (notify NotificationType_Insert Notification_Contract) rows

syncPooledTokens :: Manager -> Pool Pg.Connection -> IO ()
syncPooledTokens httpManager pool = do
  -- use admin wallet id to populate db with current pool tokens available
  mAdminWallet <- runNoLoggingT $ runDb (Identity pool) $ runBeamSerializable $
    -- SELECT _contract_id FROM _db_contracts WHERE _contract_walletId =1;
    runSelectReturningOne $
      select $
      filter_ (\ct -> _contract_walletId ct ==. val_ 1) $
      all_ (_db_contracts db)
  wid <- case mAdminWallet of
    Nothing -> fail "getPooledTokens: Admin user wallet not found"
    Just wid -> return wid

  -- In order to retreive list of pooled tokens, a request must be made to the pools endpoint first and then the response
  -- can be found be found in instances within the observable state key
  let contractInstanceId = T.unpack $ _contract_id wid
      prString = "http://localhost:8080/api/new/contract/instance/" <> contractInstanceId <> "/endpoint/pools"
  print $ "prString: " ++ prString -- DEBUG
  poolReq <- parseRequest prString
  let reqBody = "[]"
      pReq = poolReq
        { method = "POST"
        , requestHeaders = ("Content-Type","application/json"):(requestHeaders poolReq)
        , requestBody = RequestBodyLBS reqBody
        }
  _ <- httpLbs pReq httpManager

  -- This delay is necessary to give the chain 1 second to process the previous request and update the observable state
  threadDelay 1000000
  initReq <- do
    let req =
          "http://localhost:8080/api/new/contract/instance/" <>
          contractInstanceId <>
          "/status"
    putStrLn req
    parseRequest $ req


  let req = initReq { method = "GET" }
  resp <- httpLbs req httpManager
  let val = Aeson.eitherDecode (responseBody resp) :: Either String Aeson.Value
  case val of
    Left err -> do
      print $ "getPooledTokens: failed to decode response body: " <> err
      return ()
    Right obj -> do
      -- aeson-lens happened here in order to get currency symbols and token names from json
      let tokenInfo :: Maybe [((Aeson.Value, Int32), (Aeson.Value, Int32), (Aeson.Value, Int32))]
          tokenInfo = obj ^? key "cicCurrentState". key "observableState" . key "Right" . key "contents" . _Value . _JSON

          pooledTokens :: [PooledToken]
          pooledTokens = flip foldMap pools $ \p ->
            [ PooledToken (_pool_tokenASymbol p) (_pool_tokenAName p)
            , PooledToken (_pool_tokenBSymbol p) (_pool_tokenBName p)
            ]

          pools :: [LPool]
          pools = flip mapMaybe (fromMaybe mempty tokenInfo) $ \((tokenA, amountA), (tokenB, amountB), (lp, amountLp)) -> do
            let curSymbol = key "unAssetClass" . nth 0 . key "unCurrencySymbol" . _String
                curName = key "unAssetClass" . nth 1 . key "unTokenName" . _String
            tokenASymbol <- tokenA ^? curSymbol
            tokenAName <- tokenA ^? curName
            tokenBSymbol <- tokenB ^? curSymbol
            tokenBName <- tokenB ^? curName
            lpSymbol <- lp ^? key "unTokenName" . _String
            return $ Pool
              { _pool_tokenASymbol = tokenASymbol
              , _pool_tokenAName = tokenAName
              , _pool_tokenBSymbol = tokenBSymbol
              , _pool_tokenBName = tokenBName
              , _pool_tokenAAmount = amountA
              , _pool_tokenBAmount = amountB
              , _pool_liquiditySymbol = lpSymbol
              , _pool_liquidityAmount = amountLp
              }
      putStrLn $ "Pools: " <> show pools
      print $ "Pool tokens persisted: " ++ show pooledTokens -- DEBUG: Logging incoming pooled tokens
      -- Persist current state of pool tokens to Postgresql
      runNoLoggingT $ runDb (Identity pool) $ do
        rows <- runBeamSerializable $ do
          runInsert $ insertOnConflict (_db_pooledTokens db) (insertValues pooledTokens)
            (conflictingFields primaryKey)
            onConflictDoNothing
          runInsertReturningList $ insertOnConflict (_db_pools db) (insertValues pools)
            (conflictingFields primaryKey)
            onConflictDoNothing -- FIXME
        mapM_ (notify NotificationType_Insert Notification_Pool) rows
  return ()

-- This function's is modeled after the following curl that submits a request to perform a swap against PAB.
{-
curl \
  -H "Content-Type: application/json" \
  --request POST \
  --data '{
    "spAmountA": 112,
    "spAmountB": 0,
    "spCoinB": {
      "unAssetClass": [
        {
          "unCurrencySymbol": "7c7d03e6ac521856b75b00f96d3b91de57a82a82f2ef9e544048b13c3583487e"
        },
        {
          "unTokenName": "A"
        }
      ]
    },
    "spCoinA": {
      "unAssetClass": [
        {
          "unCurrencySymbol": ""
        },
        {
          "unTokenName": ""
        }
      ]
    }
  }' \
  http://localhost:8080/api/new/contract/instance/36951109-aacc-4504-89cc-6002cde36e04/endpoint/swap
-}
executeSwap :: Manager
  -> Pool Pg.Connection
  -> String
  -> (Coin AssetClass, Amount Integer)
  -> (Coin AssetClass, Amount Integer)
  -> IO (Either Aeson.Value Aeson.Value)
executeSwap httpManager pool contractId (coinA, amountA) (coinB, amountB) = do
  let requestUrl = "http://localhost:8080/api/new/contract/instance/" ++ contractId ++ "/endpoint/swap"
      reqBody = SwapParams {
          spCoinA = coinA
        , spCoinB = coinB
        , spAmountA = amountA
        , spAmountB = amountB
        }
  initReq <- parseRequest requestUrl
  let req = initReq
        { method = "POST"
        , requestHeaders = ("Content-Type","application/json"):(requestHeaders initReq)
        , requestBody = RequestBodyLBS $ Aeson.encode reqBody
        }
  -- The response to this request does not return anything but an empty list.
  -- A useful response must be fetched from "observableState"
  print ("executeSwap: sending request to pab..." :: String)
  startTime <- getCurrentTime
  _ <- httpLbs req httpManager
  print ("executeSwap: request sent." :: String)
  -- MVar that will hold response to swap request sent
  eitherObState <- newEmptyMVar
  -- Use websocket connection to fetch observable state response
  (eitherObState', endTime) <- WS.runClient "127.0.0.1" 8080 ("/ws/" ++ contractId) $ \conn -> do
    -- Allow enough time to pass for observable state to be updated (10 secs)
    let processData = do
          incomingData :: ByteString <- WS.receiveData conn
          let val :: Either String Aeson.Value = Aeson.eitherDecode' $ BS.fromStrict incomingData
          case val of
            Left err -> do
              putStrLn $ "executeSwap: failed to decode response body: " ++ err
              processData
            Right obj0 ->
              case do
                obj :: Aeson.Value <- obj0 ^? key "contents"
                newObservableStateTag <- incomingData ^? key "tag" . _String
                guard $ newObservableStateTag == "NewObservableState"
                (<|>)
                  (fmap Left $ obj ^? key "Left")
                  (do
                    let swapTag = obj ^. key "Right" . key "tag" . _String
                        txFeeDetails = obj ^. key "Right"
                          . key "contents" . nth 0 . key "txFee" . key "getValue" . nth 0 . nth 1 . nth 0 . _Array
                        aesArr = obj ^. key "Right"
                          . key "contents" . _Array
                        scrSize = fromMaybe (Aeson.Number 0) $ lastMay $ V.toList aesArr
                    guard $ swapTag == "Swapped"
                    Just $ Right (Aeson.Array txFeeDetails, scrSize))
              of
                Just x -> putMVar eitherObState x
                Nothing -> processData
    fid <- forkIO processData
    flip onException (killThread fid) $ do
      -- retreive observable state response from result of forked thread
      eitherObState' <- takeMVar eitherObState
      WS.sendClose conn ("executeSwap: closing backend websocket connection..." :: Text)
      endTime <- getCurrentTime
      return (eitherObState', endTime)
  case eitherObState' of
    Left err -> return $ Left err
    Right (txFeeDetails, Aeson.Number scrSize) -> case txFeeDetails of
      Aeson.Array xs -> case V.toList xs of
        _:(Aeson.Number txFee):_ -> do
          let contractAction = "Swap"
              processingTime :: Int32 = fromIntegral $ fromEnum $ diffUTCTime endTime startTime
              txFee' :: Int32 = (fromIntegral $ coefficient txFee)
          -- persist transaction fee and details to postgres for use in regression when estimating transaction fees later
          runNoLoggingT $ runDb (Identity pool) $ runBeamSerializable $ do
            runInsert $
              insertOnConflict (_db_txFeeDataSet db) (insertExpressions
                [TxFeeDataSet default_ (val_ txFee') (val_ contractAction) (val_ processingTime) (val_ $ fromIntegral $ coefficient scrSize)])
              (conflictingFields _txFeeDataSet_id)
              onConflictDoNothing
          return $ Right txFeeDetails
        _ -> return $ Left "Error unexpected data type in txFeeDetails"
      _ -> return $ Left "Error parsing txFeeDetails as a JSON Array"
    Right (_txFeeDetails, _) -> return $ Left "Unexpected script size data type"

{-
curl \
  -H "Content-Type: application/json" \
  --request POST \
  --data '{
  "apAmountA": 4500,
  "apAmountB": 9000,
  "apCoinB": {
      "unAssetClass": [
        {
          "unCurrencySymbol": "7c7d03e6ac521856b75b00f96d3b91de57a82a82f2ef9e544048b13c3583487e"
        },
        {
          "unTokenName": "A"
        }
      ]
    },
    "apCoinA": {
      "unAssetClass": [
        {
          "unCurrencySymbol": ""
        },
        {
          "unTokenName": ""
        }
      ]
    }
  }' \
  http://localhost:8080/api/new/contract/instance/3b0bafe2-14f4-4d34-a4d8-633afb8e52eb/endpoint/add
-}
executeStake
  :: Manager
  -> String
  -> (Coin AssetClass , Amount Integer)
  -> (Coin AssetClass, Amount Integer)
  -> IO (Either String Aeson.Value)
executeStake httpManager contractId (coinA, amountA) (coinB, amountB) = do
  let requestUrl = "http://localhost:8080/api/new/contract/instance/" ++ contractId ++ "/endpoint/add"
      reqBody = AddParams {
          apCoinA = coinA
        , apCoinB = coinB
        , apAmountA = amountA
        , apAmountB = amountB
        }
  initReq <- parseRequest requestUrl
  let req = initReq
        { method = "POST"
        , requestHeaders = ("Content-Type","application/json"):(requestHeaders initReq)
        , requestBody = RequestBodyLBS $ Aeson.encode reqBody
        }
  -- The response to this request does not return anything but an empty list.
  -- A useful response must be fetched from "observableState"
  print $ ("executeStake: sending request to pab..." :: String)
  _ <- httpLbs req httpManager
  print $ ("executeStake: request sent." :: String)
  (either (\a -> return $ Left a) (\a -> return $ Right $ fst a)) =<< fetchObservableStateFees httpManager contractId

{-
curl \
  -H "Content-Type: application/json" \
  --request POST \
  --data '{
    "rpDiff": 2461,
    "rpCoinB": {
      "unAssetClass": [
        {
          "unCurrencySymbol": "7c7d03e6ac521856b75b00f96d3b91de57a82a82f2ef9e544048b13c3583487e"
        },
        {
          "unTokenName": "A"
        }
      ]
    },
    "rpCoinA": {
      "unAssetClass": [
        {
          "unCurrencySymbol": ""
        },
        {
          "unTokenName": ""
        }
      ]
    }
  }'\
  http://localhost:8080/api/new/contract/instance/9079d01a-342b-4d4d-88b5-7525ff1118d6/endpoint/remove
-}
executeRemove
  :: Manager
  -> String
  -> Coin AssetClass
  -> Coin AssetClass
  -> Amount Integer
  -> IO (Either String Aeson.Value)
executeRemove httpManager contractId coinA coinB amount = do
  let requestUrl = "http://localhost:8080/api/new/contract/instance/" ++ contractId ++ "/endpoint/remove"
      reqBody = RemoveParams {
          rpCoinA = coinA
        , rpCoinB = coinB
        , rpDiff = amount
        }
  initReq <- parseRequest requestUrl
  let req = initReq
        { method = "POST"
        , requestHeaders = ("Content-Type","application/json"):(requestHeaders initReq)
        , requestBody = RequestBodyLBS $ Aeson.encode reqBody
        }
  -- The response to this request does not return anything but an empty list.
  -- A useful response must be fetched from "observableState"
  print $ ("executeRemove: sending request to pab..." :: String)
  _ <- httpLbs req httpManager
  print $ ("executeRemove: request sent." :: String)
  (either (\a -> return $ Left a) (\a -> return $ Right $ fst a)) =<< fetchObservableStateFees httpManager contractId

-- | Grabs transaction fees from `observaleState` field from the contract
-- instance status endpoint.
fetchObservableStateFees
  :: Manager
  -> String
  -> IO (Either String (Aeson.Value, Aeson.Value)) -- (TransactionFees, ScriptSize)
fetchObservableStateFees httpManager contractId = do
  let requestUrl = "http://localhost:8080/api/new/contract/instance/" ++ contractId ++ "/status"
  initReq <- parseRequest requestUrl
  resp <- httpLbs initReq httpManager
  let val = Aeson.eitherDecode (responseBody resp) :: Either String Aeson.Value
  case val of
    Left err -> do
      return $ Left err
    Right obj -> do
      -- Note: If there is a need to filter the observable state by tag. "tag" can be found in the result of "contents" lens
      let txFeeDetails = obj ^. key "cicCurrentState" . key "observableState" . key "Right"
            . key "contents" . nth 0 . key "txFee" . key "getValue" . nth 0 . nth 1 . nth 0 . _Array
          aesArr = obj ^. key "cicCurrentState" . key "observableState" . key "Right"
            . key "contents" . _Array
          scrSize = fromMaybe (Aeson.Number 0) $ lastMay $ V.toList aesArr
      print $ "fetchObservableStateFees: the value of txFeeDetails is: " ++ (show txFeeDetails)
      return $ Right $ (Aeson.Array txFeeDetails, scrSize)

-- | Grabs `observableState` field from the contract instance status endpoint.
-- This is used to see smart contract's response to latest request processed.
callFunds
  :: Manager
  -> ContractInstanceId Text
  -> IO ()
callFunds httpManager contractId = do
  let requestUrl = "http://localhost:8080/api/new/contract/instance/" <> (unContractInstanceId contractId) <> "/endpoint/funds"
      reqBody = "[]"
  initReq <- parseRequest $ T.unpack requestUrl
  let req = initReq
        { method = "POST"
        , requestHeaders = ("Content-Type","application/json"):(requestHeaders initReq)
        , requestBody = RequestBodyLBS reqBody
        }
  _ <- httpLbs req httpManager
  return ()

-- | Grabs `observableState` field from the contract instance status endpoint.
-- This is used to see smart contract's response to latest request processed.
callPools
  :: Manager
  -> ContractInstanceId Text
  -> IO ()
callPools httpManager contractId = do
  let requestUrl = "http://localhost:8080/api/new/contract/instance/" <> (unContractInstanceId contractId) <> "/endpoint/pools"
      reqBody = "[]"
  initReq <- parseRequest $ T.unpack requestUrl
  let req = initReq
        { method = "POST"
        , requestHeaders = ("Content-Type","application/json"):(requestHeaders initReq)
        , requestBody = RequestBodyLBS reqBody
        }
  _ <- httpLbs req httpManager
  return ()

estimateTransactionFee
  :: MonadIO m
  => Pool Pg.Connection
  -> SmartContractAction
  -> m Integer
estimateTransactionFee pool action = case action of
  SmartContractAction_Swap -> do
    -- Perform Multiple regression on data set to estimate transaction fee
    (regressionResults, _preds, _res) <- runNoLoggingT $ runDb (Identity pool) $ runBeamSerializable $ do
      -- Query for all swaps that have occurred in order to construct a data set
      previousTxDataSet <- runSelectReturningList
        $ select
        $ filter_ (\a -> _txFeeDataSet_smartContractAction a ==. (val_ "Swap"))
        $ all_ (_db_txFeeDataSet db)
      txFees :: [Double] <- forM previousTxDataSet $ \txData -> return $ fromIntegral $ _txFeeDataSet_txFee txData
      let responder = U.fromList txFees
      scriptSizes <- forM previousTxDataSet $ \txData -> return $ fromIntegral $ _txFeeDataSet_scriptSize txData
      let preds :: U.Vector Double = U.fromList scriptSizes
          predictors = fmap (\_ -> preds) ([0] :: [Integer])
      if (predictors == [] || responder == U.empty || length(predictors) < 2)
         then return (Nothing, predictors, responder)
         else return $ (Just $ olsRegress predictors responder, predictors, responder)
    case regressionResults of
      Nothing -> return 10
      Just (leastSquaresVector, _goodnessOfFit) -> do
        -- This is the y-intercept, for now it will always come out to the correct answer
        return $ round $ last $ U.toList leastSquaresVector

-- | Run a 'MonadBeam' action inside a 'Serializable' transaction. This ensures only safe
-- actions happen inside the 'Serializable'
runBeamSerializable
  :: (forall m
      . ( MonadBeam Postgres m
        , MonadBeamInsertReturning Postgres m
        , MonadBeamUpdateReturning Postgres m
        , MonadBeamDeleteReturning Postgres m
        )
      => m a)
  -> Serializable a
runBeamSerializable action = unsafeMkSerializable $ liftIO . flip runBeamPostgres action =<< ask

buildStaticSwapTransaction :: Text -> IO (Either String Text)
buildStaticSwapTransaction changeAddress = do
  dir <- getCurrentDirectory
  print $ "buildStaticSwapTransaction: Start, current dir is " <> (T.pack $ show dir)
  print $ "buildStaticSwapTransaction: value of changeAddress is " <> changeAddress
  -- use cardano-cli to query for utxos at given address
  let cardanoCliExe = "/home/zigpolymath/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/cardano-node/result/alonzo-testnet/cardano-cli/bin/cardano-cli"
      testnetMagic = "1097911063"
      nodeSocketPath =
        "./node.sock"
  (_, cliOut, cliErr) <- readCreateProcessWithExitCode
    (CreateProcess
      {
        cmdspec = RawCommand
          cardanoCliExe
          ["query", "utxo", "--address", (T.unpack changeAddress), "--testnet-magic", testnetMagic]
      , cwd = Nothing
      , env = Just $ [(("CARDANO_NODE_SOCKET_PATH" :: String)
                   ,nodeSocketPath)]
      , std_in = Inherit
      , std_out = Inherit
      , close_fds = False
      , create_group = False
      , delegate_ctlc = False
      , detach_console = False
      , create_new_console = False
      , new_session = False
      , child_group = Nothing
      , child_user = Nothing
      , use_process_jobs = False
      }) []
  print $ "buildStaticSwapTransaction: value of cliOut is " <> (T.pack $ show cliOut)
  let nodeQueryResults = catMaybes $ flip map (map T.pack $ lines cliOut) $ \utxoInfo -> do
        case T.words utxoInfo of
          txHash:txix:amount:unit:_ -> Just (txHash <> "#" <> txix, amount, unit)
          _ -> Nothing
      -- Nami Wallet creates a collateral Utxo with 5 ada. This will search for the collateral utxo and return it's transaction hash and index
      collateralTxHash :: Text = maybe ""  (\(a, _, _) -> a)  $ headMay $ filter (\(txHashIx, amount, unit) -> amount == ("5000000" :: Text) && unit == ("lovelace" :: Text)) nodeQueryResults
      -- Get transaction hash of utxo with highest lovelace balance
      highestBalance = maximum $ flip map (filter (\(_,_,unit) -> unit == "lovelace") nodeQueryResults) $ \(_, amt, _) -> case decimal amt  of
        Right (amt', _) -> amt'
        Left _ -> 0
      txHash =  maybe "" (\(a, _, _) -> a) $ headMay $ flip filter nodeQueryResults $ \(txHashIdx, amt, unit) -> amt == (T.pack $ show highestBalance) && unit == "lovelace"
  -- drop leading txHash and txIndex, keep balances of what is being held at the utxo handle
  print $ "buildStaticSwapTransaction: value of collateralTxHash is " <> collateralTxHash
  print $ "buildStaticSwapTransaction: value of txHash is " <> txHash

  -- TODO: uniswapPlutusScript should come from a config file
  -- TODO: rawSwap-redeemer should come from a config file
  -- TODO: poolDatums should come from cardano-cli query or SQL database(the former is better) should come from a config file
  -- TODO: any hardcoded utxo handles need to be auto selected or passed in
  (exitCode, stdIn, err) <- readCreateProcessWithExitCode
    (CreateProcess
      {
        cmdspec = RawCommand
          "./scripts/obelisk-handleSwap.sh"
          [(T.unpack txHash)
          , "/home/zigpolymath/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/uniswapPlutusScript.plutus"
          , "/home/zigpolymath/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/firstRawSwap/poolDatum.plutus"
          , "/home/zigpolymath/Documents/Obsidian/bobTheBuilder8/plutus-use-cases/use-case-2/dep/plutus/plutus-use-cases/rawSwap/rawSwap-redeemer"
          , (T.unpack collateralTxHash)
          , (T.unpack uniswapScriptAddress)
          , "lovelace"
          , (T.unpack changeAddress)
          , "./keys/payment.skey"
          , "555d92eb7e540ffc4d9b6a80becce5429737a5787c54d0f28c8ecb5f9d174774#1"
          , "3fe5e587c36a29b6151154fd6cf3b0226d172984196c9bf8350d8962e7aa5710#1"
          , "75a70a2a2e897886839a7a30935c2aa36a27406c5e67d21d8f749df5.PoolState"
          ]
      , cwd = Nothing
      , env = Just $ [(("CARDANO_NODE_SOCKET_PATH" :: String) ,nodeSocketPath)]
      , std_in = Inherit
      , std_out = Inherit
      , close_fds = False
      , create_group = False
      , delegate_ctlc = False
      , detach_console = False
      , create_new_console = False
      , new_session = False
      , child_group = Nothing
      , child_user = Nothing
      , use_process_jobs = False
      }) []
  case exitCode of
    ExitFailure _ -> do
      print $ show err
      -- TODO: Based on the exit code, determine whether to return Right or Left
      return $ Left $ "Transaction not built: " ++ (show err)
    ExitSuccess -> do
      -- open file to fetch cbor hash from file generated by cardano-cli during handleSwap script
      builtTxBytes <- LBS.readFile "obelisk-rawSwap-tx-signed"
      let eBuiltTx :: Either String BuiltTx = Aeson.eitherDecode builtTxBytes
      case eBuiltTx of
        Left err -> return $ Left err
        -- Return the CBOR TxHash to be passed into Nami Wallet
        Right builtTx -> do
          let cborHex =  _builtTx_cborHex builtTx
          print $ "buildStaticSwapTransaction: value of cborHex is " <> cborHex
          return $ Right cborHex

