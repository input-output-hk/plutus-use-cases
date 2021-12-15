{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import Control.Exception
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson(FromJSON(..), ToJSON(..), (.:))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isNumber)
import Data.Dependent.Sum
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Pool
import Data.Proxy
import Data.Semigroup (First(..))
import Data.Text (Text)
import Data.Text.Read (decimal)
import qualified Data.Text as T
import Data.Vessel
import Database.Beam (MonadBeam, Generic)
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Query
import qualified Database.PostgreSQL.Simple as Pg
import Gargoyle.PostgreSQL.Connect
import Obelisk.Backend
import Obelisk.ExecutableConfig.Lookup
import Obelisk.Route
import Rhyolite.Backend.App
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.Serializable
import Rhyolite.Backend.Listen
import Safe (headMay)
import Snap.Util.FileServe
import System.Directory
import System.Exit
import System.Process

import Backend.Notification
import Backend.Schema
import Common.Api
import Common.Route
import Common.Schema

import Network.HTTP.Client hiding (Proxy)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      httpManager <- newManager defaultManagerSettings
      withDb "db" $ \pool -> do
        withResource pool runMigrations
        configs <- getConfigs
        (handleListen, finalizeServeDb) <- serveDbOverWebsockets
          pool
          (requestHandler configs httpManager pool)
          (\(nm :: DbNotification Notification) q ->
             fmap (fromMaybe emptyV) $ mapDecomposedV (notifyHandler nm) q)
          (QueryHandler $ \q -> fmap (fromMaybe emptyV) $ mapDecomposedV (queryHandler pool) q)
          vesselFromWire
          vesselPipeline -- (tracePipeline "==> " . vesselPipeline)
        flip finally (finalizeServeDb) $ serve $ \case
          BackendRoute_Listen :/ () -> handleListen
          BackendRoute_WASM :/ fpath -> do
            let (_, extension) = T.breakOn "." $ fromMaybe mempty $ listToMaybe $ reverse fpath
                mimetype = case extension of
                  ".js" -> "text/javascript"
                  ".wasm" -> "application/wasm"
                  ".module.wasm" -> "application/wasm"
                  _ -> mempty
            serveFileAs mimetype $ T.unpack $ "static/" <> T.intercalate "/" fpath
          _ -> return ()

          _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }

swapProposalDir :: Text
swapProposalDir = "./swap-proposals"

swapHistoryDir :: Text
swapHistoryDir = "./swap-history"

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

data RecentPoolBalance = RecentPoolBalance
  { pcA :: Integer
  , pcB :: Integer
  , stamp :: Text
  } deriving (Show, Generic)

instance FromJSON RecentPoolBalance
instance ToJSON RecentPoolBalance

data DappBackendConfig = DappBackendConfig
  { _dappBackendConfig_cardanoCliExe :: Text
  , _dappBackendConfig_cardanoNodeSocket :: Text
  , _dappBackendConfig_testnetMagicNumber :: Text
  , _dappBackendConfig_contractOwnerSignKeyPath :: Text
  , _dappBackendConfig_contractScriptAddress :: Text
  , _dappBackendConfig_contractScriptPath :: Text
  , _dappBackendConfig_initPoolValA :: Integer -- TODO: Won't be needed when datum is no longer referenced from file
  , _dappBackendConfig_initPoolValB :: Integer -- TODO: Won't be needed when datum is no longer referenced from file
  } deriving (Show, Generic)

instance FromJSON DappBackendConfig
instance ToJSON DappBackendConfig

-- | Handle requests / commands, a standard part of Obelisk apps.
requestHandler :: Map Text ByteString -> Manager -> Pool Pg.Connection -> RequestHandler Api IO
requestHandler configs _httpManager _pool = RequestHandler $ \case
  Api_BuildStaticSwapTransaction walletAddress adaAmount -> buildStaticSwapTransaction configs walletAddress adaAmount
  Api_ConfirmSwapSuccess proposalId -> confirmSwapSuccess proposalId
  Api_EstimateSwap tokenAInput -> estimateSwapOutput configs tokenAInput

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

getConfig :: Map Text ByteString -> Text -> IO ByteString
getConfig m k = case Map.lookup k m of
  Nothing -> fail . T.unpack $ "Couldn't find config/" <> k
  Just x -> return x

getJsonConfig :: (FromJSON a) => Map Text ByteString -> Text -> IO a
getJsonConfig m k = do
  bs <- getConfig m k
  case Aeson.eitherDecode' (LBS.fromStrict bs) of
    Left e -> fail . T.unpack $ "Error while JSON decoding config/" <> k <> ": " <> (T.pack (show e))
    Right x -> return x


buildStaticSwapTransaction
  :: Map Text ByteString
  -> Text
  -> Integer
  -> IO (Either String (Text, Text)) -- In Right case (CBOR Hash, SwapProposalId)
buildStaticSwapTransaction configs changeAddress adaAmount = do
  print $ "buildStaticSwapTransaction: value of configs is " <> (show configs)

  -- fetch configs to perform swap
  dappBackendConf <- getJsonConfig configs "backend/dappBackendConfig.json"
  let uniswapScriptAddress = _dappBackendConfig_contractScriptAddress dappBackendConf
      uniswapScriptPath = _dappBackendConfig_contractScriptPath dappBackendConf
      cardanoCliExe = _dappBackendConfig_cardanoCliExe dappBackendConf
      testnetMagic = _dappBackendConfig_testnetMagicNumber dappBackendConf
      nodeSocketPath = _dappBackendConfig_cardanoNodeSocket dappBackendConf
      smartContractOwnerSignKeyFilePath = _dappBackendConfig_contractOwnerSignKeyPath dappBackendConf

  print $ "buildStaticSwapTransaction: value of changeAddress is " <> changeAddress
  -- query cli for available utxos from client's provided address
  (_, cliOut, _) <- readCreateProcessWithExitCode
    (CreateProcess
      {
        cmdspec = RawCommand
          (T.unpack cardanoCliExe)
          ["query", "utxo", "--address", (T.unpack changeAddress), "--testnet-magic", (T.unpack testnetMagic)]
      , cwd = Nothing
      , env = Just $ [(("CARDANO_NODE_SOCKET_PATH" :: String), (T.unpack nodeSocketPath))]
      , std_in = Inherit
      , std_out = Inherit
      , std_err = Inherit
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
          utxoHandle:txix:amount:unit:_ -> Just (utxoHandle <> "#" <> txix, amount, unit)
          _ -> Nothing
      -- Nami Wallet creates a collateral Utxo with 5 ada. This will search for the collateral utxo and return it's transaction hash and index
      -- TODO: This should be fetched from Nami Wallet
      collateralTxHash :: Text = maybe ""  (\(a, _, _) -> a)  $
        headMay $ filter (\(_, amount, unit) -> amount == ("5000000" :: Text) && unit == ("lovelace" :: Text)) nodeQueryResults
      -- Get transaction hash of utxo with highest lovelace balance
      highestBalance :: Integer = maximum $ flip map (filter (\(_,_,unit) -> unit == "lovelace") nodeQueryResults) $ \(_, amt, _) -> case decimal amt  of
        Right (amt', _) -> amt'
        Left _ -> 0
      txHash =  maybe "" (\(a, _, _) -> a) $ headMay $ flip filter nodeQueryResults $ \(_, amt, unit) ->
        amt == (T.pack $ show highestBalance) && unit == "lovelace"
  -- drop leading txHash and txIndex, keep balances of what is being held at the utxo handle
  print $ "buildStaticSwapTransaction: value of collateralTxHash is " <> collateralTxHash
  print $ "buildStaticSwapTransaction: value of txHash is " <> txHash

  -- Fetch Pool Utxo and PoolState
  (_, poolCliOut, poolErr) <- readCreateProcessWithExitCode
    (CreateProcess
      {
        cmdspec = RawCommand
          (T.unpack cardanoCliExe)
          ["query", "utxo", "--address", (T.unpack uniswapScriptAddress), "--testnet-magic", (T.unpack testnetMagic)]
      , cwd = Nothing
      , env = Just $ [(("CARDANO_NODE_SOCKET_PATH" :: String) ,(T.unpack nodeSocketPath))]
      , std_in = Inherit
      , std_out = Inherit
      , std_err = Inherit
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
  -- TODO: This could be the source a bug if the hash prepending "PoolState" is not specified AND there are multiple pools
  let poolNodeQueryResults = catMaybes $ flip map (flip filter (map T.pack $ lines poolCliOut) $ \utxoLine -> T.isInfixOf "PoolState" utxoLine) $ \utxoInfo -> do
        case T.words utxoInfo of
          utxoHandle:txix:otherAmounts -> do
            let poolState = headMay $ flip filter otherAmounts $ \assets -> T.isSuffixOf "PoolState" assets
            Just ((utxoHandle <> "#" <> txix), poolState)
          _ -> Nothing
      arbitraryTokenName = "OBSIDIAN"
      -- get token currency symbol and token name from cardano-cli output
      tokenInfoQueryResults = headMay $ catMaybes $ flip map (flip filter (map T.pack $ lines poolCliOut) $ \utxoLine -> T.isInfixOf arbitraryTokenName utxoLine) $ \utxoInfo -> do
        case T.words utxoInfo of
          _:_:otherAmounts -> do
            let tokenInfo = fromMaybe "" $ headMay $ flip filter otherAmounts $ \assets -> T.isSuffixOf arbitraryTokenName assets
            case T.splitOn "." tokenInfo of
              tkCurrencySymbol:tkName:_ -> Just (tkCurrencySymbol, tkName)
              _ -> Nothing
          _ -> Nothing
      -- Note: For POC there is only one pool
      demoPool = headMay poolNodeQueryResults
  print $ "buildStaticSwapTransaction: value of tokenInfoQueryResults is " <> (show tokenInfoQueryResults)
  case (demoPool, tokenInfoQueryResults) of
    (Nothing, _) -> do
      print $ "buildStaticSwapTransaction: error: value of poolErr is " <> (show poolErr)
      return $ Left "PoolState not found"
    (_, Nothing) -> do
      print $ "buildStaticSwapTransaction: error: value of tokenInfoQueryResults is " <> (show tokenInfoQueryResults)
      return $ Left "Token Info not found"
    (Just (poolUtxo, mPoolStateSymbol), Just (tokenBCurrencySymbol, tokenBName)) -> do
      case mPoolStateSymbol of
        Nothing -> return $ Left "PoolStateSymbol not found"
        Just poolStateSymbol -> do
          -- Determine the TxOut for the Uniswap smart contract
          -- run Uniswap Validator to figure out which assets are suppose to be returned to the uniswap smart contract
          currentDir <- getCurrentDirectory
          let validatorExe = currentDir ++ "/static/plutus-raw-swap" -- TODO: get abs path of plutus-raw-swap exe better than this -- $(static "./static/plutus-raw-swap")
          print $ "buildStaticSwapTransaction: value of validatorExe is " <> (show validatorExe)
          -- check for history for current pool datum file
          createDirectoryIfMissing False $ T.unpack swapHistoryDir
          historyFiles :: [String] <- getDirectoryContents $ T.unpack swapHistoryDir
          let mCurrentDatumFile = headMay $ filter (\f -> (T.isSuffixOf ".current" f)) $ T.pack <$> historyFiles
          case mCurrentDatumFile of
            Nothing -> return $ Left "ob-backend: Pool Datum not found" -- Manual intervention required to get current poolDatum.plutus
            Just currentDatumFile -> do
              let poolDatumFilePath = swapHistoryDir <> "/" <> currentDatumFile
              let lovelaceAmount = adaAmount * 1000000
              print $ "buildStaticSwapTransaction: value of lovelaceAmount to be swapped is " <> (show lovelaceAmount)
              (_, validatorOut, validatorErr) <- readCreateProcessWithExitCode
                (CreateProcess
                  {
                    cmdspec = RawCommand
                      validatorExe
                      [ "" -- currency symbol for ADA, Should come from Pool Datum decoding
                      , "" -- tokenName for ADA, Should come from Pool Datum decoding
                      , (show lovelaceAmount)
                      , (T.unpack tokenBCurrencySymbol)
                      , (T.unpack tokenBName)
                      , "0" -- TODO: This amount should be specified by the user when swapping from arbitrary token to ada is available
                      , (T.unpack poolDatumFilePath)
                      , (T.unpack cardanoCliExe)
                      , (T.unpack nodeSocketPath)
                      , (T.unpack uniswapScriptAddress)
                      , (T.unpack testnetMagic)
                      , (T.unpack swapProposalDir)
                      ]
                  , cwd = Nothing
                  , env = Just $ [(("CARDANO_NODE_SOCKET_PATH" :: String) ,(T.unpack nodeSocketPath))]
                  , std_in = Inherit
                  , std_out = Inherit
                  , std_err = Inherit
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
              print $ "buildStaticSwapTransaction: value of validatorOut is " <> (show validatorOut)
              print $ "buildStaticSwapTransaction: value of validatorErr is " <> (show validatorErr)
              -- open recentPoolBalance.json file generated by validator and retreive expected token pool balances for txOut
              let recentPoolPath = (T.unpack swapProposalDir) <> "/recentPoolBalance.json"
              rpbExist <- doesPathExist recentPoolPath
              recentPoolBalanceBytes <- if rpbExist
                then LBS.readFile recentPoolPath
                else do
                  createDirectoryIfMissing False (T.unpack swapProposalDir)
                  let rpb = RecentPoolBalance (_dappBackendConfig_initPoolValA dappBackendConf) (_dappBackendConfig_initPoolValB dappBackendConf) ""
                  LBS.writeFile recentPoolPath $ Aeson.encode rpb
                  LBS.readFile recentPoolPath
              let eRecentPoolBalance :: Either String RecentPoolBalance = Aeson.eitherDecode' recentPoolBalanceBytes
              case eRecentPoolBalance of
                Left err -> return $ Left err
                -- Return the CBOR TxHash to be passed into Nami Wallet
                Right recentPoolBalance -> do
                  let contractTxOut = T.unpack $ (T.pack $ show $ pcA recentPoolBalance)
                        <> " lovelace + "
                        <> (T.pack $ show $ pcB recentPoolBalance)
                        <> " " <> tokenBCurrencySymbol <> "." <> tokenBName
                      swapProposalId = stamp recentPoolBalance
                  print $ "buildStaticSwapTransaction: value of contractTxOut is " <> (show contractTxOut)

                  -- Determine the TxOut for the Nami client's wallet
                  let swapRedeemerFilePath = swapProposalDir <> "/" <> swapProposalId <> "-rawSwap-redeemer"
                      handleSwapProcess namiTxOut uniswapTxOut = CreateProcess
                        { cmdspec = RawCommand
                            "./scripts/obelisk-handleSwap.sh"
                            [(T.unpack txHash)
                            , (T.unpack uniswapScriptPath)
                            , (T.unpack poolDatumFilePath)
                            , (T.unpack swapRedeemerFilePath)
                            , (T.unpack collateralTxHash) -- TODO: this should be determined by Nami Wallet
                            , (T.unpack uniswapScriptAddress)
                            , "lovelace"
                            , (T.unpack changeAddress)
                            , (T.unpack smartContractOwnerSignKeyFilePath)
                            , "9502aa61684bd443d69eba5999a138e4dc4186d7335efc14a51db46ac3f1f2b6#1" -- TODO: We don't need this argument anymore, update ./scripts/obelisk-handleSwap.sh
                            , (T.unpack poolUtxo)
                            , (T.unpack poolStateSymbol)
                            , namiTxOut
                            , uniswapTxOut
                            ]
                        , cwd = Nothing
                        , env = Just $ [(("CARDANO_NODE_SOCKET_PATH" :: String) ,(T.unpack nodeSocketPath))]
                        , std_in = Inherit
                        , std_out = Inherit
                        , std_err = Inherit
                        , close_fds = False
                        , create_group = False
                        , delegate_ctlc = False
                        , detach_console = False
                        , create_new_console = False
                        , new_session = False
                        , child_group = Nothing
                        , child_user = Nothing
                        , use_process_jobs = False
                        }
                  -- txout lovelace balance used to cause cardano-cli to correct itself and we retreive the correct txout balances in subsequent cardano cli request
                  let initLovelacePlaceholder = "+ 1 lovelace"
                  print $ "buildStaticSwapTransaction: value of poolUtxo is " <> poolUtxo
                  -- NOTE: There are no automatic transaction balancing libs or tools that can be trusted at the moment. The idea here is to
                    -- allow cardano-cli to say what the transaction outputs should be by trail and error of the cardano-cli command itself.
                    -- The plan is to run cardano-cli 4 times, each iteration gets us closer to what the nami wallet client's txout is expected to be.
                  -- First Attempt to build transaction, expecting to fail and extract the minimum required lovelace for this tx
                  (_, _, errOut1) <- readCreateProcessWithExitCode (handleSwapProcess initLovelacePlaceholder contractTxOut) []
                  -- retreive minimumRequiredUtxo from stdErr
                  print $ "buildStaticSwapTransaction: value of errOut1 is " <> errOut1
                  let mMinimumRequiredUtxo = fmap (T.unwords . reverse . T.words . T.toLower . T.dropWhile (\c -> c /= 'L')) $ headMay $
                        flip filter (T.lines $ T.pack errOut1) $ \out -> T.isPrefixOf "Minimum required UTxO" out
                  case mMinimumRequiredUtxo of
                    Nothing -> return $ Left "Minimum required utxo for Nami Wallet Client TxOut could not be established"
                    Just minimumRequiredUtxo -> do
                      print $ "buildStaticSwapTransaction: value of minimumRequiredUtxo  is " <> (T.pack $ show minimumRequiredUtxo)
                      -- Second Attempt to build transaction, expecting to fail and extract the unbalanced ada and non-ada assets
                      (_, _, errOut2) <- readCreateProcessWithExitCode (handleSwapProcess (" + " <> (T.unpack minimumRequiredUtxo)) contractTxOut) []
                      let mBalancedAssets = fmap (T.dropWhile (\c -> not $ isNumber c)) $ headMay $ flip filter (T.lines $ T.pack errOut2) $ \out -> T.isPrefixOf "Command failed" out
                      print $ "buildStaticSwapTransaction: value of mBalancedAssets  is " <> (T.pack $ show mBalancedAssets)
                      case mBalancedAssets of
                        Nothing -> return $ Left "Unable to balance Nami Wallet Client TxOut assets"
                        Just balancedAssets -> do
                          let minimumLovelace :: Integer = either (\_ -> 0) fst $ decimal $ T.takeWhile (\c -> isNumber c) minimumRequiredUtxo
                              balancedLovelace :: Integer = either (\_ -> 0) fst $ decimal $ T.takeWhile (\c -> isNumber c) balancedAssets
                              summedLovelace = minimumLovelace + balancedLovelace --TODO: At risk of overflow?
                              remainingAssets = T.dropWhile (\c -> isNumber c) balancedAssets
                          print $ "buildStaticSwapTransaction: value of minimumLovelace  is " <> (T.pack $ show minimumLovelace)
                          print $ "buildStaticSwapTransaction: value of balancedLovelace  is " <> (T.pack $ show balancedLovelace)
                          print $ "buildStaticSwapTransaction: value of summedLovelace  is " <> (T.pack $ show summedLovelace)
                          print $ "buildStaticSwapTransaction: value of remainingAssets  is " <> (T.pack $ show remainingAssets)
                          -- Third Attempt to build transaction, expecting to fail to get what might be the tx fee
                          (_, _, errOut3) <- readCreateProcessWithExitCode (handleSwapProcess (" + " <> (show summedLovelace) <> (T.unpack remainingAssets)) contractTxOut) []
                          let mTxFee = fmap ((T.takeWhile (\c -> isNumber c)) . (T.dropWhile (\c -> not $ isNumber c))) $
                                headMay $ flip filter (T.lines $ T.pack errOut3) $ \out -> T.isPrefixOf "Command failed" out
                          print $ "buildStaticSwapTransaction: value of txFeeError  is " <> (T.pack $ show mTxFee)
                          case mTxFee of
                            Nothing -> return $ Left "Unable to determine tx fee"
                            Just txFee -> do
                              let namiClientLovelaceAmount = summedLovelace - (either (\_ -> 0) fst $ decimal $ txFee)
                              print $ "buildStaticSwapTransaction: value of namiClientLovelaceAmount  is " <> (T.pack $ show namiClientLovelaceAmount)
                              -- Fourth Attempt to build transaction, expecting to succeed
                              (exitCode, _, errOut) <- readCreateProcessWithExitCode (handleSwapProcess (" + " <> (show namiClientLovelaceAmount) <> (T.unpack remainingAssets)) contractTxOut) []
                              case exitCode of
                                ExitFailure _ -> do
                                  print $ show errOut
                                  return $ Left $ "Transaction not built: " ++ (show errOut)
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
                                      return $ Right (cborHex, swapProposalId)

confirmSwapSuccess
  :: Text
  -> IO (Either String ())
confirmSwapSuccess proposalId = do
  -- check for history for current pool datum file
  historyFiles :: [String] <- getDirectoryContents $ T.unpack swapHistoryDir
  let mCurrentDatumFile = headMay $ filter (\f -> (T.isSuffixOf ".current" f)) $ T.pack <$> historyFiles
  -- copy over new current pool datum file
  dirFiles :: [String] <- getDirectoryContents $ T.unpack swapProposalDir
  let mDatumFile = headMay $ filter (\f -> (T.isPrefixOf proposalId f) && (T.isSuffixOf "poolDatum.plutus" f)) $ T.pack <$> dirFiles
  print $ "confirmSwapSuccess: value of mDatumFile is " <> (show mDatumFile)
  case mDatumFile of
    Nothing -> return $ Left "Unable to locate datum proposal"
    Just datumFile -> do
      -- change name of current datum file
      case mCurrentDatumFile of
        Nothing -> print $ ("confirmSwapSuccess: no current datum file found, congrats on your first swap!" :: Text)
        Just currentDatumFile -> do
          print $ "confirmSwapSuccess: value of currentDatumFile is " <> (show currentDatumFile)
          let ammendedFileName = fst $ T.breakOnEnd ".current" currentDatumFile
          print $ "confirmSwapSuccess: renaming datum file to  " <> (show ammendedFileName)
          renameFile (T.unpack $ swapHistoryDir <> "/" <> currentDatumFile) (T.unpack $ swapHistoryDir <> "/" <> ammendedFileName)
      -- update current datum file
      copyFile (T.unpack $ swapProposalDir <> "/" <> datumFile) (T.unpack $ swapHistoryDir <> "/" <> datumFile <> ".current")
      return $ Right ()

estimateSwapOutput
  :: Map Text ByteString
  -> Integer
  -> IO (Either String Integer)
estimateSwapOutput configs tokenAInput = do
  dappBackendConf <- getJsonConfig configs "backend/dappBackendConfig.json"
  print $ "estimateSwapOutput: val of tokenAInput is " <> (show tokenAInput)
  -- TODO: Pool State should be retreived from Pool Datum File or Pool Datum Hash instead of this .json file created by validator script
  let recentPoolPath = (T.unpack swapProposalDir) <> "/recentPoolBalance.json"
  rpbExist <- doesPathExist recentPoolPath
  recentPoolBalanceBytes <- if rpbExist
    then LBS.readFile recentPoolPath
    else do
      createDirectoryIfMissing False (T.unpack swapProposalDir)
      let rpb = RecentPoolBalance (_dappBackendConfig_initPoolValA dappBackendConf) (_dappBackendConfig_initPoolValB dappBackendConf) ""
      LBS.writeFile recentPoolPath $ Aeson.encode rpb
      LBS.readFile recentPoolPath
  let eRecentPoolBalance :: Either String RecentPoolBalance = Aeson.eitherDecode' recentPoolBalanceBytes
  case eRecentPoolBalance of
    Left err -> do
      print $ "estimateSwapOutput: error " <> (show err)
      return $ Left err
    Right recentPoolBalance -> do
      print $ "estimateSwapOutput: val of (pcA recentPoolBalance) is " <> (show (pcA recentPoolBalance))
      print $ "estimateSwapOutput: val of (pcB recentPoolBalance) is " <> (show (pcB recentPoolBalance))
      let adaToLovelace amt = amt * 1000000
      let estimatedAmount = findSwapA (pcA recentPoolBalance) (pcB recentPoolBalance) (adaToLovelace tokenAInput)
      print $ "estimateSwapOutput: val of estimatedAmount is " <> (show estimatedAmount)
      return $ Right estimatedAmount
  -- validation helper functions from Uniswap.OffChain
  where
    traceIfFalse :: String -> Bool -> Bool
    traceIfFalse _ a = if a then True else False

    checkSwap
      :: Integer -- Amount A
      -> Integer -- Amount B
      -> Integer -- Amount A
      -> Integer -- Amount B
      -> Bool
    checkSwap oldA oldB newA newB =
        traceIfFalse "expected positive oldA" (oldA > 0) &&
        traceIfFalse "expected positive oldB" (oldB > 0) &&
        traceIfFalse "expected positive-newA" (newA > 0) &&
        traceIfFalse "expected positive-newB" (newB > 0) &&
        traceIfFalse "expected product to increase"
            ((((newA * feeDen) - (inA * feeNum)) * ((newB * feeDen) - (inB * feeNum)))
             >= (feeDen * feeDen * oldA * oldB))
      where
        -- use max 0 just in case calculation result is less than 0
        inA = max 0 $ newA - oldA
        inB = max 0 $ newB - oldB
        -- The uniswap fee is 0.3%; here it is multiplied by 1000, so that the
        -- on-chain code deals only in integers.
        -- See: <https://uniswap.org/whitepaper.pdf> Eq (11) (Page 7.)
        feeNum, feeDen :: Integer
        feeNum = 3
        feeDen = 1000

    findSwapA
      :: Integer -- original total of first coin in liquidity
      -> Integer -- original total of second coin in liquidity
      -> Integer -- amount of first coin being swapped for second coin
      -> Integer -- amount of second coin recieved
    findSwapA oldA oldB inA
        | ub' <= 1   = 0
        | otherwise  = go 1 ub'
      where
        -- check if swap is valid and fee is computed correctly
        cs :: Integer -> Bool
        cs outB = checkSwap oldA oldB (oldA + inA) (oldB - outB)

        -- find the first integer where checkswap would be invalid
        ub' :: Integer
        ub' = head $ dropWhile cs [2 ^ i | i <- [0 :: Int ..]]

        -- determine amount to swap for
        go :: Integer -> Integer -> Integer
        go lb ub
            | ub == (lb + 1) = lb
            | otherwise      =
          let
            m = div (ub + lb) 2
          in
            if cs m then go m ub else go lb m

