{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Backend where

import Control.Exception
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson.Lens
import qualified Data.Aeson as Aeson
import Data.Dependent.Sum
import Data.Maybe
import Data.Pool
import Data.Proxy
import Data.Semigroup (First(..))
import Data.Vessel
import Database.Beam (MonadBeam)
import Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam.Postgres
import Database.Beam.Query
import qualified Database.PostgreSQL.Simple as Pg
import Gargoyle.PostgreSQL.Connect
import Obelisk.Backend
import Obelisk.Route
import Rhyolite.Backend.App
import Rhyolite.Backend.DB
import Rhyolite.Backend.DB.Serializable
import Rhyolite.Backend.Listen

import Backend.Notification
import Backend.Schema
import Common.Api
import Common.Route
import Common.Schema

import Network.HTTP.Client hiding (Proxy)
import Control.Lens

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      withDb "db" $ \pool -> do
        getWallets pool
        withResource pool runMigrations
        withResource pool $ \conn -> runBeamPostgres conn ensureCounterExists
        (handleListen, finalizeServeDb) <- serveDbOverWebsockets
          pool
          (requestHandler pool)
          (\(nm :: DbNotification Notification) q -> fmap (fromMaybe emptyV) $ mapDecomposedV (notifyHandler nm) q)
          (QueryHandler $ \q -> fmap (fromMaybe emptyV) $ mapDecomposedV (queryHandler pool) q)
          vesselFromWire
          vesselPipeline -- (tracePipeline "==> " . vesselPipeline)
        flip finally finalizeServeDb $ serve $ \case
          BackendRoute_Listen :/ () -> handleListen
          _ -> return ()
  , _backend_routeEncoder = fullRouteEncoder
  }

requestHandler :: Pool Pg.Connection -> RequestHandler Api IO
requestHandler pool = RequestHandler $ \case
  Api_IncrementCounter -> runNoLoggingT $ runDb (Identity pool) $ do
    rows <- runBeamSerializable $ do
      runUpdateReturningList $ update (_db_counter db)
        (\counter -> _counter_amount counter <-. current_ (_counter_amount counter) + val_ 1)
        (\counter -> _counter_id counter ==. val_ 0)
    mapM_ (notify NotificationType_Update Notification_Counter . _counter_amount) rows
  -- TODO: Make API for swap instead
  -- Api_ListWalletAccounts -> runNoLoggingT $ runDb (Identity pool) $ do
  --   -- TODO: acquire all cicContractIds from "http://localhost:8080/api/new/contract/instances"
  --   rows <- runBeamSerializable $ do
  --     runUpdateReturningList $ update (_db_counter db)
  --       (\counter -> _counter_amount counter <-. current_ (_counter_amount counter) + val_ 1)
  --       (\counter -> _counter_id counter ==. val_ 0)
  --   mapM_ (notify NotificationType_Update Notification_Counter . _counter_amount) rows

notifyHandler :: DbNotification Notification -> DexV Proxy -> IO (DexV Identity)
notifyHandler dbNotification _ = case _dbNotification_message dbNotification of
  Notification_Counter :=> Identity n -> do
    let val = case _dbNotification_notificationType dbNotification of
          NotificationType_Delete -> Nothing
          NotificationType_Insert -> Just n
          NotificationType_Update -> Just n
    return $ singletonV Q_Counter $ IdentityV $ Identity $ First val

queryHandler :: Pool Pg.Connection -> DexV Proxy -> IO (DexV Identity)
queryHandler pool v = buildV v $ \case
  Q_Counter -> \_ -> runNoLoggingT $ runDb (Identity pool) $ runBeamSerializable $ do
    counter <- runSelectReturningOne $ lookup_ (_db_counter db) (CounterId 0)
    return $ IdentityV $ Identity $ First $ _counter_amount <$> counter
  Q_Counter -> \_ -> runNoLoggingT $ runDb (Identity pool) $ runBeamSerializable $ do
    -- TODO: ask mock chain for contract instances
    -- TODO: store contract instances in DB
    counter <- runSelectReturningOne $ lookup_ (_db_counter db) (CounterId 0)
    return $ IdentityV $ Identity $ First $ _counter_amount <$> counter

getWallets :: Pool Pg.Connection -> IO ()
getWallets pool = do
  initReq <- parseRequest "http://localhost:8080/api/new/contract/instances"
  let req = initReq { method = "GET" }
  httpManager <- newManager defaultManagerSettings
  resp <- httpLbs req httpManager
  let val = Aeson.eitherDecode (responseBody resp) :: Either String Aeson.Value
  case val of
    Left _ -> return () -- TODO: Handle error properly
    Right obj -> do
      let contractInstanceIds = obj ^.. values . key "cicContract". key "unContractInstanceId" . _String
          walletIds = obj ^.. values . key "cicWallet". key "getWallet" . _Integer
          walletContracts = zipWith (\a b -> Contract a (fromIntegral b)) contractInstanceIds walletIds
      print walletContracts
      runNoLoggingT $ runDb (Identity pool) $ runBeamSerializable $ do
        runInsert $ insertOnConflict (_db_contracts db) (insertValues walletContracts)
          (conflictingFields _contract_id)
          onConflictDoNothing



  -- TODO: Parse response and place in DB
  return ()


ensureCounterExists :: MonadBeam Postgres m => m ()
ensureCounterExists = do
  runInsert $ insertOnConflict (_db_counter db) (insertValues [(Counter 0 0)])
    (conflictingFields _counter_id)
    onConflictDoNothing

-- | Run a 'MonadBeam' action inside a 'Serializable' transaction. This ensures only safe
-- actions happen inside the 'Serializable'
runBeamSerializable :: (forall m. (MonadBeam Postgres m, MonadBeamInsertReturning Postgres m, MonadBeamUpdateReturning Postgres m, MonadBeamDeleteReturning Postgres m) => m a) -> Serializable a
runBeamSerializable action = unsafeMkSerializable $ liftIO . flip runBeamPostgres action =<< ask
