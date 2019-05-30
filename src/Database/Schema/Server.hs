{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Database.Schema.Server
  ( server
  , genericServer
  , UpgradeAPI
  , UpgradeRequest(..)
  )
where

import           Control.Exception.Lifted                (catch)
import           Control.Monad                           (forM, void, (<=<))
import           Control.Monad.Except                    (ExceptT, MonadError,
                                                          runExceptT)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Control.Monad.Reader                    (runReaderT)
import           Control.Monad.Trans.Control             (MonadBaseControl)
import           Data.Aeson                              (FromJSON, ToJSON)
import           Data.String.Conversions                 (cs)
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Database.HDBC                           (SqlError)
import           Database.HDBC.PostgreSQL                (connectPostgreSQL)
import           Database.Schema.Migrations
import           Database.Schema.Migrations.Backend
import           Database.Schema.Migrations.Backend.HDBC (hdbcBackend)
import           Database.Schema.Migrations.Migration    (Migration (mId))
import           Database.Schema.Migrations.Store
import           Database.Schema.Migrations.Tarball      (TarballContents (..),
                                                          TarballStoreError,
                                                          tarballStore)
import           GHC.Generics                            (Generic)
import           Moo.CommandUtils                        (lookupMigration,
                                                          withBackend)
import           Moo.Core

import           Servant

data UpgradeRequest = UpgradeRequest
  { connString :: String
  , tarball    :: TarballContents
  } deriving (Generic, ToJSON, FromJSON)

data UpgradeServerError =
    LoadError [MapValidationError]
  | TarballStoreError TarballStoreError
  | SqlError SqlError
  deriving Show

type UpgradeAPI = ReqBody '[JSON] UpgradeRequest
               :> Post '[JSON] [Text]

server :: Server UpgradeAPI
server = hoistServer (Proxy @UpgradeAPI) nt genericServer
  where
  nt :: ExceptT UpgradeServerError Handler x -> Handler x
  nt = either (throwError . toServantError) pure <=< runExceptT

genericServer
  :: ( MonadBaseControl IO m
     , MonadIO m
     , MonadError UpgradeServerError m
     )
  => UpgradeRequest -> m [Text]
genericServer req = do

  connection <- liftIO $ connectPostgreSQL (connString req)
  let backend = hdbcBackend connection
  let command = upgrade

  case tarballStore $ tarball req of
    Right store -> do
      loadedStoreData <- liftIO $ loadMigrations store
      case loadedStoreData of
        Left es -> throwError $ LoadError es
        Right storeData -> do
          let st = AppState { _appOptions = commandOptions
                            , _appCommand = command
                            , _appRequiredArgs = []
                            , _appOptionalArgs = ["" :: Text]
                            , _appBackend = backend
                            , _appStore = store
                            , _appStoreData = storeData
                            , _appLinearMigrations = False
                            , _appTimestampFilenames = False
                            }
          applied <- liftIO (runReaderT (upgradeCommand storeData) st)
                      `catch` (throwError . SqlError)
          pure $ map mId applied
    Left err -> throwError $ TarballStoreError err
{-# INLINEABLE genericServer #-}

upgrade :: Command
upgrade = Command "upgrade" [] [] [] "" (void . upgradeCommand)

upgradeCommand :: StoreData -> AppT [Migration]
upgradeCommand storeData = withBackend $ \backend -> do
  ensureBootstrappedBackend backend >> commitBackend backend
  migrationNames <- missingMigrations backend storeData
  if null migrationNames
  then pure []
  else do
    applied <- fmap concat $ forM migrationNames $ \migrationName -> do
      m <- lookupMigration storeData migrationName
      toApply <- migrationsToApply storeData backend m
      mapM_ (applyMigration backend) toApply
      pure toApply
    commitBackend backend
    pure applied

commandOptions :: CommandOptions
commandOptions = CommandOptions Nothing False True

toServantError
  :: UpgradeServerError -> ServantErr
toServantError (LoadError errors)      = err400 {errBody = msg }
  where msg = cs $ T.unlines $ map (cs . show) errors
toServantError (TarballStoreError err) = err400 {errBody = cs $ show err}
toServantError (SqlError err)          = err400 {errBody = cs $ show err}
