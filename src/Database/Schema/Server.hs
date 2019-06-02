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

import           Database.Schema.Migrations.Tarball      (TarballContents (..),
                                                          TarballStoreError,
                                                          tarballStore)

import           Control.Exception.Lifted                (handle)
import           Control.Monad                           (forM, (<=<))
import           Control.Monad.Except                    (ExceptT, MonadError,
                                                          runExceptT,
                                                          throwError)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Control.Monad.Reader                    (asks, runReaderT)
import           Control.Monad.Trans.Control             (MonadBaseControl)
import           Data.Aeson                              (FromJSON, ToJSON)
import           Data.Proxy                              (Proxy (..))
import           Data.String.Conversions                 (cs)
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Database.HDBC                           (SqlError)
import           Database.HDBC.PostgreSQL                (connectPostgreSQL)
import           Database.Schema.Migrations              (ensureBootstrappedBackend,
                                                          migrationsToApply,
                                                          missingMigrations)
import           Database.Schema.Migrations.Backend      (applyMigration,
                                                          commitBackend,
                                                          rollbackBackend)
import           Database.Schema.Migrations.Backend.HDBC (hdbcBackend)
import           Database.Schema.Migrations.Migration    (Migration (mId))
import           Database.Schema.Migrations.Store        (MapValidationError,
                                                          StoreData,
                                                          loadMigrations)
import           GHC.Generics                            (Generic)
import           Moo.CommandUtils                        (lookupMigration,
                                                          withBackend)
import           Moo.Core                                (AppState (..), AppT,
                                                          Command (..),
                                                          CommandOptions (..))
import           Servant                                 ((:>), Handler, JSON,
                                                          Post, ReqBody,
                                                          ServantErr (..),
                                                          Server, err400,
                                                          err422, hoistServer)

data UpgradeRequest = UpgradeRequest
  { connString :: String
  , tarball    :: TarballContents
  , test       :: Bool
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
  eStore <- liftIO $ tarballStore $ tarball req
  case eStore of
    Right store -> do
      loadedStoreData <- liftIO $ loadMigrations store
      case loadedStoreData of
        Left es -> throwError $ LoadError es
        Right storeData -> handle (throwError . SqlError) $ liftIO $ do
          connection <- connectPostgreSQL (connString req)
          let st = AppState
                    { _appOptions = CommandOptions
                        { _configFilePath = Nothing
                        , _test = test req
                        , _noAsk = True
                        }
                    , _appCommand = Command "" [] [] [] "" (const (pure ())) --dummy
                    , _appRequiredArgs = []
                    , _appOptionalArgs = []
                    , _appBackend = hdbcBackend connection
                    , _appStore = store
                    , _appStoreData = storeData
                    , _appLinearMigrations = False
                    , _appTimestampFilenames = False
                    }
          applied <- runReaderT (upgradeCommand storeData) st
          pure $ map mId applied
    Left err -> throwError $ TarballStoreError err
{-# INLINEABLE genericServer #-}

-- | Like dbmigrations' 'upgradeCommand' but this one doesn't exit the process on
-- error and returns a list of applied migration names.
-- IOW, behaves well inside a long-running process
upgradeCommand :: StoreData -> AppT [Migration]
upgradeCommand storeData = do
  isTesting <-  _test <$> asks _appOptions
  withBackend $ \backend -> do
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
      (if isTesting then rollbackBackend else commitBackend) backend
      pure applied

toServantError
  :: UpgradeServerError -> ServantErr
toServantError (LoadError errors)      = err400 {errBody = msg }
  where msg = cs $ T.unlines $ map (cs . show) errors
toServantError (TarballStoreError err) = err400 {errBody = cs $ show err}
toServantError (SqlError err)          = err422 {errBody = cs $ show err}
