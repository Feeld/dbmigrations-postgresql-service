{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Database.Schema.Server
  ( server
  , UpgradeAPI
  , UpgradeRequest(..)
  )
where

import           Control.Exception.Lifted                (throwIO, catch)
import           Control.Monad.Trans.Control             (MonadBaseControl)
import           Control.Monad.Except                    (MonadError)
import           Control.Monad.Reader                    (forM_, runReaderT)
import           Control.Monad.IO.Class                  (MonadIO(..))
import qualified Data.ByteString.Lazy                    as LBS
import           Data.String.Conversions                 (cs)
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Database.HDBC                           (SqlError, catchSql,
                                                          seErrorMsg)
import           Database.HDBC.PostgreSQL                (connectPostgreSQL)
import           Database.Schema.Migrations.Backend.HDBC (hdbcBackend)
import           Database.Schema.Migrations.Store
import           Database.Schema.Migrations.Tarball      (TarballContents (..),
                                                          tarballStore, TarballStoreError)
import           Moo.CommandInterface
import           Moo.Core (AppState(..), CommandOptions(..),  Command(_cHandler))

import           Servant

data UpgradeRequest = UpgradeRequest
  { _connectionString :: String
  , _tarballData      :: TarballContents
  }

data UpgradeServerError =
    LoadError [MapValidationError]
  | TarballStoreError TarballStoreError
  | SqlError SqlError

type UpgradeAPI = ReqBody '[JSON] UpgradeRequest
               :> PostNoContent '[JSON] NoContent

server :: ( MonadBaseControl IO m
          , MonadIO m
          , MonadError ServantErr m
          ) => UpgradeRequest -> m NoContent
server req = do

  connection <- liftIO $ connectPostgreSQL (_connectionString req)
  let backend = hdbcBackend connection

  let command      = upgradeCommand

  case tarballStore $ _tarballData req of
    Right store -> do
      loadedStoreData <- liftIO $ loadMigrations store
      case loadedStoreData of
        Left es -> throwAsServantError $ LoadError es
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
          (liftIO $ runReaderT (_cHandler command storeData) st) `catch` (throwAsServantError . SqlError)
          pure NoContent
    Left err -> throwAsServantError $ TarballStoreError err

upgradeCommand :: Command
Just upgradeCommand = findCommand "upgrade"

commandOptions :: CommandOptions
commandOptions = CommandOptions Nothing False True

throwAsServantError
  :: ( MonadError ServantErr m )
  =>  UpgradeServerError -> m a
throwAsServantError = throwError . toServantError

toServantError
  :: UpgradeServerError -> ServantErr
toServantError (LoadError errors)      = err400 {errBody = msg }
  where msg = cs $ T.unlines $ map (cs . show) errors
toServantError (TarballStoreError err) = err400 {errBody = cs $ show err}
toServantError (SqlError err)          = err400 {errBody = cs $ show err}
