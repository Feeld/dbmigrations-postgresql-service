{-# LANGUAGE OverloadedStrings #-}
module Service (main) where

import           Control.Exception                       (throwIO)
import           Control.Monad.Reader                    (forM_, runReaderT)
import qualified Data.ByteString.Lazy                    as LBS
import           Data.Text                               (Text)
import           Database.HDBC                           (SqlError, catchSql,
                                                          seErrorMsg)
import           Database.HDBC.PostgreSQL                (connectPostgreSQL)
import           Database.Schema.Migrations.Backend.HDBC (hdbcBackend)
import           Database.Schema.Migrations.Store
import           Database.Schema.Migrations.Tarball      (TarballContents (..),
                                                          tarballStore)
import           Moo.CommandInterface
import           Moo.Core (AppState(..), CommandOptions(..),  Command(_cHandler))

data UpgradeRequest = UpgradeRequest
  { _connectionString :: String
  , _tarballData      :: TarballContents
  }

main :: IO ()
main = do
  let req :: UpgradeRequest
      req = undefined

  connection <- connectPostgreSQL (_connectionString req)
  let backend = hdbcBackend connection

  let command      = upgradeCommand

  case tarballStore $ _tarballData req of
    Right store -> do
      loadedStoreData <- loadMigrations store
      case loadedStoreData of
        Left es -> do
          putStrLn "There were errors in the migration store:"
          forM_ es $ \err -> putStrLn $ "  " ++ show err
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
          runReaderT (_cHandler command storeData) st `catchSql` reportSqlError
    Left err -> throwIO err

upgradeCommand :: Command
Just upgradeCommand = findCommand "upgrade"

commandOptions :: CommandOptions
commandOptions = CommandOptions Nothing False True

reportSqlError :: SqlError -> IO ()
reportSqlError e =
  putStrLn $ "\n" ++ "A database error occurred: " ++ seErrorMsg e
  --fixme
