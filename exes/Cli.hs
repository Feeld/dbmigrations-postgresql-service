{-# LANGUAGE OverloadedStrings #-}
module Cli (main) where

import           Control.Monad.Except               (runExceptT)
import qualified Data.ByteString.Lazy               as LBS
import           Database.Schema.Migrations.Tarball (TarballContents (..))
import           Database.Schema.Server             (UpgradeRequest (..),
                                                     genericServer)
import           System.Environment                 (getArgs)
import           System.Exit                        (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [connstring] -> do
      contents <- TarballContents <$> LBS.getContents
      eResult <- runExceptT $ genericServer $ UpgradeRequest connstring contents
      case eResult of
        Right [] -> putStrLn "Database is up to date"
        Right applied -> do
          putStrLn "Applied:"
          mapM_ print applied
        Left err -> do
          print err
          exitFailure
    _ -> do
      putStrLn "Usage: dbmigrations-postgresql-cli connectionString"
      exitFailure
