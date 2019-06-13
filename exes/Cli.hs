{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Cli (main) where

import           Database.Schema.Migrations.Tarball (TarballContents (..))
import           Database.Schema.Server             (UpgradeRequest (..),
                                                     UpgradeResponse (..),
                                                     genericServer)

import           Control.Monad.Except               (runExceptT)
import qualified Data.ByteString.Lazy               as LBS
import           Data.Maybe                         (isJust)
import           System.Environment                 (getArgs, lookupEnv)
import           System.Exit                        (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [connstring] -> do
      contents <- TarballContents <$> LBS.getContents
      isTesting <- isJust <$> lookupEnv "DBM_TEST"
      eResult <- runExceptT $ genericServer "dummy" $ UpgradeRequest connstring contents isTesting "dummy"
      case eResult of
        Right UpgradeResponse{appliedMigrations=[]} ->
          putStrLn "Database is up to date"
        Right UpgradeResponse{appliedMigrations} -> do
          putStrLn "Applied:"
          mapM_ print appliedMigrations
        Left err -> do
          print err
          exitFailure
    _ -> do
      putStrLn "Usage: dbmigrations-postgresql-cli connectionString"
      exitFailure
