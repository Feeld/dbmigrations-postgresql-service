{-# LANGUAGE OverloadedStrings #-}
module Cli (main) where

import           Control.Monad.Except                    (runExceptT)
import qualified Data.ByteString.Lazy                    as LBS
import           Database.Schema.Server
import           Database.Schema.Migrations.Tarball      (TarballContents(..))
import           System.Environment                      (getArgs)
import           System.Exit                             (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [connString] -> do
      contents <- TarballContents <$> LBS.getContents
      eResult <- runExceptT $ server $ UpgradeRequest connString contents
      case eResult of
        Right _  -> pure ()
        Left err -> do
          putStrLn $ show err
          exitFailure
    _ -> do
      putStrLn "Usage: dbmigrations-postgresql-cli connectionString"
      exitFailure
