{-# LANGUAGE OverloadedStrings #-}

module Service (main) where

import           Database.Schema.Server

import           Control.Exception        (throwIO)
import           Data.Text                (Text)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           System.Envy              (FromEnv (..), decodeEnv, env)



main :: IO ()
main = do
  Port port <- either (throwIO . userError) pure =<< decodeEnv
  AccessToken token <- either (throwIO . userError) pure =<< decodeEnv
  run port (upgradeApp token)

upgradeApp :: Text -> Application
upgradeApp = serve upgradeApi . server

upgradeApi :: Proxy UpgradeAPI
upgradeApi = Proxy


newtype Port = Port Int
instance FromEnv Port where
  fromEnv = Port <$> env "PORT"

newtype AccessToken = AccessToken Text
instance FromEnv AccessToken where
  fromEnv = AccessToken <$> env "MIGRATION_ACCESS_TOKEN"
