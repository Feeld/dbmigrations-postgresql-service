{-# LANGUAGE OverloadedStrings      #-}

module Service (main) where

import           Database.Schema.Server

import           Control.Exception        (throwIO)
import           Control.Monad.Reader     (liftIO)

import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant

import           System.Envy              (FromEnv (..), decodeEnv, env)



main :: IO ()
main = do
  Port port <- either (throwIO . userError) pure =<< liftIO decodeEnv
  run port upgradeApp

upgradeApp :: Application
upgradeApp = serve upgradeApi server

upgradeApi :: Proxy UpgradeAPI
upgradeApi = Proxy


newtype Port = Port Int
instance FromEnv Port where
  fromEnv = Port <$> env "DBM_PORT"
