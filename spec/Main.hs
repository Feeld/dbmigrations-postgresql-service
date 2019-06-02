{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main (main) where

import           Database.Schema.Server      (UpgradeAPI, server)

import qualified Codec.Archive.Tar           as Tar
import qualified Codec.Archive.Tar.Entry     as Tar
import qualified Codec.Compression.GZip      as Gzip
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy        as LBS
import           Data.String.Conversions     (cs)
import           Data.Text                   (Text)
import           Network.HTTP.Types.Header
import           Network.Wai.Test            (SResponse)
import           Servant
import           System.Environment          (lookupEnv)
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON         (json)
import NeatInterpolation (text)


main :: IO ()
main = hspec spec

app :: Application
app = serve (Proxy @UpgradeAPI) (server "mySecret")


spec :: Spec
spec = before (pure app) $

  describe "POST /" $ do

    it "responds with 200 and applies the migrations, skipping directories" $ do
      Just connString <- liftIO $ lookupEnv "DBM_DATABASE"
      let tarball :: Text
          tarball = cs . B64.encode $ Gzip.compress $ Tar.write
            [ entryInitial
            , entrySecond
            , entryThird
            , entryDir
            ]
          Right entryInitial = do
            path <- Tar.toTarPath False "./initial.txt"
            pure $ Tar.fileEntry path $ cs [text|
              Description: Initial database schema
              Created: 2019-05-09 11:55:52.439373 UTC
              Depends:
              Apply: |
                create table foo (id serial, bar text);
              Revert: |
                drop table foo;
              |]
          Right entrySecond = do
            path <- Tar.toTarPath False "./second.txt"
            pure $ Tar.fileEntry path $ cs [text|
              Depends: initial
              Apply: |
                create table bar (id serial, foo text);
              |]
          Right entryThird = do
            path <- Tar.toTarPath False "./third.txt"
            pure $ Tar.fileEntry path $ cs [text|
              Depends: second
              Apply: |
                drop table foo;
                drop table bar;
              |]
          Right entryDir = Tar.directoryEntry <$> Tar.toTarPath False "./fourth.txt"
      postJson "/" [json|{
        connString: #{connString},
        tarball: #{tarball},
        test: true,
        accessToken: "mySecret"
        }|] `shouldRespondWith` [json|["initial", "second", "third"]|]

    it "responds with 200 and does nothing if no migrations to apply" $ do
      Just connString <- liftIO $ lookupEnv "DBM_DATABASE"
      let tarball :: Text
          tarball = cs . B64.encode $ Gzip.compress $ Tar.write [ ]
      postJson "/" [json|{
        connString: #{connString},
        tarball: #{tarball},
        test: true,
        accessToken: "mySecret"
        }|] `shouldRespondWith` [json|[]|]

    it "responds with 403 if invalid accessToken" $ do
      Just connString <- liftIO $ lookupEnv "DBM_DATABASE"
      let tarball :: Text
          tarball = cs . B64.encode $ Gzip.compress $ Tar.write [ ]
      postJson "/" [json|{
        connString: #{connString},
        tarball: #{tarball},
        test: true,
        accessToken: "badSecret"
        }|] `shouldRespondWith` 403

    it "responds with 422 if migration has bad sql" $ do
      Just connString <- liftIO $ lookupEnv "DBM_DATABASE"
      let tarball :: Text
          tarball = cs . B64.encode $ Gzip.compress $ Tar.write
            [ entryInitial
            ]
          Right entryInitial = do
            path <- Tar.toTarPath False "./initial.txt"
            pure $ Tar.fileEntry path $ cs [text|
              Depends:
              Apply: |
                create table foo;
              |]
      postJson "/" [json|{
        connString: #{connString},
        tarball: #{tarball},
        test: true,
        accessToken: "mySecret"
        }|] `shouldRespondWith` 422

    it "responds with 422 if bad connString" $ do
      let tarball :: Text
          tarball = cs . B64.encode $ Gzip.compress $ Tar.write
            [ entryInitial
            ]
          Right entryInitial = do
            path <- Tar.toTarPath False "./initial.txt"
            pure $ Tar.fileEntry path $ cs [text|
              Depends:
              Apply: |
                create table foo (id serial);
              |]
      postJson "/" [json|{
        connString: "dbname=lalala",
        tarball: #{tarball},
        test: true,
        accessToken: "mySecret"
        }|] `shouldRespondWith` 422

    it "responds with 400 if tarball is missing migrations" $ do
      Just connString <- liftIO $ lookupEnv "DBM_DATABASE"
      let tarball :: Text
          tarball = cs . B64.encode $ Gzip.compress $ Tar.write
            [ entryInitial
            ]
          Right entryInitial = do
            path <- Tar.toTarPath False "./initial.txt"
            pure $ Tar.fileEntry path $ cs [text|
              Depends: non-existing
              Apply: |
                create table foo (id serial);
              |]
      postJson "/" [json|{
        connString: #{connString},
        tarball: #{tarball},
        test: true,
        accessToken: "mySecret"
        }|] `shouldRespondWith` 400

    it "responds with 400 if a migration has malformed yaml" $ do
      Just connString <- liftIO $ lookupEnv "DBM_DATABASE"
      let tarball :: Text
          tarball = cs . B64.encode $ Gzip.compress $ Tar.write
            [ entryInitial
            ]
          Right entryInitial = do
            path <- Tar.toTarPath False "./initial.txt"
            pure $ Tar.fileEntry path $ cs [text|
              Dependsbad
              Apply: |
                create table foo (id serial);
              |]
      postJson "/" [json|{
        connString: #{connString},
        tarball: #{tarball},
        test: true,
        accessToken: "mySecret"
        }|] `shouldRespondWith` 400

    it "responds with 400 if a migration yaml has a wrongly typed field" $ do
      Just connString <- liftIO $ lookupEnv "DBM_DATABASE"
      let tarball :: Text
          tarball = cs . B64.encode $ Gzip.compress $ Tar.write
            [ entryInitial
            ]
          Right entryInitial = do
            path <- Tar.toTarPath False "./initial.txt"
            pure $ Tar.fileEntry path $ cs [text|
              Depends: [4, 2]
              Apply: |
                create table foo (id serial);
              |]
      postJson "/" [json|{
        connString: #{connString},
        tarball: #{tarball},
        test: true,
        accessToken: "mySecret"
        }|] `shouldRespondWith` 400

    it "responds with 400 if a migration yaml is not a mapping" $ do
      Just connString <- liftIO $ lookupEnv "DBM_DATABASE"
      let tarball :: Text
          tarball = cs . B64.encode $ Gzip.compress $ Tar.write
            [ entryInitial
            ]
          Right entryInitial = do
            path <- Tar.toTarPath False "./initial.txt"
            pure $ Tar.fileEntry path $ cs [text|
              - [4, 2]
              - |
                create table foo (id serial);
              |]
      postJson "/" [json|{
        connString: #{connString},
        tarball: #{tarball},
        test: true,
        accessToken: "mySecret"
        }|] `shouldRespondWith` 400

    it "responds with 400 if a migration yaml has an unrecognized field" $ do
      Just connString <- liftIO $ lookupEnv "DBM_DATABASE"
      let tarball :: Text
          tarball = cs . B64.encode $ Gzip.compress $ Tar.write
            [ entryInitial
            ]
          Right entryInitial = do
            path <- Tar.toTarPath False "./initial.txt"
            pure $ Tar.fileEntry path $ cs [text|
              Unrecognized: I don't exist
              Depends:
              Apply: |
                create table foo (id serial);
              |]
      postJson "/" [json|{
        connString: #{connString},
        tarball: #{tarball},
        test: true,
        accessToken: "mySecret"
        }|] `shouldRespondWith` 400

    it "responds with 400 if gzip corrupted" $ do
      Just connString <- liftIO $ lookupEnv "DBM_DATABASE"
      let tarball :: Text
          tarball = cs . B64.encode $ LBS.drop 15 $ Gzip.compress $ Tar.write
            [ entryInitial
            ]
          Right entryInitial = do
            path <- Tar.toTarPath False "./initial.txt"
            pure $ Tar.fileEntry path $ cs [text|
              Depends:
              Apply: |
                create table foo (id serial, bar text);
              |]
      postJson "/" [json|{
        connString: #{connString},
        tarball: #{tarball},
        test: true,
        accessToken: "mySecret"
        }|] `shouldRespondWith` 400

    it "responds with 400 if tarball corrupted" $ do
      Just connString <- liftIO $ lookupEnv "DBM_DATABASE"
      let tarball :: Text
          tarball = cs . B64.encode $ Gzip.compress $ LBS.drop 15 $ Tar.write
            [ entryInitial
            ]
          Right entryInitial = do
            path <- Tar.toTarPath False "./initial.txt"
            pure $ Tar.fileEntry path $ cs [text|
              Depends:
              Apply: |
                create table foo (id serial, bar text);
              |]
      postJson "/" [json|{
        connString: #{connString},
        tarball: #{tarball},
        test: true,
        accessToken: "mySecret"
        }|] `shouldRespondWith` 400

postJson :: BS.ByteString -> LBS.ByteString -> WaiSession SResponse
postJson url = request "POST" url
  [ (hAccept, "application/json")
  , (hContentType, "application/json")
  ]
