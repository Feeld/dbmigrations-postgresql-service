{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Schema.Migrations.Tarball
( tarballStore
, TarballStoreError (..)
, TarballContents (..)
) where
import           Database.Schema.Migrations.Migration (Migration (..),
                                                       emptyMigration)
import           Database.Schema.Migrations.Store

import qualified Codec.Archive.Tar                    as Tar
import qualified Codec.Compression.GZip               as Gzip
import           Control.Exception                    (Exception, catch,
                                                       throwIO)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.Map.Strict                      as M
import           Data.Maybe
import           Data.String.Conversions              (cs)
import qualified Data.Text                            as T
import           Data.Time                            ()
import           Data.Time.Clock                      (UTCTime)
import           Data.Yaml.YamlLight
import           System.FilePath                      (takeFileName)

type ContentMap = M.Map T.Text BS.ByteString

data TarballStoreError
  = NotImplemented
  | TarballStoreError String
  | InvalidTarballError String
  deriving (Show, Exception)


tarballStore :: TarballContents -> Either TarballStoreError MigrationStore
tarballStore contents =
  case eContentMap of
    Right entryMap ->
      Right MigrationStore
        { loadMigration = migrationFromEntry entryMap
        , saveMigration = \_ -> throwIO NotImplemented
        , getMigrations = pure $ mapMaybe (T.stripSuffix ".txt") $ M.keys entryMap
        , fullMigrationName = pure . cs
        }
    Left (e, _) -> Left (InvalidTarballError (show e))
  where
  step :: ContentMap -> Tar.Entry -> ContentMap
  step !existingMap entry =
    case entryBs entry of
      Just bs -> M.insert (cs $ takeFileName $ Tar.entryPath entry) bs existingMap
      Nothing -> existingMap

  entryBs :: Tar.Entry -> Maybe BS.ByteString
  entryBs entry =
    case Tar.entryContent entry of
      Tar.NormalFile bs _ -> Just $ LBS.toStrict bs
      _                   -> Nothing
  eContentMap
    = Tar.foldlEntries step mempty
    $ Tar.read
    $ Gzip.decompress
    $ unTarballContents contents

newtype TarballContents = TarballContents
  { unTarballContents :: LBS.ByteString }


-- make everything compile
-- then change filepath to entry and call it migrationfromentry
-- to get filepath for name etc you need to say that path = EntryPath,
-- then parse the bytes from entry. Look at Tar documentations.
migrationFromEntry :: ContentMap -> T.Text -> IO (Either String Migration)
migrationFromEntry contentMap name =
  (Right <$> process) `catch` (\(TarballStoreError s) -> return $ Left $ "Could not parse migration " ++ cs name ++ ":" ++ s)
  where
    process =
      case M.lookup (name<>".txt") contentMap of
          Just bs -> do
            yaml <- parseYamlBytes bs
            -- Convert yaml structure into basic key/value map
            fields <- getFields yaml
            let missing = missingFields fields

            case length missing of
              0 -> do
                let newM = emptyMigration name
                case migrationFromFields newM fields of
                  Nothing -> throwTBS $ "Error in " ++ cs name ++ ": unrecognized field found"
                  Just m -> return m
              _ -> throwTBS $ "Error in " ++ cs name ++ ": missing required field(s): " ++ show missing

          Nothing -> throwTBS $ "Error in " ++ cs name ++ ": not found in archive."


type FieldProcessor = T.Text -> Migration -> Maybe Migration

getFields :: YamlLight -> IO [(T.Text, T.Text)]
getFields (YMap mp) = mapM toPair $ M.assocs mp
    where
      toPair :: (YamlLight, YamlLight) -> IO (T.Text, T.Text)
      toPair (YStr k, YStr v) = pure (cs k, cs v)
      toPair (k, v) = throwTBS $ "Error in YAML input; expected string key and string value, got " ++ show (k, v)
getFields _ = throwTBS "Error in YAML input; expected mapping"

missingFields :: [(T.Text, T.Text)] -> [T.Text]
missingFields fs =
    [ k | k <- requiredFields, k `notElem` inputStrings ]
    where
      inputStrings = map fst fs

requiredFields :: [T.Text]
requiredFields = [ "Apply"
                 , "Depends"
                 ]

throwTBS :: String -> IO a
throwTBS = throwIO . TarballStoreError

migrationFromFields :: Migration -> [(T.Text, T.Text)] -> Maybe Migration
migrationFromFields m [] = Just m
migrationFromFields m ((name, value):rest) = do
  processor <- lookup name fieldProcessors
  newM <- processor value m
  migrationFromFields newM rest

fieldProcessors :: [(T.Text, FieldProcessor)]
fieldProcessors = [ ("Created", setTimestamp )
                  , ("Description", setDescription )
                  , ("Apply", setApply )
                  , ("Revert", setRevert )
                  , ("Depends", setDepends )
                  ]

setTimestamp :: FieldProcessor
setTimestamp value m = do
  ts <- case readTimestamp value of
          [(t, _)] -> return t
          _        -> fail "expected one valid parse"
  return $ m { mTimestamp = Just ts }

readTimestamp :: T.Text -> [(UTCTime, String)]
readTimestamp = reads . cs

setDescription :: FieldProcessor
setDescription desc m = Just $ m { mDesc = Just desc }

setApply :: FieldProcessor
setApply apply m = Just $ m { mApply = apply }

setRevert :: FieldProcessor
setRevert revert m = Just $ m { mRevert = Just revert }

setDepends :: FieldProcessor
setDepends depString m = Just $ m { mDeps = T.words depString }
