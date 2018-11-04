{-# LANGUAGE OverloadedStrings #-}
{-
Author: Glaukon Ariston
Date: 18.10.2018
Abstract:
-}

module Ting.BookConfig
    ( BookConfig(..)
    , BookDescription(..)
    , parseBookConfig
    )
where


import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Control.Applicative (Applicative)

-- https://github.com/snoyberg/yaml/blob/master/yaml/examples/Config.hs

data BookConfig = BookConfig
    { _bookId :: Int
    , _tingIdBase :: Int
    , _mp3s :: [FilePath]
    , _description :: BookDescription
    } deriving (Eq, Show)


instance FromJSON BookConfig where
  parseJSON (Y.Object v) =
    BookConfig <$>
    v .: "bookId" <*>
    v .: "tingIdBase" <*>
    v .: "mp3s" <*>
    v .: "description"
  parseJSON _ = fail "Expected Object for BookConfig value"


data BookDescription = BookDescription
    { _name :: String
    , _publisher :: String
    , _author :: String
    , _bookVersion :: String
    , _url :: String
    , _bookAreaCode :: String
    , _thumb :: String
    } deriving (Eq, Show)


instance FromJSON BookDescription where
  parseJSON (Y.Object v) =
    BookDescription <$>
    v .: "name" <*>
    v .: "publisher" <*>
    v .: "author" <*>
    v .: "bookVersion" <*>
    v .: "url" <*>
    v .: "bookAreaCode" <*>
    v .: "thumb"
  parseJSON _ = fail "Expected Object for BookDescription value"


parseBookConfig :: FilePath -> IO BookConfig
parseBookConfig yamlFile = Y.decodeFileThrow yamlFile


