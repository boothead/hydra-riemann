{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Hydra.Types
  ( Metric(..)
  , Product(..)
  , Output(..)
  , HydraBuild(..)
  , getAttr
  ) where

import Control.Monad (join)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy                       as BSL
import           Data.Foldable                              (toList)
import           Data.Int                                   (Int64)
import           Data.Map.Strict                            (Map)
import           Data.Scientific                            (floatingOrInteger)
import           Data.Text                                  (Text)
import           Data.Time.Clock                            (DiffTime)
import           GHC.Generics                               (Generic)
import qualified Network.Monitoring.Riemann.Proto.Attribute as A
import qualified Network.Monitoring.Riemann.Proto.Event     as E
import           Text.ProtocolBuffers.Header                (uToString)


getAttr :: String -> E.Event -> Maybe String
getAttr attrK = join . lookup attrK . toList . fmap toKV . E.attributes
  where
    toKV (A.Attribute k v) = (uToString k, uToString <$> v)

data Metric = Metric {
    name  :: !Text
  , value :: !(Either Double Int64)
  , unit  :: !Text

  } deriving (Show, Eq, Generic)

instance ToJSON Metric where
  toJSON (Metric n v u) =
    object [ "name" .= n
           , "value" .= either toJSON toJSON v
           , "unit" .= u
           ]

instance FromJSON Metric where
  parseJSON = withObject "Metric" $ \v -> Metric
    <$> v .: "name"
    <*> (floatingOrInteger <$> v .: "value")
    <*> v .: "unit"

data Product = Product {
    _defaultPath :: !Text
  , _fileSize    :: !(Maybe Integer)
  , _name        :: !Text
  , _path        :: !FilePath
  , _productNr   :: !Int
  , _sha1hash    :: !(Maybe Text)
  , _sha256hash  :: !(Maybe Text)
  , _subtype     :: !Text
  , _type        :: !Text
  } deriving (Show, Eq, Generic)

instance ToJSON Product where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = drop 1 }

instance FromJSON Product where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }


data Output = Output {
    name :: !Text
  , path :: !Text
  } deriving (Show, Eq, Generic)


instance ToJSON Output
instance FromJSON Output

data HydraBuild = HydraBuild {
    build       :: !Integer
  , buildStatus :: !Int
  , drvPath     :: !FilePath
  , event       :: !Text
  , finished    :: !Int
  , job         :: !Text
  , jobset      :: !Text
  , metrics     :: [Metric]
  , outputs     :: [Output]
  , products    :: [Product]
  , project     :: !Text
  , startTime   :: !DiffTime
  , stopTime    :: !DiffTime
  , timestamp   :: !DiffTime
  } deriving (Show, Eq, Generic)

instance ToJSON HydraBuild
instance FromJSON HydraBuild

getJSON :: FromJSON a => FilePath -> IO (Either String a)
getJSON f = eitherDecode <$> BSL.readFile f
