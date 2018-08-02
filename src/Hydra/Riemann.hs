module Hydra.Riemann
  ( Metric(..)
  , parseHydraMetric
  , hydraMetricToEvent
  , hydraBuildToEvent
  , parseMetricsFile
  , sendHydraMetrics
  , sendHydraBuild
  , getJSON
  ) where

import Data.Aeson
import           Data.Attoparsec.Text                    (Parser)
import qualified Data.Attoparsec.Text                    as AT
import qualified Data.Char                               as Char
import Data.Time.Clock (diffTimeToPicoseconds)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy                   as BSL
import           Data.Int                                (Int64)
import Data.Foldable (for_)
import           Data.Monoid                             ((<>))
import           Data.Scientific                         (floatingOrInteger)
import           Data.Text                               (Text)
import qualified Data.Text                               as Text
import qualified Data.Text.IO                            as Text
import           Network.Monitoring.Riemann.Client       (Client (..))
import           Network.Monitoring.Riemann.Event        (Event, attribute)
import qualified Network.Monitoring.Riemann.Event.Monoid as EM

import Hydra.Types

parseHydraMetric :: Parser Metric
parseHydraMetric = Metric
  <$> (AT.takeTill AT.isHorizontalSpace <* AT.skipSpace)
  <*> (floatingOrInteger <$> AT.scientific <* AT.skipSpace)
  <*> AT.takeWhile (\c -> Char.isAlpha c || (c == '%'))

hydraMetricToEvent :: Metric -> IO Event
hydraMetricToEvent (Metric name amount unit) = do
  let mkEv = EM.ok (Text.unpack name)
      attr = EM.attributes [attribute "unit" (Just $ Text.unpack unit)]
      metric = either EM.metric EM.metric amount
      endo = metric <> attr
  evt <- (endo <>) <$> EM.timeAndHost
  return $! mkEv evt

hydraBuildToEvent :: HydraBuild -> IO Event
hydraBuildToEvent build = do
  let mkEv = EM.ok "hydra-build"
      payload = unpack $ encode build
      attr = EM.attributes [attribute "payload" (Just payload)]
      ts = EM.timestamp . truncate $ timestamp build
      endo = (ts <> attr)
  evt <- (endo <>) <$> EM.timeAndHost
  return $! mkEv evt

parseMetricsFile :: FilePath -> IO (Either String [Metric])
parseMetricsFile fp = AT.parseOnly parser <$> Text.readFile fp
  where
    parser = parseHydraMetric `AT.sepBy` (AT.skipMany AT.endOfLine)

sendHydraMetrics :: Client c => c -> FilePath -> IO ()
sendHydraMetrics client fp = do
  hMetrics <- either error id <$> parseMetricsFile fp
  for_ hMetrics $ \m ->
    sendEvent client =<< hydraMetricToEvent m

sendHydraBuild :: Client c => c -> FilePath -> IO ()
sendHydraBuild client fp = do
  hBuild <- either error id <$> getJSON fp
  sendEvent client =<< hydraBuildToEvent hBuild


getJSON :: FromJSON a => FilePath -> IO (Either String a)
getJSON f = eitherDecode <$> BSL.readFile f
