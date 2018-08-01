module Hydra.Riemann
  ( HydraMetric(..)
  , parseHydraMetric
  , hydraMetricToEvent
  , parseMetricsFile
  , sendHydraMetrics
  ) where

import           Data.Attoparsec.Text                    (Parser)
import qualified Data.Attoparsec.Text                    as AT
import qualified Data.Char                               as Char
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


data HydraMetric = HydraMetric {
    metricName   :: Text
  , metricAmount :: Either Double Int64
  , metricUnit   :: Text

  } deriving (Show, Eq)

parseHydraMetric :: Parser HydraMetric
parseHydraMetric = HydraMetric
  <$> (AT.takeTill AT.isHorizontalSpace <* AT.skipSpace)
  <*> (floatingOrInteger <$> AT.scientific <* AT.skipSpace)
  <*> AT.takeWhile (\c -> Char.isAlpha c || (c == '%'))

hydraMetricToEvent :: HydraMetric -> IO Event
hydraMetricToEvent (HydraMetric name amount unit) = do
  let mkEv = EM.ok (Text.unpack name)
      attr = EM.attributes [attribute "unit" (Just $ Text.unpack unit)]
      metric = either EM.metric EM.metric amount
      endo = metric <> attr
  evt <- (endo <>) <$> EM.timeAndHost
  return $ mkEv evt

parseMetricsFile :: FilePath -> IO (Either String [HydraMetric])
parseMetricsFile fp = AT.parseOnly parser <$> Text.readFile fp
  where
    parser = parseHydraMetric `AT.sepBy` (AT.skipMany AT.endOfLine)

sendHydraMetrics :: Client c => c -> FilePath -> IO ()
sendHydraMetrics client fp = do
  hMetrics <- either error id <$> parseMetricsFile fp
  for_ hMetrics $ \m ->
    sendEvent client =<< hydraMetricToEvent m
