{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Attoparsec.Text as AT
import Data.Text (Text)
import           Test.Hspec
import           Text.ProtocolBuffers.Basic                 as Basic
import qualified Network.Monitoring.Riemann.Proto.Event     as E


import Hydra.Riemann

main :: IO ()
main = hspec $ do
    describe "HydraMetric" $ do
        it "should decode from valid line with an int64" $ do
            parseOnly parseHydraMetric "A.Name 100 thing"
              `shouldBe`
              (Right (HydraMetric "A.Name" (Right 100) "thing"))
        it "should decode from valid line with a double" $ do
            parseOnly parseHydraMetric "A.Name 10.1 thing"
              `shouldBe`
              (Right (HydraMetric "A.Name" (Left 10.1) "thing"))
        it "should decode % in the unit" $ do
            parseOnly parseHydraMetric "A.Name 80 %"
              `shouldBe`
              (Right (HydraMetric "A.Name" (Right 80) "%"))
        it "should work with tabs" $ do
            parseOnly parseHydraMetric "A.Name\t100\tthing"
              `shouldBe`
              (Right (HydraMetric "A.Name" (Right 100) "thing"))
        it "should work with numeric characters in the name" $ do
            parseOnly parseHydraMetric "A.Name.1 100 thing"
              `shouldBe`
              (Right (HydraMetric "A.Name.1" (Right 100) "thing"))
        it "should create an Event with the correct service" $ do
          eventShould ((== Just "test.name") . fmap Basic.uToString . E.service)
                      "test.name 100 things"
        it "should create an Event with the correct metric for integral" $ do
          eventShould ((== Just 100) . E.metric_sint64)
                      "test.name 100 things"
        it "should create an Event with the correct metric for double" $ do
          eventShould ((== Just 10.1) . E.metric_d)
                      "test.name 10.1 things"
        it "should parse lines" $ do
          parseMetricsFile "test/metrics.txt"
            `shouldReturn`
            Right
              [ HydraMetric "name.1" (Right 100) "things"
              , HydraMetric "name.2" (Left 10.5) "%"
              , HydraMetric "name.3" (Right 5)   "MiB"
              ]

eventShould :: (E.Event -> Bool) -> Text -> IO ()
eventShould f evtText = do
  let (Right metric) = parseOnly parseHydraMetric evtText
  flip shouldSatisfy f =<< hydraMetricToEvent metric
