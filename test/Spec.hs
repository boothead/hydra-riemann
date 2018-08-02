{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Types (parseEither)
import           Data.Attoparsec.Text                   as AT
import           Data.Text                              (Text)
import qualified Network.Monitoring.Riemann.Proto.Event as E
import           Test.Hspec
import           Text.ProtocolBuffers.Basic             as Basic
import Data.ByteString.Lazy.Char8 (pack)

import           Hydra.Riemann
import           Hydra.Types

main :: IO ()
main = hspec $ do
    describe "Metric" $ do
        it "should decode from valid line with an int64" $ do
            parseOnly parseHydraMetric "A.Name 100 thing"
              `shouldBe`
              (Right (Metric "A.Name" (Right 100) "thing"))
        it "should decode from valid line with a double" $ do
            parseOnly parseHydraMetric "A.Name 10.1 thing"
              `shouldBe`
              (Right (Metric "A.Name" (Left 10.1) "thing"))
        it "should decode % in the unit" $ do
            parseOnly parseHydraMetric "A.Name 80 %"
              `shouldBe`
              (Right (Metric "A.Name" (Right 80) "%"))
        it "should work with tabs" $ do
            parseOnly parseHydraMetric "A.Name\t100\tthing"
              `shouldBe`
              (Right (Metric "A.Name" (Right 100) "thing"))
        it "should work with numeric characters in the name" $ do
            parseOnly parseHydraMetric "A.Name.1 100 thing"
              `shouldBe`
              (Right (Metric "A.Name.1" (Right 100) "thing"))
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
              [ Metric "name.1" (Right 100) "things"
              , Metric "name.2" (Left 10.5) "%"
              , Metric "name.3" (Right 5)   "MiB"
              ]
    describe "HydraBuild" $ do
      it "should decode example json" $
        getJSON "test/hydra-build.json" `shouldReturn` (Right exampleBuild)
      it "FromJSON . ToJSON == id" $
        parseEither (parseJSON) (toJSON exampleBuild) `shouldBe` (Right exampleBuild)
      it "Should decode from Event payload" $ do
        Right hb <- getJSON "test/hydra-build.json"
        evt <- hydraBuildToEvent hb
        let ts = timestamp hb
        Just ts `shouldBe` (fromIntegral <$> E.time evt)
        (Just $ encode hb) `shouldBe` (pack <$> getAttr "payload" evt)

exampleBuild :: HydraBuild
exampleBuild = HydraBuild {
    build = 3772978
  , buildStatus = 0
  , drvPath = "/nix/store/9y4h1fyx9pl3ic08i2f09239b90x1lww-patchelf-tarball-0.8pre894_ed92f9f.drv"
  , event = "buildFinished"
  , finished = 1
  , job = "tarball"
  , jobset = "master"
  , metrics = [
        Metric {name = "random1", value = Right 20282, unit = "%"}
      , Metric {name = "random2", value = Right 6664, unit = "KiB"}
      ]
  , outputs = [
        Output {
          name = "out"
        , path = "/nix/store/39h5xciz5pnh1aypmr3rpdx0536y5s2w-patchelf-tarball-0.8pre894_ed92f9f"
        }
      ]
  , products = [
        Product {
            _defaultPath = ""
          , _fileSize = Just 148216
          , _name = "patchelf-0.8pre894_ed92f9f.tar.gz"
          , _path = "/nix/store/39h5xciz5pnh1aypmr3rpdx0536y5s2w-patchelf-tarball-0.8pre894_ed92f9f/tarballs/patchelf-0.8pre894_ed92f9f.tar.gz"
          , _productNr = 4
          , _sha1hash = Just "9f27d18382436a7f743f6c2f6ad66e1b536ab4c8"
          , _sha256hash = Just "b04faef2916c411f10711b58ea26965df7cb860ca33a87f1e868051b874c44b3"
          , _subtype = "source-dist"
          , _type = "file"}
       , Product {
            _defaultPath = ""
          , _fileSize = Just 121279
          , _name = "patchelf-0.8pre894_ed92f9f.tar.bz2"
          , _path = "/nix/store/39h5xciz5pnh1aypmr3rpdx0536y5s2w-patchelf-tarball-0.8pre894_ed92f9f/tarballs/patchelf-0.8pre894_ed92f9f.tar.bz2"
          , _productNr = 3
          , _sha1hash = Just "7a664841fb779dec19023be6a6121e0398067b7c"
          , _sha256hash = Just "c81e36099893f541a11480f869fcdebd2fad3309900519065c8745f614dd024a"
          , _subtype = "source-dist"
          , _type = "file"}
       , Product {
            _defaultPath = "README"
          , _fileSize = Nothing
          , _name = ""
          , _path = "/nix/store/39h5xciz5pnh1aypmr3rpdx0536y5s2w-patchelf-tarball-0.8pre894_ed92f9f"
          , _productNr = 2
          , _sha1hash = Nothing
          , _sha256hash = Nothing
          , _subtype = "readme"
          , _type = "doc"}
      , Product {
            _defaultPath = ""
          , _fileSize = Just 6230
          , _name = "README"
          , _path = "/nix/store/39h5xciz5pnh1aypmr3rpdx0536y5s2w-patchelf-tarball-0.8pre894_ed92f9f/README"
          , _productNr = 1
          , _sha1hash = Just "dc6bb09093183ab52d7e6a35b72d179869bd6fbf"
          , _sha256hash = Just "5371aee9de0216b3ea2d5ea869da9d5ee441b99156a99055e7e11e7a705f7920"
          , _subtype = "readme"
          , _type = "doc"}
      ]
  , project = "patchelf"
  , startTime = 1533137091
  , stopTime = 1533137094
  , timestamp = 1533136076
  }


eventShould :: (E.Event -> Bool) -> Text -> IO ()
eventShould f evtText = do
  let (Right metric) = parseOnly parseHydraMetric evtText
  flip shouldSatisfy f =<< hydraMetricToEvent metric
