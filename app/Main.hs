module Main where

import           Data.Monoid
import           Network.Monitoring.Riemann.TCPClient (tcpClient)
import           Options.Applicative                  (Parser, argument, auto,
                                                       command, eitherReader,
                                                       execParser, fullDesc,
                                                       header, help, helper,
                                                       hsubparser, info, long,
                                                       metavar, option,
                                                       progDesc, short, str,
                                                       strOption, value, (<**>))

import Hydra.Riemann

data Spec = Spec
  { subHost  :: String
  , subPort  :: Int
  , subQuery :: FilePath
  }


hostP = strOption
          ( long "host"
         <> short 'h'
         <> metavar "HOST"
         <> help "Riemann Server Host"
         <> value "127.0.0.1"
          )

portP n = option auto
          ( long "port"
         <> short 'p'
         <> metavar "PORT"
         <> help "Riemann Server Port"
         <> value n
          )

specParser :: Parser Spec
specParser = Spec
  <$> hostP
  <*> portP 5555
  <*> argument str ( metavar "FILE" <> help "File containing hydra metrics")

execSpec (Spec host port metricsFile) = do
  c <- tcpClient host port
  sendHydraMetrics c metricsFile

main :: IO ()
main = execSpec =<< execParser opts
  where
    opts = info (specParser <**> helper)
      ( fullDesc
      <> header "hydra-riemann - send metrics gathered by hydra to riemann"
      )
