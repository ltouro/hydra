{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hydra.Prelude

import Hydra.Chain.Direct.Explorer (runServer)
import Hydra.Chain.Direct.Observer (ObserverConfig (..))
import Hydra.Options (networkIdParser, nodeSocketParser, startChainFromParser)
import Options.Applicative (
  Parser,
  ParserInfo,
  execParser,
  flag,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  progDesc,
  short,
 )
import Prelude (read)

optionsParser :: Parser ObserverConfig
optionsParser =
  ObserverConfig
    <$> networkIdParser
    <*> nodeSocketParser
    <*> optional startChainFromParser
    <*> dumpEventsParser

dumpEventsParser :: Parser Bool
dumpEventsParser =
  flag
    False
    True
    ( long "dump"
        <> short 'd'
        <> help "Dump events seen to stdout in JSON."
    )

toolsOptions :: ParserInfo ObserverConfig
toolsOptions =
  info
    ( optionsParser
        <**> helper
    )
    ( fullDesc
        <> progDesc "Hydra Head Observer"
        <> header "hydra-explorer - A Tool to explore Hydra Heads living on a Cardano network"
    )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  config <- execParser toolsOptions
  port <- maybe 8000 read <$> lookupEnv "HYDRA_OBSERVER_PORT"
  runServer port config