{-# LANGUAGE OverloadedStrings #-}

import Cardano.Db
    ( readPGPassFileEnv )
import Cardano.Rest.Parsers
    ( pWebserverConfig )
import Explorer.Web
    ( runServer )
import Options.Applicative
    ( ParserPrefs )
import qualified Options.Applicative as Opt

main :: IO ()
main = do
  webserverConfig <- Opt.customExecParser prefs opts
  pgConfig <- readPGPassFileEnv
  runServer webserverConfig pgConfig
  where
    opts =
      Opt.info
        (Opt.helper <*> pWebserverConfig 8100)
        (Opt.fullDesc <>
         Opt.header
           "cardano-explorer-api - A block explorer for the cardano network")
    prefs :: ParserPrefs
    prefs = Opt.prefs $ Opt.showHelpOnEmpty <> Opt.showHelpOnError
