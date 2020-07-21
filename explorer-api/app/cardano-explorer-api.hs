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
        (Opt.helper <*> pWebserverConfig)
        (Opt.fullDesc <>
         Opt.header
           "cardano-explorer-api - Run a reporting API server that explores the blockchain")
    prefs :: ParserPrefs
    prefs = Opt.prefs $ Opt.showHelpOnEmpty <> Opt.showHelpOnError
