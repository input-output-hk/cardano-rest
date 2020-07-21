{-# LANGUAGE OverloadedStrings #-}

import Cardano.Db
    ( readPGPassFileEnv )
import Explorer.Web
    ( WebserverConfig (WebserverConfig), runServer )
import Network.Wai.Handler.Warp
    ( HostPreference, Port )
import Options.Applicative
    ( Parser, ParserPrefs )
import Options.Applicative
    ( auto, help, long, metavar, option, showDefault, value )
import qualified Options.Applicative as Opt

main :: IO ()
main = do
  webserverConfig <- Opt.customExecParser prefs opts
  pgConfig <- readPGPassFileEnv
  runServer webserverConfig pgConfig
  where
    opts =
      Opt.info
        (Opt.helper <*> pConfig)
        (Opt.fullDesc <>
         Opt.header
           "cardano-explorer-api - Run a reporting API server that explores the blockchain")
    prefs :: ParserPrefs
    prefs = Opt.prefs $ Opt.showHelpOnEmpty <> Opt.showHelpOnError

pConfig :: Parser WebserverConfig
pConfig = WebserverConfig <$> hostPreferenceOption <*> portOption

------------------------------------------------------------
hostPreferenceOption :: Parser HostPreference
hostPreferenceOption =
  option auto $
  long "listen-address" <>
  metavar "HOST" <>
  help
    ("Specification of which host to the bind API server to. " <>
     "Can be an IPv[46] address, hostname, or '*'.") <>
  value "127.0.0.1" <> showDefault

portOption :: Parser Port
portOption =
  option auto $
  long "port" <>
  metavar "INT" <>
  help "port used for serving the wallet API." <> value 8090 <> showDefault
