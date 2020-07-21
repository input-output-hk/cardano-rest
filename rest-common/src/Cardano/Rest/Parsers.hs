{-# LANGUAGE OverloadedStrings #-}
module Cardano.Rest.Parsers (pWebserverConfig) where

import Cardano.Rest.Types
    ( WebserverConfig (WebserverConfig) )
import Network.Wai.Handler.Warp
    ( HostPreference, Port )
import Options.Applicative
    ( Parser )
import Options.Applicative
    ( auto, help, long, metavar, option, showDefault, value )

pWebserverConfig :: Parser WebserverConfig
pWebserverConfig = WebserverConfig <$> hostPreferenceOption <*> portOption

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
