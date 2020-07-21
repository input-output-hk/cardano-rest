{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Rest.Types
  ( WebserverConfig(..)
  , toWarpSettings
  ) where

import Data.Function
    ( (&) )
import qualified Network.Wai.Handler.Warp as Warp

------------------------------------------------------------
data WebserverConfig =
  WebserverConfig
    { wcHost :: Warp.HostPreference
    , wcPort :: Warp.Port
    }
  deriving (Show, Eq)

toWarpSettings :: WebserverConfig -> Warp.Settings
toWarpSettings (WebserverConfig {wcHost, wcPort}) =
  Warp.defaultSettings & Warp.setHost wcHost & Warp.setPort wcPort
