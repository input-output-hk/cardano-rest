{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Cardano.TxSubmit.Tracing.ToObjectOrphans () where

import Cardano.BM.Data.Severity
import Cardano.BM.Data.Tracer
import Data.Aeson
    ( (.=) )
import Ouroboros.Network.NodeToClient
    ( ErrorPolicyTrace (..), WithAddr (..) )

import qualified Network.Socket as Socket

instance HasPrivacyAnnotation (WithAddr Socket.SockAddr ErrorPolicyTrace)
instance HasSeverityAnnotation (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  getSeverityAnnotation (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {} -> Warning -- peer misbehaved
    ErrorPolicySuspendConsumer {} -> Notice -- peer temporarily not useful
    ErrorPolicyLocalNodeError {} -> Error
    ErrorPolicyResumePeer {} -> Debug
    ErrorPolicyKeepSuspended {} -> Debug
    ErrorPolicyResumeConsumer {} -> Debug
    ErrorPolicyResumeProducer {} -> Debug
    ErrorPolicyUnhandledApplicationException {} -> Error
    ErrorPolicyUnhandledConnectionException {} -> Error
    ErrorPolicyAcceptException {} -> Error

instance ToObject (WithAddr Socket.SockAddr ErrorPolicyTrace) where
  toObject _verb (WithAddr addr ev) =
    mkObject [ "kind" .= ("ErrorPolicyTrace" :: String)
             , "address" .= show addr
             , "event" .= show ev ]
