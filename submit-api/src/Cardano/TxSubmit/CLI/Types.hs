module Cardano.TxSubmit.CLI.Types
  ( ConfigFile (..)
  , GenesisFile (..)
  , SocketPath (..)
  , TxSubmitNodeParams (..)
  ) where

import Cardano.Api.Protocol
    ( Protocol (..) )
import Cardano.Api.Typed
    ( NetworkId (..) )
import Cardano.TxSubmit.Types

-- | The product type of all command line arguments
data TxSubmitNodeParams = TxSubmitNodeParams
  { tspConfigFile :: !ConfigFile
  , tspProtocol :: !Protocol
  , tspNetworkId :: !NetworkId
  , tspSocketPath :: !SocketPath
  , tspWebPort :: !TxSubmitPort
  }

newtype ConfigFile = ConfigFile
  { unConfigFile :: FilePath
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  }

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  }
