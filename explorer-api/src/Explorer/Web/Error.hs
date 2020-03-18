{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types describing runtime errors related to Explorer

module Explorer.Web.Error
  ( ExplorerError (..)
  , renderExplorerError
  ) where

import Cardano.Db
    ( LookupFail (..), renderLookupFail )
import Data.Aeson
    ( ToJSON (..), Value (..) )
import Data.Text
    ( Text )
import Formatting
    ( bprint, stext, (%) )
import Formatting.Buildable
    ( Buildable )
import GHC.Generics
    ( Generic )

import qualified Formatting.Buildable

data ExplorerError
  = Internal Text  -- Stupid error constructor from the old code base.
  | EELookupFail !LookupFail
  deriving (Generic)

instance Buildable ExplorerError where
  build ee =
    case ee of
      Internal msg -> bprint ("Internal explorer error ("%stext%")") msg
      EELookupFail err -> bprint stext $ renderLookupFail err

renderExplorerError :: ExplorerError -> Text
renderExplorerError ee =
  case ee of
      Internal msg -> mconcat [ "Internal explorer error: ", msg ]
      EELookupFail err -> renderLookupFail err


instance ToJSON ExplorerError where
  toJSON ee =
    case ee of
      Internal msg -> String msg
      EELookupFail err -> String $ renderLookupFail err
