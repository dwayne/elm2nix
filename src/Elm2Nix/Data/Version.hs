module Elm2Nix.Data.Version (Version(..)) where

import qualified Data.Aeson as Json
import qualified Data.Text as Text

import Data.Aeson (ToJSON(..))
import Numeric.Natural (Natural)


data Version
    = Version
        { major :: Natural
        , minor :: Natural
        , patch :: Natural
        }
    deriving (Eq, Ord)


instance Show Version where
    show (Version major minor patch) =
        show major ++ "." ++ show minor ++ "." ++ show patch


instance ToJSON Version where
    toJSON = Json.String . Text.pack . show
    toEncoding = toEncoding . Text.pack . show
