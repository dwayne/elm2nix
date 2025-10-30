module Elm2Nix.Data.Dependency
    ( Author
    , Dependency(..)
    , Package
    , toName
    , toUrl
    ) where

import qualified Data.Text as Text

import Data.Text (Text)

import Elm2Nix.Data.Version (Version(..))
import Elm2Nix.Lib.Nix (Url)


type Author = Text
type Package = Text


data Dependency
    = Dependency
        { author :: Author
        , package :: Package
        , version :: Version
        }
    deriving (Eq, Ord, Show)


toName :: Dependency -> String
toName (Dependency author package version) =
    Text.unpack author ++ "-" ++ Text.unpack package ++ "-" ++ show version


toUrl :: Dependency -> Url
toUrl (Dependency author package version) =
    "https://github.com/" ++ Text.unpack author ++ "/" ++ Text.unpack package ++ "/archive/" ++ show version ++ ".tar.gz"
