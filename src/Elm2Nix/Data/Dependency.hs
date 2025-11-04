{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.Dependency (Dependency(..), toUrl) where

import qualified Elm2Nix.Data.Name as Name

import Elm2Nix.Data.Name (Name)
import Elm2Nix.Data.Version (Version(..))
import Elm2Nix.Lib.Nix (Url)


data Dependency
    = Dependency
        { name :: Name
        , version :: Version
        }
    deriving (Eq, Ord)


instance Show Dependency where
    show (Dependency name version) =
        Name.toString "-" name ++ "-" ++ show version


toUrl :: Dependency -> Url
toUrl (Dependency name version) =
    "https://github.com/" ++ show name ++ "/archive/" ++ show version ++ ".tar.gz"
