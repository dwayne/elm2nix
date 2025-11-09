{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.Dependency
    ( Dependency(..)
    , toAuthor, toPackage
    , toString
    , toUrl
    ) where

import qualified Elm2Nix.Data.Name as Name

import Elm2Nix.Data.Name (Name, Author, Package)
import Elm2Nix.Data.Version (Version)
import Elm2Nix.Lib.Nix (Url)


data Dependency
    = Dependency
        { toName :: Name
        , toVersion :: Version
        }
    deriving (Eq, Ord)



-- INSTANCES



instance Show Dependency where
    show = toString



-- CONVERT



toAuthor :: Dependency -> Author
toAuthor (Dependency name _) =
    Name.toAuthor name


toPackage :: Dependency -> Package
toPackage (Dependency name _) =
    Name.toPackage name


toString :: Dependency -> String
toString (Dependency name version) =
    Name.toString "-" name ++ "-" ++ show version


toUrl :: Dependency -> Url
toUrl (Dependency name version) =
    "https://github.com/" ++ show name ++ "/archive/" ++ show version ++ ".tar.gz"
