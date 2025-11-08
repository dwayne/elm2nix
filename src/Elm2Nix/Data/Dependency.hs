{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.Dependency
    ( Dependency(..)
    , toName, toAuthor, toPackage, toVersion
    , toString
    , toUrl
    ) where

import qualified Elm2Nix.Data.Name as Name

import Elm2Nix.Data.Name (Name, Author, Package)
import Elm2Nix.Data.Version (Version)
import Elm2Nix.Lib.Nix (Url)


data Dependency
    = Dependency
        { name :: Name
        , version :: Version
        }
    deriving (Eq, Ord)


instance Show Dependency where
    show = toString


toName :: Dependency -> Name
toName (Dependency name _) = name


toAuthor :: Dependency -> Author
toAuthor (Dependency name _) = Name.toAuthor name


toPackage :: Dependency -> Package
toPackage (Dependency name _) = Name.toPackage name


toVersion :: Dependency -> Version
toVersion (Dependency _ version) = version


toString :: Dependency -> String
toString (Dependency name version) =
    Name.toString "-" name ++ "-" ++ show version


toUrl :: Dependency -> Url
toUrl (Dependency name version) =
    "https://github.com/" ++ show name ++ "/archive/" ++ show version ++ ".tar.gz"
