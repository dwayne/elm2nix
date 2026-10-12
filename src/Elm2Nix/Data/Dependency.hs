{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.Dependency (Dependency(..), toString, toUrl) where

import qualified Elm2Nix.Data.Name as Name

import Elm2Nix.Data.Name (Name)
import Elm2Nix.Data.Version (Version)


data Dependency
  = Dependency Name Version
  deriving (Eq, Ord)


type Url = String



-- Instances



instance Show Dependency where
  show = toString



-- Convert



toString :: Dependency -> String
toString (Dependency name version) =
  Name.toString "-" name ++ "-" ++ show version


toUrl :: Dependency -> Url
toUrl (Dependency name version) =
  "https://github.com/" ++ show name ++ "/archive/" ++ show version ++ ".tar.gz"
