{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.ElmLock
  ( ElmLock
  , fromFile, fromList
  , elmLockDecoder
  , toSet
  ) where

import qualified Data.Json.Decode as JD
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Elm2Nix.Data.Name as Name

import Data.Set (Set)
import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.Name (Name)
import Elm2Nix.Data.Version (Version)


newtype ElmLock = ElmLock (Set Dependency)
  deriving (Eq, Show)



-- Construct



fromFile :: FilePath -> IO (Either JD.Error ElmLock)
fromFile = JD.decodeFile elmLockDecoder


fromList :: [Dependency] -> ElmLock
fromList = ElmLock . Set.fromList



-- Decoder



elmLockDecoder :: JD.Decoder ElmLock
elmLockDecoder = fromList <$> JD.list dependencyDecoder


dependencyDecoder :: JD.Decoder Dependency
dependencyDecoder = Dependency <$> nameDecoder <*> versionDecoder


nameDecoder :: JD.Decoder Name
nameDecoder = do
  author <- JD.field "author" JD.text
  package <- JD.field "package" JD.text
  case Name.fromText (author <> "/" <> package) of
    Right name ->
      JD.succeed name

    Left err ->
      JD.fail (T.pack $ Name.fromTextErrorToString err)


versionDecoder :: JD.Decoder Version
versionDecoder = JD.field "version" JD.decoder



-- Convert



toSet :: ElmLock -> Set Dependency
toSet (ElmLock dependencies) = dependencies
