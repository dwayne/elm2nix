{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.ElmJson (ElmJson, fromDependencies, toDependencies) where

import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Set as Set

import Data.Aeson.Key (Key)
import Data.Aeson.Types ((.:), FromJSON, Object, Parser, Value, parseFail)
import Data.Set (Set)
import Data.Traversable.WithIndex (itraverse)

import qualified Elm2Nix.Data.Name as Name
import qualified Elm2Nix.Data.Version as Version

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.Name (Name)
import Elm2Nix.Data.Version (Version)


newtype ElmJson
    = ElmJson
        { _dependencies :: Set Dependency
        }
    deriving (Eq, Show)


instance FromJSON ElmJson where
    parseJSON = elmJsonParser


elmJsonParser :: Value -> Parser ElmJson
elmJsonParser =
    Json.withObject "ElmJson" $ \o ->
        ElmJson <$> (Set.union <$> directAndIndirectParser "dependencies" o <*> directAndIndirectParser "test-dependencies" o)


directAndIndirectParser :: String -> Object -> Parser (Set Dependency)
directAndIndirectParser field o =
    let
        key = Key.fromString field
    in
    (o .: key) >>= Json.withObject "DirectAndIndirect" (\di ->
        Set.union <$> (di .: "direct" >>= dependenciesParser) <*> (di .: "indirect" >>= dependenciesParser)
    )


dependenciesParser :: Value -> Parser (Set Dependency)
dependenciesParser =
    Json.withObject "Dependencies" $ fmap (foldr Set.insert Set.empty) . itraverse dependencyParser


dependencyParser :: Key -> Value -> Parser Dependency
dependencyParser key value =
    Dependency <$> nameParser key <*> versionParser value


nameParser :: Key -> Parser Name
nameParser =
    either (parseFail . errorToString) pure . Name.fromText . Key.toText
    where
        errorToString Name.EmptyAuthor         = "author is empty"
        errorToString Name.EmptyPackage        = "package is empty"
        errorToString Name.MissingForwardSlash = "/ is missing"


versionParser :: Value -> Parser Version
versionParser =
    Json.withText "Version" $ maybe (parseFail "invalid") pure . Version.fromText


fromDependencies :: [Dependency] -> ElmJson
fromDependencies = ElmJson . Set.fromList


toDependencies :: ElmJson -> Set Dependency
toDependencies (ElmJson dependencies) = dependencies
