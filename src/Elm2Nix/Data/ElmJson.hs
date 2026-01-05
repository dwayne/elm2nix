{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.ElmJson
    ( ElmJson
    , fromFile, fromFiles
    , fromList
    , toAscList
    , toSet
    , decoder, dependenciesDecoder, nameDecoder
    ) where

import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as T

import Data.Aeson.Key (Key)
import Data.Aeson.Types ((<?>), (.:?), FromJSON, JSONPathElement(..), Object, Parser, Value, emptyObject, parseFail)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Traversable.WithIndex (itraverse)

import qualified Elm2Nix.Data.Name as Name
import qualified Elm2Nix.Data.Version as Version
import qualified Elm2Nix.Lib.Json.Decode as JD

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.Name (Name)
import Elm2Nix.Data.Version (Version)


newtype ElmJson = ElmJson (Set Dependency)
    deriving (Eq)



-- INSTANCES



instance Show ElmJson where
    show (ElmJson dependencies) = show dependencies


instance FromJSON ElmJson where
    parseJSON = elmJsonParser


elmJsonParser :: Value -> Parser ElmJson
elmJsonParser =
    Json.withObject "ElmJson" $ \o ->
        ElmJson <$> (
            Set.union
                <$> optional o "dependencies" directAndIndirectParser
                <*> optional o "test-dependencies" directAndIndirectParser
        )


directAndIndirectParser :: Value -> Parser (Set Dependency)
directAndIndirectParser =
    Json.withObject "DirectAndIndirect" $ \o ->
        Set.union
            <$> optional o "direct" dependenciesParser
            <*> optional o "indirect" dependenciesParser


dependenciesParser :: Value -> Parser (Set Dependency)
dependenciesParser =
    Json.withObject "Dependencies" $ fmap (foldr Set.insert Set.empty) . itraverse dependencyParser


dependencyParser :: Key -> Value -> Parser Dependency
dependencyParser key value =
    (Dependency <$> nameParser key <*> versionParser value) <?> Key key


optional :: Object -> Key -> (Value -> Parser a) -> Parser a
optional o key f =
    (o .:? key >>= f . Maybe.fromMaybe emptyObject) <?> Key key


decoder :: JD.Decoder ElmJson
decoder =
    JD.field "type" (JD.literal "application") >>
        ((\a b c d -> fromList $ a ++ b ++ c ++ d)
            <$> pathToDependenciesDecoder [ "dependencies", "direct" ]
            <*> pathToDependenciesDecoder [ "dependencies", "indirect" ]
            <*> pathToDependenciesDecoder [ "test-dependencies", "direct" ]
            <*> pathToDependenciesDecoder [ "test-dependencies", "indirect" ])


pathToDependenciesDecoder :: [String] -> JD.Decoder [Dependency]
pathToDependenciesDecoder path =
    fmap (fromMaybe []) (JD.optionalAt path dependenciesDecoder)


dependenciesDecoder :: JD.Decoder [Dependency]
dependenciesDecoder =
    JD.keyValuePairs nameFromString Version.decoder >>= JD.succeed . map (uncurry Dependency)


nameFromString :: String -> Either String Name
nameFromString = first errorToString . Name.fromText . T.pack
    where
        errorToString Name.EmptyAuthor         = "author is empty"
        errorToString Name.EmptyPackage        = "package is empty"
        errorToString Name.MissingForwardSlash = "/ is missing"


nameParser :: Key -> Parser Name
nameParser =
    either (parseFail . errorToString) pure . Name.fromText . Key.toText
    where
        errorToString Name.EmptyAuthor         = "author is empty"
        errorToString Name.EmptyPackage        = "package is empty"
        errorToString Name.MissingForwardSlash = "/ is missing"


nameDecoder :: JD.Decoder Name
nameDecoder =
    JD.string >>= \s ->
        case Name.fromText (T.pack s) of
            Right name ->
                JD.succeed name

            Left err ->
                JD.failWith (errorToString err)

    where
        errorToString Name.EmptyAuthor         = "author is empty"
        errorToString Name.EmptyPackage        = "package is empty"
        errorToString Name.MissingForwardSlash = "/ is missing"


versionParser :: Value -> Parser Version
versionParser =
    Json.withText "Version" $ \t -> maybe (parseFail $ "version is invalid: " ++ show t) pure (Version.fromText t)



-- CONSTRUCT



fromFile :: FilePath -> IO (Either JD.Error ElmJson)
fromFile = JD.decodeFile decoder


fromFiles :: [FilePath] -> IO (Either (FilePath, JD.Error) ElmJson)
fromFiles =
    fromFilesHelper Set.empty


fromFilesHelper :: Set Dependency -> [FilePath] -> IO (Either (FilePath, JD.Error) ElmJson)
fromFilesHelper currentDeps paths =
    case paths of
        [] ->
            return $ Right (ElmJson currentDeps)

        path : restPaths -> do
            result <- fromFile path
            case result of
                --
                -- TODO: Use a function that doesn't wrap the dependencies into ElmJson prematurely.
                --
                Right (ElmJson nextDeps) ->
                    fromFilesHelper (Set.union currentDeps nextDeps) restPaths

                Left err ->
                    return $ Left ( path, err )


fromList :: [Dependency] -> ElmJson
fromList = ElmJson . Set.fromList



-- CONVERT



toAscList :: ElmJson -> [Dependency]
toAscList (ElmJson dependencies) = Set.toAscList dependencies


toSet :: ElmJson -> Set Dependency
toSet (ElmJson dependencies) = dependencies
