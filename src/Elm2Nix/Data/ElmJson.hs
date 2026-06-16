{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.ElmJson
    ( ElmJson
    , fromFile, fromFiles, fromList
    , decoder, dependenciesDecoder
    , toAscList, toSet
    ) where

import qualified Data.Json.Decode as JD
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Elm2Nix.Data.Name as Name
import qualified Elm2Nix.Data.Version as Version

import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.Name (Name)


newtype ElmJson = ElmJson (Set Dependency)
    deriving (Eq)



-- INSTANCES



instance Show ElmJson where
    show (ElmJson dependencies) = show dependencies



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
                Right (ElmJson nextDeps) ->
                    fromFilesHelper (Set.union currentDeps nextDeps) restPaths

                Left err ->
                    return $ Left ( path, err )


fromList :: [Dependency] -> ElmJson
fromList = ElmJson . Set.fromList


decoder :: JD.Decoder ElmJson
decoder =
    JD.field "type" (literal "application") >>
        ((\a b c d -> fromList $ a ++ b ++ c ++ d)
            <$> pathToDependenciesDecoder [ "dependencies", "direct" ]
            <*> pathToDependenciesDecoder [ "dependencies", "indirect" ]
            <*> pathToDependenciesDecoder [ "test-dependencies", "direct" ]
            <*> pathToDependenciesDecoder [ "test-dependencies", "indirect" ])


literal :: Text -> JD.Decoder ()
literal expected =
    JD.text >>= \actual ->
        if actual == expected then
            JD.succeed ()

        else
            JD.fail $ "not equal to \"" <> expected <> "\": \"" <> actual <> "\""


pathToDependenciesDecoder :: [Text] -> JD.Decoder [Dependency]
pathToDependenciesDecoder path =
    fmap (fromMaybe []) (JD.optionalAt path dependenciesDecoder)


dependenciesDecoder :: JD.Decoder [Dependency]
dependenciesDecoder =
    JD.keyValuePairsWithKeyTransformer toName Version.decoder >>= JD.succeed . map (uncurry Dependency)


toName :: Text -> Either Text Name
toName = first (T.pack . Name.fromTextErrorToString) . Name.fromText



-- CONVERT



toAscList :: ElmJson -> [Dependency]
toAscList (ElmJson dependencies) = Set.toAscList dependencies


toSet :: ElmJson -> Set Dependency
toSet (ElmJson dependencies) = dependencies
