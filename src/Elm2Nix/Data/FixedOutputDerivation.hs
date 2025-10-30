{-# LANGUAGE TupleSections #-}

module Elm2Nix.Data.FixedOutputDerivation
    ( FixedOutputDerivation(..)
    , FromDependencyError, fromDependency
    , FromFileError, fromFile
    ) where

import qualified Data.Set as Set

import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Function ((&))
import UnliftIO.Async (pooledMapConcurrently)

import qualified Elm2Nix.Data.Dependency as Dependency
import qualified Elm2Nix.ElmJson as ElmJson
import qualified Elm2Nix.Lib.Json as Json

import Elm2Nix.Data.Dependency (Author, Dependency(..), Package)
import Elm2Nix.Data.Version (Version)
import Elm2Nix.Lib.Json (DecodeFileError)
import Elm2Nix.Lib.Nix (NixPrefetchUrlError, NixPrefetchUrlOutput(..), Sha256, nixPrefetchUrl)


data FixedOutputDerivation
    = FixedOutputDerivation
        { author :: Author
        , package :: Package
        , version :: Version
        , hash :: Sha256
        , path :: FilePath
        }
    deriving (Eq, Show)


data FromFileError
    = DecodeFileError DecodeFileError
    | JsonError String
    | NixPrefetchUrlErrors [( Dependency, NixPrefetchUrlError )]
    deriving (Eq, Show)


fromFile :: FilePath -> IO (Either FromFileError [FixedOutputDerivation])
fromFile filePath = do
    result <- Json.decodeFile filePath
    case result of
        Right value ->
            case ElmJson.getDependencies value of
                Right dependencies ->
                    dependencies
                        & Set.toAscList
                        & pooledMapConcurrently (\d -> first (d,) <$> fromDependency d)
                        & fmap (resolve . partitionEithers)

                Left err ->
                    return $ Left $ JsonError err

        Left err ->
            return $ Left $ DecodeFileError err

    where
        resolve :: ( [( Dependency, NixPrefetchUrlError )], [FixedOutputDerivation] ) -> Either FromFileError [FixedOutputDerivation]
        resolve ( errors, fods ) =
            if null errors then
                Right fods

            else
                Left $ NixPrefetchUrlErrors errors


type FromDependencyError = NixPrefetchUrlError


fromDependency :: Dependency -> IO (Either FromDependencyError FixedOutputDerivation)
fromDependency dependency@(Dependency author package version) =
    fmap convert <$> nixPrefetchUrl (Dependency.toUrl dependency) (Dependency.toName dependency)
    where
        convert :: NixPrefetchUrlOutput -> FixedOutputDerivation
        convert (NixPrefetchUrlOutput hash path) =
            FixedOutputDerivation author package version hash path
