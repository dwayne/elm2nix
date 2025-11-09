{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Elm2Nix.Data.FixedOutputDerivation
    ( FixedOutputDerivation
    , FromDependencyError, fromDependency, fromNameAndVersion
    , FromDependenciesError, fromDependencies, fromElmJson
    , toDependency, toHash, toPath
    ) where

import qualified Data.Aeson as Json

import Data.Aeson (ToJSON(..), (.=))
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import UnliftIO.Async (pooledMapConcurrently)

import qualified Elm2Nix.Data.Dependency as Dependency
import qualified Elm2Nix.Data.ElmJson as ElmJson

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.ElmJson (ElmJson)
import Elm2Nix.Data.Name (Name)
import Elm2Nix.Data.Version (Version)
import Elm2Nix.Lib.Nix (NixPrefetchUrlError, NixPrefetchUrlOutput(..), Sha256, nixPrefetchUrl)


data FixedOutputDerivation
    = FixedOutputDerivation
        { dependency :: Dependency
        , hash :: Sha256
        , path :: FilePath
        }
    deriving (Eq, Ord, Show)


instance ToJSON FixedOutputDerivation where
    toJSON (FixedOutputDerivation dependency hash _) =
        Json.object
            [ "author" .= Dependency.toAuthor dependency
            , "package" .= Dependency.toPackage dependency
            , "version" .= show (Dependency.toVersion dependency)
            , "sha256" .= hash
            ]

    toEncoding (FixedOutputDerivation dependency hash _) =
        Json.pairs $
            "author" .= Dependency.toAuthor dependency <>
            "package" .= Dependency.toPackage dependency <>
            "version" .= show (Dependency.toVersion dependency) <>
            "sha256" .= hash


type FromDependencyError = NixPrefetchUrlError


fromDependency :: Dependency -> IO (Either FromDependencyError FixedOutputDerivation)
fromDependency dependency =
    fmap convert <$> nixPrefetchUrl (Dependency.toUrl dependency) (Dependency.toString dependency)
    where
        convert :: NixPrefetchUrlOutput -> FixedOutputDerivation
        convert (NixPrefetchUrlOutput hash path) =
            FixedOutputDerivation dependency hash path


fromNameAndVersion :: Name -> Version -> IO (Either FromDependencyError FixedOutputDerivation)
fromNameAndVersion name = fromDependency . Dependency name


type FromDependenciesError = [( Dependency, NixPrefetchUrlError )]


fromDependencies :: [Dependency] -> IO (Either FromDependenciesError [FixedOutputDerivation])
fromDependencies =
    fmap (resolve . partitionEithers) . pooledMapConcurrently (\d -> first (d,) <$> fromDependency d)
    where
        resolve :: ( FromDependenciesError, [FixedOutputDerivation] ) -> Either FromDependenciesError [FixedOutputDerivation]
        resolve ( err, fods ) =
            if null err then
                Right fods

            else
                Left err


fromElmJson :: ElmJson -> IO (Either FromDependenciesError [FixedOutputDerivation])
fromElmJson =
    fromDependencies . ElmJson.toDependencies


toDependency :: FixedOutputDerivation -> Dependency
toDependency (FixedOutputDerivation dependency _ _) = dependency


toHash :: FixedOutputDerivation -> Sha256
toHash (FixedOutputDerivation _ hash _) = hash


toPath :: FixedOutputDerivation -> FilePath
toPath (FixedOutputDerivation _ _ path) = path
