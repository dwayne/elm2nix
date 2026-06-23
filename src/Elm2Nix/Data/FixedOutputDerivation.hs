{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Elm2Nix.Data.FixedOutputDerivation
  ( FixedOutputDerivation
  , FromDependencyError, fromDependency, fromNameAndVersion
  , FromDependenciesError, fromDependencies, fromElmJson
  , toDependency, toHash, toPath
  ) where

import qualified Data.Json.Encode as JE
import qualified Data.Text as T
import qualified Elm2Nix.Data.Dependency as Dependency
import qualified Elm2Nix.Data.ElmJson as ElmJson
import qualified Elm2Nix.Data.Name as Name

import Data.Json.Encode (ToJson(encode))
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.ElmJson (ElmJson)
import Elm2Nix.Data.Name (Name)
import Elm2Nix.Data.Version (Version)
import Elm2Nix.Lib.Nix (NixPrefetchUrlError, NixPrefetchUrlOutput(..), Sha256, nixPrefetchUrl)
import UnliftIO.Async (pooledMapConcurrently)


data FixedOutputDerivation
  = FixedOutputDerivation
    { _dependency :: Dependency
    , _hash :: Sha256
    , _path :: FilePath
    }
  deriving (Eq, Ord, Show)



-- Instances



instance ToJson FixedOutputDerivation where
  encode (FixedOutputDerivation (Dependency name version) hash _) =
    JE.object
      [ ( "author", encode $ Name.toAuthor name )
      , ( "package", encode $ Name.toPackage name )
      , ( "version", encode $ T.show version )
      , ( "sha256", encode $ T.pack hash )
      ]



-- Construct



type FromDependencyError = NixPrefetchUrlError


fromDependency :: Dependency -> IO (Either FromDependencyError FixedOutputDerivation)
fromDependency dependency =
  fmap toFOD <$> nixPrefetchUrl (Dependency.toUrl dependency) (Dependency.toString dependency)
  where
    toFOD :: NixPrefetchUrlOutput -> FixedOutputDerivation
    toFOD (NixPrefetchUrlOutput hash path) =
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
fromElmJson = fromDependencies . ElmJson.toAscList



-- Convert



toDependency :: FixedOutputDerivation -> Dependency
toDependency (FixedOutputDerivation dependency _ _) = dependency


toHash :: FixedOutputDerivation -> Sha256
toHash (FixedOutputDerivation _ hash _) = hash


toPath :: FixedOutputDerivation -> FilePath
toPath (FixedOutputDerivation _ _ path) = path
