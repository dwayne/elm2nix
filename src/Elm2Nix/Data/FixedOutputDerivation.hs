module Elm2Nix.Data.FixedOutputDerivation
    ( FixedOutputDerivation(..)
    , FromDependencyError
    , fromDependency
    ) where

import qualified Elm2Nix.Data.Dependency as Dependency

import Elm2Nix.Data.Dependency (Author, Dependency(..), Package)
import Elm2Nix.Data.Version (Version)
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


type FromDependencyError = NixPrefetchUrlError


fromDependency :: Dependency -> IO (Either FromDependencyError FixedOutputDerivation)
fromDependency dependency@(Dependency author package version) =
    fmap convert <$> nixPrefetchUrl (Dependency.toUrl dependency) (Dependency.toName dependency)
    where
        convert :: NixPrefetchUrlOutput -> FixedOutputDerivation
        convert (NixPrefetchUrlOutput hash path) =
            FixedOutputDerivation author package version hash path
