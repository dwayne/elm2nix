module Elm2Nix.Data.RegistryDat
    ( RegistryDat
    , fromElmJson, fromList, fromSet
    , toCount, toPackages
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Map (Map)
import Data.Set (Set)

import qualified Elm2Nix.Data.ElmJson as ElmJson

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.ElmJson (ElmJson)
import Elm2Nix.Data.Name (Name)
import Elm2Nix.Data.Version (Version)


data RegistryDat
    = RegistryDat
        { _count :: !Int
        , _packages :: !(Map Name (Set Version))
        }
    deriving (Eq, Show)



-- CONSTRUCT



fromElmJson :: ElmJson -> RegistryDat
fromElmJson = fromSet . ElmJson.toSet


fromList :: [Dependency] -> RegistryDat
fromList = fromSet . Set.fromList


fromSet :: Set Dependency -> RegistryDat
fromSet =
    uncurry RegistryDat . foldr insert ( 0, Map.empty )
    where
        insert :: Dependency -> ( Int, Map Name (Set Version) ) -> ( Int, Map Name (Set Version) )
        insert (Dependency name version) ( count, packages ) =
            ( count + 1, Map.insertWith (<>) name (Set.singleton version) packages )



-- CONVERT



toCount :: RegistryDat -> Int
toCount (RegistryDat count _) = count


toPackages :: RegistryDat -> Map Name (Set Version)
toPackages (RegistryDat _ packages) = packages
