module Elm2Nix.Data.RegistryDat
    ( RegistryDat
    , fromElmJson, fromList, fromSet
    , toCount, toPackages
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Binary (Binary(..))
import Data.Map (Map)
import Data.Set (Set)

import qualified Elm2Nix.Data.ElmJson as ElmJson

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.ElmJson (ElmJson)
import Elm2Nix.Data.Name (Name)
import Elm2Nix.Data.Version (Version)


data RegistryDat
    = RegistryDat
        --
        -- _count    - The number of unique dependencies in _packages
        -- _packages - Maps the name of a package to its versions where
        --             the versions are in descending order
        --
        -- For e.g. if _packages = fromList [ ( elm/browser, [ 1.0.2, 1.0.1, 1.0.0 ] ), ( elm/core, [ 1.0.5, 1.0.0 ] ) ]
        -- then _count = 5.
        --
        { _count :: !Int
        , _packages :: !(Map Name Versions)
        }
    deriving (Eq, Show)


--
-- N.B. This type is primarily used so that we can provide a different binary serialization of the list type.
--
newtype Versions =
    Versions
        { toVersions :: [Version]
        }
    deriving (Eq, Show)



-- INSTANCES



instance Binary RegistryDat where
    put (RegistryDat count packages) = put count >> put packages
    get = RegistryDat <$> get <*> get


instance Binary Versions where
    put (Versions (v : vs)) = put v >> put vs
    --
    -- It should be non-empty by construction. If this occurs then there's an error in your logic.
    --
    put _ = error "logic error: no versions found"

    get = Versions <$> ((:) <$> get <*> get)



-- CONSTRUCT



fromElmJson :: ElmJson -> RegistryDat
fromElmJson = fromSet . ElmJson.toSet


fromList :: [Dependency] -> RegistryDat
fromList = fromSet . Set.fromList


fromSet :: Set Dependency -> RegistryDat
fromSet =
    uncurry RegistryDat . fmap (Map.map (Versions . Set.toDescList)) . foldr insert ( 0, Map.empty )
    where
        insert :: Dependency -> ( Int, Map Name (Set Version) ) -> ( Int, Map Name (Set Version) )
        insert (Dependency name version) ( count, packages ) =
            ( count + 1, Map.insertWith (<>) name (Set.singleton version) packages )



-- CONVERT



toCount :: RegistryDat -> Int
toCount (RegistryDat count _) = count


toPackages :: RegistryDat -> Map Name [Version]
toPackages (RegistryDat _ packages) = Map.map toVersions packages
