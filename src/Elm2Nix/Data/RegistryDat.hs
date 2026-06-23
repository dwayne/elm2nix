{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.RegistryDat
  ( RegistryDat
  , fromElmLock, fromElmJson, fromList, fromSet
  , toCount, toPackages
  ) where

import qualified Data.Json.Encode as JE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Elm2Nix.Data.ElmJson as ElmJson
import qualified Elm2Nix.Data.ElmLock as ElmLock
import qualified Elm2Nix.Data.Name as Name

import Data.Binary (Binary(..))
import Data.Function ((&))
import Data.Json.Encode (ToJson(encode))
import Data.List (sort)
import Data.Map (Map)
import Data.Set (Set)
import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.ElmJson (ElmJson)
import Elm2Nix.Data.ElmLock (ElmLock)
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



-- Instances



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


instance ToJson RegistryDat where
  encode (RegistryDat _ packages) =
    --
    -- Maps "author/package" to a list of version strings such that
    -- the versions have been sorted from oldest to latest
    --
    packages
      & Map.toAscList
      & map (\( name, Versions versions ) -> ( Name.toText "/" name, JE.encode $ map T.show (sort versions) ))
      & JE.object



-- Construct



fromElmLock :: ElmLock -> RegistryDat
fromElmLock = fromSet . ElmLock.toSet


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



-- Convert



toCount :: RegistryDat -> Int
toCount (RegistryDat count _) = count


toPackages :: RegistryDat -> Map Name [Version]
toPackages (RegistryDat _ packages) = Map.map toVersions packages
