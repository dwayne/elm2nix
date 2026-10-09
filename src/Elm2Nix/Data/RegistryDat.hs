{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.RegistryDat
  ( RegistryDat
  , fromElmLock, fromElmJson, fromList, fromSet, fromLazyByteString
  , toCount, toPackages, toAllPackages, toLazyByteString
  , binaryEncoderV0_19_1, binaryDecoderV0_19_1
  , encodeRegistryDat
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Elm2Nix.Data.Bytes.DecodeV0_19_1 as DV0_19_1
import qualified Elm2Nix.Data.Bytes.EncodeV0_19_1 as EV0_19_1
import qualified Elm2Nix.Data.ElmJson as ElmJson
import qualified Elm2Nix.Data.ElmLock as ElmLock
import qualified Elm2Nix.Data.Name as Name
import qualified Elm2Nix.Data.Version as Version
import qualified Json.Encode as JE

import Data.Bifunctor (second)
import Data.Function ((&))
import Data.List (sort)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.ElmJson (ElmJson)
import Elm2Nix.Data.ElmLock (ElmLock)
import Elm2Nix.Data.ElmVersion (ElmVersion(..))
import Elm2Nix.Data.Name (Name)
import Elm2Nix.Data.Version (Version)
import Json.Encode (Json, ToJson)


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
newtype Versions
  = Versions
      { toVersions :: [Version]
      }
  deriving (Eq, Show)



-- Instances



instance ToJson RegistryDat where
  encode = encodeRegistryDat



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


fromLazyByteString :: ElmVersion -> LBS.ByteString -> RegistryDat
fromLazyByteString v =
  case v of
    V0_19_1 ->
      DV0_19_1.decode binaryDecoderV0_19_1



-- Convert



toCount :: RegistryDat -> Int
toCount (RegistryDat count _) = count


toPackages :: RegistryDat -> Map Name [Version]
toPackages (RegistryDat _ packages) = Map.map toVersions packages


toAllPackages :: RegistryDat -> [(Text, [Text])]
toAllPackages (RegistryDat _ packages) =
  --
  -- Maps "author/package" to a list of version strings such that
  -- the versions have been sorted from oldest to latest
  --
  packages
      & Map.toAscList
      & map (\( name, Versions versions ) -> ( Name.toText "/" name, map T.show (sort versions) ))


toLazyByteString :: ElmVersion -> RegistryDat -> LBS.ByteString
toLazyByteString v =
  case v of
    V0_19_1 ->
      EV0_19_1.encode . binaryEncoderV0_19_1



-- Binary Encoder



binaryEncoderV0_19_1 :: RegistryDat -> EV0_19_1.Encoder
binaryEncoderV0_19_1 (RegistryDat count packages) =
  EV0_19_1.int count <> EV0_19_1.dict Name.binaryEncoderV0_19_1 versionsBinaryEncoderV0_19_1 packages


versionsBinaryEncoderV0_19_1 :: Versions -> EV0_19_1.Encoder
versionsBinaryEncoderV0_19_1 (Versions versions) =
  case versions of
    v : vs ->
      Version.binaryEncoderV0_19_1 v <> EV0_19_1.list Version.binaryEncoderV0_19_1 vs

    _ ->
      error "logic error: no versions found"



-- Binary Decoder



binaryDecoderV0_19_1 :: DV0_19_1.Decoder RegistryDat
binaryDecoderV0_19_1 =
  RegistryDat <$> DV0_19_1.int <*> DV0_19_1.dict Name.binaryDecoderV0_19_1 versionsBinaryDecoderV0_19_1


versionsBinaryDecoderV0_19_1 :: DV0_19_1.Decoder Versions
versionsBinaryDecoderV0_19_1 =
  (\v vs -> Versions $ v : vs) <$> Version.binaryDecoderV0_19_1 <*> DV0_19_1.list Version.binaryDecoderV0_19_1



-- JSON Encoder



encodeRegistryDat :: RegistryDat -> Json
encodeRegistryDat =
  JE.object . map (second JE.encode) . toAllPackages
