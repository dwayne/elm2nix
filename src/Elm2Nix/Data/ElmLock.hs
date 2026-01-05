module Elm2Nix.Data.ElmLock
    ( ElmLock
    , fromFile, fromList
    , decoder
    , toSet
    ) where

import qualified Data.Set as Set
import qualified Data.Text as T

import Data.Set (Set)

import qualified Elm2Nix.Data.Name as Name
import qualified Elm2Nix.Data.Version as Version
import qualified Elm2Nix.Lib.Json.Decode as JD

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.Name (Name)


newtype ElmLock = ElmLock (Set Dependency)
    deriving (Eq, Show)


fromFile :: FilePath -> IO (Either JD.Error ElmLock)
fromFile = JD.decodeFile decoder


fromList :: [Dependency] -> ElmLock
fromList = ElmLock . Set.fromList


decoder :: JD.Decoder ElmLock
decoder =
    fromList <$> JD.list dependencyDecoder


dependencyDecoder :: JD.Decoder Dependency
dependencyDecoder =
    Dependency <$> nameDecoder <*> JD.field "version" Version.decoder


nameDecoder :: JD.Decoder Name
nameDecoder = do
    author <- JD.field "author" JD.string
    package <- JD.field "package" JD.string
    case Name.fromText (T.pack $ author ++ "/" ++ package) of
        Right name ->
            JD.succeed name

        Left err ->
            JD.failWith (Name.fromTextErrorToString err)


toSet :: ElmLock -> Set Dependency
toSet (ElmLock dependencies) = dependencies
