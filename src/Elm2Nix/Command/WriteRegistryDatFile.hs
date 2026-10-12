module Elm2Nix.Command.WriteRegistryDatFile (Error(..), run) where

import qualified Data.ByteString.Lazy as LBS
import qualified Elm2Nix.Data.ElmLock as ElmLock
import qualified Elm2Nix.Data.RegistryDat as RegistryDat
import qualified Json.Decode as JD

import Data.Function ((&))
import Elm2Nix.Data.RegistryDat.ElmVersion (ElmVersion)


data Error
  = JsonDecodeError FilePath JD.Error
  deriving (Eq, Show)


run :: ElmVersion -> FilePath -> FilePath -> IO (Either Error ())
run elmVersion inFile outFile = do
  elmLockOrErr <- ElmLock.fromFile inFile

  case elmLockOrErr of
    Right elmLock ->
      elmLock
        & RegistryDat.fromElmLock
        & RegistryDat.toLazyByteString elmVersion
        & LBS.writeFile outFile
        & fmap Right

    Left err ->
      return $ Left $ JsonDecodeError inFile err
