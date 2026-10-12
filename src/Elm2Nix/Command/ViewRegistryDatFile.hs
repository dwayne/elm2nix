module Elm2Nix.Command.ViewRegistryDatFile (Input(..), Error(..), run, toRegistryDat) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.IO as TIO
import qualified Elm2Nix.Data.Bytes.Decode as BD
import qualified Elm2Nix.Data.RegistryDat as RegistryDat
import qualified Json
import qualified System.IO.Error as IOError

import Control.Exception (tryJust)
import Elm2Nix.Data.ElmVersion (ElmVersion)
import Elm2Nix.Data.RegistryDat (RegistryDat)
import Prelude hiding (readFile)
import System.IO (stdout)


data Input
  = Input
      { elmVersion :: ElmVersion
      , compact :: Bool
      , file :: FilePath
      }
  deriving (Eq, Show)


data Error
  = FileNotFound FilePath
  | DecodeError FilePath BD.DecodeError
  deriving (Eq, Show)


run :: Input -> IO (Either Error ())
run (Input elmVersion compact file) = do
  registryDatOrErr <- toRegistryDat elmVersion file

  case registryDatOrErr of
    Right registryDat ->
      let
        ( put, toText ) =
          if compact then
            ( TIO.hPutStr, Json.compact )
          else
            ( TIO.hPutStrLn, Json.pretty 4 )
      in
      Right <$> put stdout (toText (RegistryDat.toJson registryDat))

    Left err ->
      return (Left err)


toRegistryDat :: ElmVersion -> FilePath -> IO (Either Error RegistryDat)
toRegistryDat elmVersion file = do
  contentsOrErr <- readFile file

  case contentsOrErr of
    Right contents ->
      case RegistryDat.fromLazyByteString elmVersion contents of
        Right registryDat ->
          return (Right registryDat)

        Left err ->
          return (Left (DecodeError file err))

    Left err ->
      return (Left err)


readFile :: FilePath -> IO (Either Error LBS.ByteString)
readFile file =
  tryJust handleException (LBS.readFile file)
  where
    handleException e =
      if IOError.isDoesNotExistError e then
        Just (FileNotFound file)

      else
        Nothing
