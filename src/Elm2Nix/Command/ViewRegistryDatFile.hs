module Elm2Nix.Command.ViewRegistryDatFile (Error(..), run, toRegistryDat) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.IO as TIO
import qualified Elm2Nix.Data.RegistryDat as RegistryDat
import qualified Elm2Nix.Data.RegistryDat.Decode as RDD
import qualified Json
import qualified System.IO.Error as IOError

import Control.Exception (tryJust)
import Data.Function ((&))
import Elm2Nix.Data.RegistryDat (RegistryDat)
import Elm2Nix.Data.RegistryDat.ElmVersion (ElmVersion)
import Prelude hiding (readFile)
import System.IO (stdout)


data Error
  = FileNotFound FilePath
  | DecodeError FilePath RDD.DecodeError
  deriving (Eq, Show)


run :: ElmVersion -> Bool -> FilePath -> IO (Either Error ())
run elmVersion compact inFile = do
  registryDatOrErr <- toRegistryDat elmVersion inFile

  case registryDatOrErr of
    Right registryDat ->
      let
        ( put, toText ) =
          if compact then
            ( TIO.hPutStr, Json.compact )
          else
            ( TIO.hPutStrLn, Json.pretty 4 )
      in
      registryDat
        & RegistryDat.toJson
        & toText
        & put stdout
        & fmap Right

    Left err ->
      return $ Left err


toRegistryDat :: ElmVersion -> FilePath -> IO (Either Error RegistryDat)
toRegistryDat elmVersion inFile = do
  contentsOrErr <- readFile inFile

  case contentsOrErr of
    Right contents ->
      case RegistryDat.fromLazyByteString elmVersion contents of
        Right registryDat ->
          return $ Right registryDat

        Left err ->
          return $ Left $ DecodeError inFile err

    Left err ->
      return $ Left err


readFile :: FilePath -> IO (Either Error LBS.ByteString)
readFile path =
  tryJust handleException (LBS.readFile path)
  where
    handleException e =
      if IOError.isDoesNotExistError e then
        Just $ FileNotFound path

      else
        Nothing
