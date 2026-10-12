module Elm2Nix.Command.WriteElmLockFile (Error(..), run) where

import qualified Elm2Nix.Data.ElmJson as ElmJson
import qualified Elm2Nix.Data.FixedOutputDerivation as FOD
import qualified Json
import qualified Json.Decode as JD
import qualified Json.Encode as JE


data Error
  = FromFilesError FilePath JD.Error
  | FromDependenciesError FOD.FromDependenciesError
  deriving (Eq, Show)


run :: [FilePath] -> Bool -> FilePath -> IO (Either Error ())
run inFiles compact outFile = do
  elmJsonOrErr <- ElmJson.fromFiles inFiles

  case elmJsonOrErr of
    Right elmJson -> do
      fodsOrErr <- FOD.fromElmJson elmJson

      case fodsOrErr of
        Right fods ->
          fmap Right $
            if compact then
              encodeCompact outFile fods

            else
              encodeExpanded outFile fods

        Left err ->
          return $ Left $ FromDependenciesError err

    Left ( path, err ) ->
      return $ Left $ FromFilesError path err


encodeCompact :: FilePath -> [FOD.FixedOutputDerivation] -> IO ()
encodeCompact outFile = Json.writeCompact outFile . JE.encode


encodeExpanded :: FilePath -> [FOD.FixedOutputDerivation] -> IO ()
encodeExpanded outFile = Json.writePretty outFile 4 . JE.encode
