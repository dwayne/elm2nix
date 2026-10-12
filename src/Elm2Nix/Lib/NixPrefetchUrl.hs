module Elm2Nix.Lib.NixPrefetchUrl (Url, Sha256, Output(..), Error(..), run) where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified System.Process.Typed as Process


type Url = String
type Sha256 = String


data Output
  = Output
      { hash :: Sha256
      , path :: FilePath
      }
  deriving (Eq, Show)


data Error
  = ProcessError String
  | BadOutput String
  deriving (Eq, Show)


run :: Url -> String -> IO (Either Error Output)
run url name = do
  ( code, stdout, stderr ) <- Process.readProcess processConfig

  if code == Process.ExitSuccess then
    case Char8.lines stdout of
      [ hash, path ] ->
        return $ Right $ Output (Char8.unpack hash) (Char8.unpack path)

      _ ->
        return $ Left $ BadOutput $ Char8.unpack stdout
  else
    return $ Left $ ProcessError $ Char8.unpack stderr

  where
    processConfig :: Process.ProcessConfig () () ()
    processConfig =
      Process.proc "nix-prefetch-url"
        [ url
        , "--type", "sha256"
        , "--print-path"
        , "--unpack"
        , "--name", name
        ]
