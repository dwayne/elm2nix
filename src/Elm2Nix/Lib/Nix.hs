module Elm2Nix.Lib.Nix
    ( NixPrefetchUrlError(..)
    , NixPrefetchUrlOutput(..)
    , Sha256
    , Url
    , nixPrefetchUrl
    ) where

import qualified Data.ByteString.Lazy.Char8 as Char8

import System.Process.Typed (ExitCode(ExitSuccess), ProcessConfig, readProcess, proc)


data NixPrefetchUrlOutput
    = NixPrefetchUrlOutput
        { hash :: Sha256
        , path :: FilePath
        }
    deriving (Eq, Show)


type Sha256 = String
type Url = String


data NixPrefetchUrlError
    = ProcessError String
    | BadOutput String
    deriving (Eq, Show)


nixPrefetchUrl :: Url -> String -> IO (Either NixPrefetchUrlError NixPrefetchUrlOutput)
nixPrefetchUrl url name = do
    ( code, stdout, stderr ) <- readProcess processConfig
    if code == ExitSuccess then
        case Char8.lines stdout of
            [ hash, path ] ->
                return $ Right $ NixPrefetchUrlOutput (Char8.unpack hash) (Char8.unpack path)

            _ ->
                return $ Left $ BadOutput $ Char8.unpack stdout
    else
        return $ Left $ ProcessError $ Char8.unpack stderr

    where
        processConfig :: ProcessConfig () () ()
        processConfig =
            proc "nix-prefetch-url"
                [ url
                , "--type", "sha256"
                , "--print-path"
                , "--unpack"
                , "--name", name
                ]
