module Main (main) where

import qualified Control.Exception as E
import qualified Data.Text.IO as TIO
import qualified Elm2Nix.Cli.Arguments as Arguments
import qualified Elm2Nix.Cli.Error as Error
import qualified Elm2Nix.Command.ViewRegistryDatFile as ViewRegistryDatFile
import qualified Elm2Nix.Command.WriteElmLockFile as WriteElmLockFile
import qualified Elm2Nix.Command.WriteRegistryDatFile as WriteRegistryDatFile

import Data.Text (Text)
import System.Exit (ExitCode, exitFailure)
import System.IO (hPutStrLn, stderr)


main :: IO ()
main = runSafely unsafeMain


unsafeMain :: IO ()
unsafeMain = do
  args <- Arguments.parseIO

  case args of
    Arguments.Lock (Arguments.LockOptions inFiles compact outFiles) ->
      WriteElmLockFile.run inFiles compact outFiles
        >>= either (die . Error.toText . Error.WriteElmLockFileError) return

    Arguments.Registry (Arguments.Generate (Arguments.GenerateOptions elmVersion inFile outFile)) ->
      WriteRegistryDatFile.run elmVersion inFile outFile
        >>= either (die . Error.toText . Error.WriteRegistryDatFileError) return

    Arguments.Registry (Arguments.View (Arguments.ViewOptions elmVersion compact inFile)) ->
      ViewRegistryDatFile.run elmVersion compact inFile
        >>= either (die . Error.toText . Error.ViewRegistryDatFileError) return


die :: Text -> IO ()
die t = TIO.hPutStrLn stderr t >> exitFailure


runSafely :: IO () -> IO ()
runSafely action =
  action `E.catches`
    [ E.Handler rethrowAsync
    , E.Handler rethrowExitCode
    , E.Handler handleUnexpected
    ]
  where
    rethrowAsync :: E.AsyncException -> IO ()
    rethrowAsync = E.throwIO

    rethrowExitCode :: ExitCode -> IO ()
    rethrowExitCode = E.throwIO

    handleUnexpected :: E.SomeException -> IO ()
    handleUnexpected e =
      hPutStrLn stderr ("An unexpected error occurred: " ++ E.displayException e) >> exitFailure
