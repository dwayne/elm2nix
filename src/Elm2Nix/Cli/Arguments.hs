module Elm2Nix.Cli.Arguments
  ( Arguments(..)
  , LockOptions(..)
  , RegistryCommands(..), GenerateOptions(..), ViewOptions(..)
  , parseIO, parse
  ) where

import Elm2Nix.Data.ElmVersion (ElmVersion(..))
import Options.Applicative


data Arguments
  = Lock LockOptions
  | Registry RegistryCommands
  deriving (Eq, Show)


data LockOptions
  = LockOptions
    { loInFiles :: [FilePath]
    , loCompact :: Bool
    , loOutFile :: FilePath
    }
  deriving (Eq, Show)


data RegistryCommands
  = Generate GenerateOptions
  | View ViewOptions
  deriving (Eq, Show)


data GenerateOptions
  = GenerateOptions
    { goElmVersion :: ElmVersion
    , goInFile :: FilePath
    , goOutFile :: FilePath
    }
  deriving (Eq, Show)


data ViewOptions
  = ViewOptions
    { voElmVersion :: ElmVersion
    , voCompact :: Bool
    , voInput :: FilePath
    }
  deriving (Eq, Show)


parseIO :: IO Arguments
parseIO =
  customExecParser preferences arguments


parse :: [String] -> ParserResult Arguments
parse =
  execParserPure preferences arguments


preferences :: ParserPrefs
preferences =
  prefs $ mconcat
    [ showHelpOnEmpty
    , noBacktrack
    ]


arguments :: ParserInfo Arguments
arguments =
  info (commands <**> helper) $ mconcat
    [ header "Create Elm support files to be used when compiling Elm applications with Nix"
    ]


commands :: Parser Arguments
commands =
  hsubparser $ mconcat
    [ command "lock" (info (Lock <$> lockOptions) (progDesc "Generate a lock file from one or more elm.json files"))
    , command "registry" (info (Registry <$> registryCommands) (progDesc "Generate or view a registry.dat file"))
    ]


registryCommands :: Parser RegistryCommands
registryCommands =
  hsubparser $ mconcat
    [ command "generate" (info (Generate <$> generateOptions) (progDesc "Generate a registry.dat file from a lock file"))
    , command "view" (info (View <$> viewOptions) (progDesc "View a registry.dat file as JSON"))
    ]


lockOptions :: Parser LockOptions
lockOptions =
  LockOptions <$> elmJsonInFilesArgument <*> isCompactOption <*> elmLockOutFileOption


generateOptions :: Parser GenerateOptions
generateOptions =
  GenerateOptions <$> elmVersionOption <*> elmLockInFileOption <*> registryOutFileOption


viewOptions :: Parser ViewOptions
viewOptions =
  ViewOptions <$> elmVersionOption <*> isCompactOption <*> registryInFileOption


elmJsonInFilesArgument :: Parser [FilePath]
elmJsonInFilesArgument =
  withDefault <$> many
    (strArgument $ mconcat
      [ metavar "FILE..."
      , help "The paths to the elm.json files (default: \"elm.json\")"
      ]
    )
  where
    withDefault [] = [ "elm.json" ]
    withDefault fs = fs


isCompactOption :: Parser Bool
isCompactOption =
  switch $ mconcat
    [ long "compact"
    , showDefault
    , help "Format the JSON as compactly as possible"
    ]


elmLockInFileOption :: Parser FilePath
elmLockInFileOption =
  strOption $ mconcat
    [ long "input"
    , short 'i'
    , value "elm.lock"
    , showDefault
    , metavar "FILE"
    , help "The path to the lock file"
    ]


elmLockOutFileOption :: Parser FilePath
elmLockOutFileOption =
  strOption $ mconcat
    [ long "output"
    , short 'o'
    , value "elm.lock"
    , showDefault
    , metavar "FILE"
    , help "The path to the lock file"
    ]


registryInFileOption :: Parser FilePath
registryInFileOption =
  strOption $ mconcat
    [ long "input"
    , short 'i'
    , value "registry.dat"
    , showDefault
    , metavar "FILE"
    , help "The path to the registry.dat file"
    ]


registryOutFileOption :: Parser FilePath
registryOutFileOption =
  strOption $ mconcat
    [ long "output"
    , short 'o'
    , value "registry.dat"
    , showDefault
    , metavar "FILE"
    , help "The path to the registry.dat file"
    ]


elmVersionOption :: Parser ElmVersion
elmVersionOption =
  option readElmVersion $ mconcat
    [ long "elm-version"
    , value V0_19_3
    , showDefault
    , metavar "ELM_VERSION"
    , help "The version, either 0.19.1 or 0.19.3, of the registry.dat file"
    ]


readElmVersion :: ReadM ElmVersion
readElmVersion =
  eitherReader
    (\s ->
      case s of
        "0.19.1" ->
          Right V0_19_1

        "0.19.3" ->
          Right V0_19_3

        _ ->
          Left ("must be either 0.19.1 or 0.19.3: " ++ s)
    )
