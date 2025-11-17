module Elm2Nix.CLI
    ( CLI(..)
    , LockOptions(..)
    , RegistryCommands(..), GenerateOptions(..), ViewOptions(..)
    , run, runIO
    ) where

import Options.Applicative


data CLI
    = Lock LockOptions
    | Registry RegistryCommands
    deriving (Eq, Show)


data LockOptions
    = LockOptions
        { loCompact :: Bool
        , loInput :: FilePath
        , loOutput :: FilePath
        }
    deriving (Eq, Show)


data RegistryCommands
    = Generate GenerateOptions
    | View ViewOptions
    deriving (Eq, Show)


data GenerateOptions
    = GenerateOptions
        { goInput :: FilePath
        , goOutput :: FilePath
        }
    deriving (Eq, Show)


data ViewOptions
    = ViewOptions
        { voInput :: FilePath
        }
    deriving (Eq, Show)


runIO :: IO CLI
runIO =
    customExecParser preferences cli


run :: [String] -> ParserResult CLI
run =
    execParserPure preferences cli


preferences :: ParserPrefs
preferences =
    prefs $ mconcat
        [ showHelpOnEmpty
        , noBacktrack
        ]


cli :: ParserInfo CLI
cli =
    info (commands <**> helper) $ mconcat
        [ header "Create Elm artifacts to be used when compiling Elm applications with Nix"
        ]


commands :: Parser CLI
commands =
    hsubparser $ mconcat
        [ command "lock" (info (Lock <$> lockOptions) (progDesc "Generate a lock file from your elm.json"))
        , command "registry" (info (Registry <$> registryCommands) (progDesc "Generate or view a registry.dat file"))
        ]


registryCommands :: Parser RegistryCommands
registryCommands =
    hsubparser $ mconcat
        [ command "generate" (info (Generate <$> generateOptions) (progDesc "Generate a registry.dat file from your elm.json"))
        , command "view" (info (View <$> viewOptions) (progDesc "Display a registry.dat file in a human-readable format"))
        ]


lockOptions :: Parser LockOptions
lockOptions =
    LockOptions <$> isCompactOption <*> elmJsonInputOption <*> elmLockOutputOption


generateOptions :: Parser GenerateOptions
generateOptions =
    GenerateOptions <$> elmJsonInputOption <*> registryOutputOption


viewOptions :: Parser ViewOptions
viewOptions =
    ViewOptions <$> registryInputOption


isCompactOption :: Parser Bool
isCompactOption =
    switch $ mconcat
        [ long "compact"
        , showDefault
        , help "Format the lock file as compactly as possible"
        ]


elmJsonInputOption :: Parser FilePath
elmJsonInputOption =
    strOption $ mconcat
        [ long "input"
        , short 'i'
        , value "elm.json"
        , showDefault
        , metavar "FILE"
        , help "The path to the elm.json file"
        ]


elmLockOutputOption :: Parser FilePath
elmLockOutputOption =
    strOption $ mconcat
        [ long "output"
        , short 'o'
        , value "elm.lock"
        , showDefault
        , metavar "FILE"
        , help "The path to the lock file"
        ]


registryInputOption :: Parser FilePath
registryInputOption =
    strOption $ mconcat
        [ long "input"
        , short 'i'
        , value "registry.dat"
        , showDefault
        , metavar "FILE"
        , help "The path to the registry.dat file"
        ]


registryOutputOption :: Parser FilePath
registryOutputOption =
    strOption $ mconcat
        [ long "output"
        , short 'o'
        , value "registry.dat"
        , showDefault
        , metavar "FILE"
        , help "The path to the registry.dat file"
        ]
