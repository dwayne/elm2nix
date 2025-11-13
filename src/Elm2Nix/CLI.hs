module Elm2Nix.CLI (CLI(..), LockOptions(..), run, runIO) where

import Options.Applicative


data CLI
    = Lock LockOptions
    deriving (Eq, Show)


data LockOptions
    = LockOptions
        { loCompact :: Bool
        , loInput :: FilePath
        , loOutput :: FilePath
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
        ]


lockOptions :: Parser LockOptions
lockOptions =
    LockOptions <$> isCompactOption <*> elmJsonInputOption <*> elmLockOutputOption


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
