module Version (Version(..)) where

import Numeric.Natural (Natural)


data Version
    = Version
        { major :: Natural
        , minor :: Natural
        , patch :: Natural
        }
    deriving (Eq, Ord)


instance Show Version where
    show (Version major minor patch) =
        show major ++ "." ++ show minor ++ "." ++ show patch
