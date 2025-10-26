{-# LANGUAGE OverloadedStrings #-}

module Dependency (Author, Dependency(..), Package) where

import Data.Text (Text)
import Version (Version(..))


type Author = Text
type Package = Text


data Dependency
    = Dependency
        { author :: Author
        , package :: Package
        , version :: Version
        }
    deriving (Eq, Ord, Show)
