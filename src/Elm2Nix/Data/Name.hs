{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.Name
    ( Name, Author, Package
    , elmCore
    , fromText
    , toString
    ) where

import qualified Data.Text as Text

import Data.Text (Text)


data Name
    = Name
        { author :: Author
        , package :: Package
        }
    deriving (Eq, Ord)


instance Show Name where
    show = toString "/"


type Author = Text
type Package = Text


elmCore :: Name
elmCore =
    Name "elm" "core"


fromText :: Text -> Either String Name
fromText t =
    let
        ( author, slashPackage ) =
            Text.breakOn "/" t
    in
    case Text.uncons slashPackage of
        Just ( '/', package ) ->
            if Text.null author then
                Left "author is empty"

            else if Text.null package then
                Left "package is empty"

            else
                Right $ Name author package

        _ ->
            Left "/ is missing"


toString :: Text -> Name -> String
toString separator (Name author package) =
    Text.unpack $ author <> separator <> package
