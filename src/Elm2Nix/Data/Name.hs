{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.Name
    ( Name, Author, Package
    , elmBrowser, elmCore, elmHtml, elmJson
    , fromText
    , toText, toString
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


elmBrowser :: Name
elmBrowser =
    Name "elm" "browser"


elmCore :: Name
elmCore =
    Name "elm" "core"


elmHtml :: Name
elmHtml =
    Name "elm" "html"


elmJson :: Name
elmJson =
    Name "elm" "json"


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


toText :: Text -> Name -> Text
toText separator (Name author package) =
    author <> separator <> package


toString :: Text -> Name -> String
toString separator =
    Text.unpack . toText separator
