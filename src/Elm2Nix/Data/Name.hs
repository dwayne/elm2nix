{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.Name
    ( Name, Author, Package
    , elmBrowser, elmCore, elmHtml, elmJson, elmTime, elmUrl, elmVirtualDom
    , FromTextError(..), fromText
    , toAuthor, toPackage
    , toText, toString
    ) where

import qualified Data.Text as Text

import Data.Text (Text)


data Name
    = Name
        { _author :: Author
        , _package :: Package
        }
    deriving (Eq, Ord)


type Author = Text
type Package = Text


instance Show Name where
    show = toString "/"


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


elmTime :: Name
elmTime =
    Name "elm" "time"


elmUrl :: Name
elmUrl =
    Name "elm" "url"


elmVirtualDom :: Name
elmVirtualDom =
    Name "elm" "virtual-dom"


data FromTextError
    = EmptyAuthor
    | EmptyPackage
    | MissingForwardSlash
    deriving (Eq, Show)


fromText :: Text -> Either FromTextError Name
fromText t =
    let
        ( author, slashPackage ) =
            Text.breakOn "/" t
    in
    case Text.uncons slashPackage of
        Just ( '/', package ) ->
            if Text.null author then
                Left EmptyAuthor

            else if Text.null package then
                Left EmptyPackage

            else
                Right $ Name author package

        _ ->
            Left MissingForwardSlash


toAuthor :: Name -> Author
toAuthor (Name author _) = author


toPackage :: Name -> Package
toPackage (Name _ package) = package


toText :: Text -> Name -> Text
toText separator (Name author package) =
    author <> separator <> package


toString :: Text -> Name -> String
toString separator =
    Text.unpack . toText separator
