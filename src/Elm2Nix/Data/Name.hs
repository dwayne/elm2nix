{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.Name
    ( Name, Author, Package
    , elmBrowser, elmCore, elmHtml, elmJson, elmTime, elmUrl, elmVirtualDom
    , FromTextError(..), fromText
    , toAuthor, toPackage
    , toText, toString
    ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Data.Binary (Binary(..), Get, Put, getWord8, putWord8)
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import Data.Text (Text)


data Name
    = Name
        { _author :: Author
        , _package :: Package
        }
    deriving (Eq, Ord)


type Author = Text
type Package = Text



-- INSTANCES



instance Show Name where
    show = toString "/"


instance Binary Name where
    put (Name author project) = putText author <> putText project
    get = Name <$> getText <*> getText


putText :: Text -> Put
putText t =
    putWord8 (fromIntegral $ BS.length bs) <> putByteString bs
    where
        bs = TE.encodeUtf8 t


getText :: Get Text
getText =
    getWord8 >>= fmap TE.decodeUtf8 . getByteString . fromIntegral



-- CONSTRUCT



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
            T.breakOn "/" t
    in
    case T.uncons slashPackage of
        Just ( '/', package ) ->
            if T.null author then
                Left EmptyAuthor

            else if T.null package then
                Left EmptyPackage

            else
                Right $ Name author package

        _ ->
            Left MissingForwardSlash



-- CONVERT



toAuthor :: Name -> Author
toAuthor (Name author _) = author


toPackage :: Name -> Package
toPackage (Name _ package) = package


toText :: Text -> Name -> Text
toText separator (Name author package) =
    author <> separator <> package


toString :: Text -> Name -> String
toString separator =
    T.unpack . toText separator
