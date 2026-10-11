{-# LANGUAGE OverloadedStrings #-}

module Elm2Nix.Data.Name
  ( Name, Author, Package
  , elmBrowser, elmCore, elmHtml, elmJson, elmTime, elmUrl, elmVirtualDom
  , FromTextError(..), fromText
  , toAuthor, toPackage
  , toText, toString
  , fromTextErrorToString
  , binaryEncoderV0_19_1, binaryDecoderV0_19_1
  , binaryEncoderV0_19_3, binaryDecoderV0_19_3
  ) where

import qualified Data.Text as T
import qualified Elm2Nix.Data.Bytes.Decode as BD
import qualified Elm2Nix.Data.Bytes.DecodeV0_19_1 as DV0_19_1
import qualified Elm2Nix.Data.Bytes.DecodeV0_19_3 as DV0_19_3
import qualified Elm2Nix.Data.Bytes.Encode as BE
import qualified Elm2Nix.Data.Bytes.EncodeV0_19_1 as EV0_19_1
import qualified Elm2Nix.Data.Bytes.EncodeV0_19_3 as EV0_19_3

import Data.Text (Text)


data Name
  = Name
      { _author :: Author
      , _package :: Package
      }
  deriving (Eq, Ord)


type Author = Text
type Package = Text



-- Instances



instance Show Name where
  show = toString "/"



-- Construct



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
      if isBlank author then
        Left EmptyAuthor

      else if isBlank package then
        Left EmptyPackage

      else
        Right $ Name author package

    _ ->
      Left MissingForwardSlash


isBlank :: Text -> Bool
isBlank =
  T.null . T.strip



-- Convert



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


fromTextErrorToString :: FromTextError -> String
fromTextErrorToString EmptyAuthor         = "author is empty"
fromTextErrorToString EmptyPackage        = "package is empty"
fromTextErrorToString MissingForwardSlash = "/ is missing"



-- Binary Encoder/Decoder for Elm 0.19.1



binaryEncoderV0_19_1 :: Name -> BE.Encoder
binaryEncoderV0_19_1 (Name author project) =
  EV0_19_1.text author <> EV0_19_1.text project


binaryDecoderV0_19_1 :: BD.Decoder Name
binaryDecoderV0_19_1 =
  Name <$> DV0_19_1.text <*> DV0_19_1.text



-- Binary Encoder/Decoder for Elm 0.19.3



binaryEncoderV0_19_3 :: Name -> BE.Encoder
binaryEncoderV0_19_3 (Name author project) =
  EV0_19_3.string8 author <> EV0_19_3.string8 project


binaryDecoderV0_19_3 :: BD.Decoder Name
binaryDecoderV0_19_3 =
  Name <$> DV0_19_3.string8 <*> DV0_19_3.string8
