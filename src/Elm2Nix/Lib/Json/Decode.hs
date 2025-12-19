module Elm2Nix.Lib.Json.Decode
    ( Decoder
    , DecodeError(..)
    , succeed, failWith
    , string, keyValuePairs, field, at, optional
    , Error(..)
    , decodeString, decodeValue
    ) where

import qualified Text.JSON as JSON
import qualified Text.JSON.Types as JSON

import Data.Bifunctor (first)
import Text.JSON (JSValue(..))


newtype Decoder a
    = Decoder (JSValue -> Either DecodeError a)


data DecodeError
    = Failure String
    | Expected String JSValue
    | FieldError String DecodeError
    deriving (Eq, Show)



-- INSTANCES



instance Functor Decoder where
    fmap f decoder =
        Decoder $ \value ->
            fmap f (decodeValue decoder value)


instance Applicative Decoder where
    pure = succeed

    df <*> da =
        Decoder $ \value ->
            case decodeValue df value of
                Right f ->
                    case decodeValue da value of
                        Right a ->
                            Right (f a)

                        Left err ->
                            Left err

                Left err ->
                    Left err


instance Monad Decoder where
    da >>= callback =
        Decoder $ \value ->
            case decodeValue da value of
                Right a ->
                    decodeValue (callback a) value

                Left err ->
                    Left err



-- CONSTRUCT



succeed :: a -> Decoder a
succeed x = Decoder (\_ -> Right x)


failWith :: String -> Decoder a
failWith msg = Decoder (\_ -> Left (Failure msg))


string :: Decoder String
string =
    Decoder $ \value ->
        case value of
            JSString s ->
                Right (JSON.fromJSString s)

            _ ->
                Left (Expected "a STRING" value)


keyValuePairs :: (String -> Either String k) -> Decoder v -> Decoder [(k, v)]
keyValuePairs toKey valueDecoder =
    Decoder $ \value ->
        case value of
            JSObject o ->
                keyValuePairsHelper [] toKey valueDecoder (JSON.fromJSObject o)

            _ ->
                Left (Expected "an OBJECT" value)


keyValuePairsHelper :: [(k, v)] -> (String -> Either String k) -> Decoder v -> [(String, JSValue)] -> Either DecodeError [(k, v)]
keyValuePairsHelper accum toKey valueDecoder assoc =
    case assoc of
        [] ->
            Right (reverse accum)

        ( name, x ) : rest ->
            case toKey name of
                Right key ->
                    case decodeValue valueDecoder x of
                        Right value ->
                            keyValuePairsHelper ((key, value) : accum) toKey valueDecoder rest

                        Left err ->
                            Left (FieldError name err)

                Left err ->
                    Left (FieldError name (Failure err))


field :: String -> Decoder a -> Decoder a
field name fieldDecoder =
    Decoder $ \value ->
        let
            err =
                Expected ("an OBJECT with a field named `" ++ name ++ "`") value
        in
        case value of
            JSObject o ->
                case JSON.get_field o name of
                    Just fieldValue ->
                        first (FieldError name) (decodeValue fieldDecoder fieldValue)

                    Nothing ->
                        Left err

            _ ->
                Left err


at :: [String] -> Decoder a -> Decoder a
at path decoder = foldr field decoder path


optional :: Decoder a -> Decoder (Maybe a)
optional decoder =
    Decoder $ \value ->
        case decodeValue decoder value of
            Right x ->
                Right (Just x)

            Left _ ->
                Right Nothing



-- RUN



data Error
    = SyntaxError String
    | DecodeError DecodeError
    deriving (Eq, Show)


decodeString :: Decoder a -> String -> Either Error a
decodeString decoder input =
    case parseJson input of
        JSON.Ok value ->
            first DecodeError (decodeValue decoder value)

        JSON.Error message ->
            Left (SyntaxError message)


decodeValue :: Decoder a -> JSValue -> Either DecodeError a
decodeValue (Decoder f) = f


parseJson :: String -> JSON.Result JSValue
parseJson = JSON.decode
