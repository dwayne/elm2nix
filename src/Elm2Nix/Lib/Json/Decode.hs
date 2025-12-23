module Elm2Nix.Lib.Json.Decode
    ( Decoder
    , DecodeError(..)
    , succeed, failWith
    , string, literal, keyValuePairs
    , field, optionalAt
    , list
    , Error(..)
    , decodeFile, decodeString, decodeValue
    ) where

import qualified Text.JSON as JSON
import qualified Text.JSON.Types as JSON

import Data.Bifunctor (first)
import Data.List (intercalate)
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


literal :: String -> Decoder ()
literal s =
    string >>= \t ->
        if t == s then
            succeed ()

        else
            failWith $ "not equal to \"" ++ s ++ "\": \"" ++ t ++ "\""


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


optionalAt :: [String] -> Decoder a -> Decoder (Maybe a)
optionalAt path decoder =
    let
        dottedName = intercalate "." path
    in
    Decoder $ \value ->
        case getFields path value of
            Right maybeValue ->
                case maybeValue of
                    Just fieldValue ->
                        case decodeValue decoder fieldValue of
                            Right a ->
                                Right (Just a)

                            Left err ->
                                Left (FieldError dottedName err)

                    Nothing ->
                        Right Nothing

            Left err ->
                Left (FieldError dottedName err)


getFields :: [String] -> JSON.JSValue -> Either DecodeError (Maybe JSON.JSValue)
getFields path value =
    case path of
        [] ->
            Right Nothing

        [name] ->
            case value of
                JSObject o ->
                    Right $ JSON.get_field o name

                _ ->
                    Left (Expected "an OBJECT" value)

        name : restPath ->
            case value of
                JSObject o ->
                    case JSON.get_field o name of
                        Just fieldValue ->
                            getFields restPath fieldValue

                        Nothing ->
                            Right Nothing

                _ ->
                    Left (Expected "an OBJECT" value)


list :: Decoder a -> Decoder [a]
list decoder =
    Decoder $ \value ->
        case value of
            JSArray values ->
                traverse (decodeValue decoder) values

            _ ->
                Left (Expected "an ARRAY" value)


-- RUN



data Error
    = SyntaxError String
    | DecodeError DecodeError
    deriving (Eq, Show)


decodeFile :: Decoder a -> FilePath -> IO (Either Error a)
decodeFile decoder path =
    decodeString decoder <$> readFile path


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
