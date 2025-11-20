module Test.Elm2Nix.Lib.Json (main) where

import Test.Hspec

import qualified Elm2Nix.Data.ElmJson as ElmJson
import qualified Elm2Nix.Data.Name as Name
import qualified Elm2Nix.Lib.Json as Json
import qualified Test.Fixture as Fixture

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.ElmJson (ElmJson)
import Elm2Nix.Data.Version (Version(..))


main :: IO ()
main = hspec $
    describe "Elm2Nix.Lib.Json" decodeFileSpec


decodeFileSpec :: Spec
decodeFileSpec =
    describe "decodeFile (skip)" $ do
        describe "valid input" $
            it "example 1" $
                let
                    elmJson =
                        ElmJson.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            , Dependency Name.elmCore (Version 1 0 5)
                            , Dependency Name.elmHtml (Version 1 0 0)
                            , Dependency Name.elmJson (Version 1 1 3)
                            , Dependency Name.elmTime (Version 1 0 0)
                            , Dependency Name.elmUrl (Version 1 0 0)
                            , Dependency Name.elmVirtualDom (Version 1 0 3)
                            ]
                in
                (decodeFile =<< Fixture.file "elm.json") `shouldReturn` Right elmJson

        describe "invalid input" $ do
            it "when the file does not exist" $
                decodeFile "path/to/missing/elm.json" `shouldReturn` Left (Json.FileNotFound "path/to/missing/elm.json")

            it "when / is missing" $ do
                path <- Fixture.file "name-missing-forward-slash.json"
                Left (Json.SyntaxError _ details) <- decodeFile path
                details `shouldBe` "Error in $.dependencies.direct.elmbrowser: / is missing"

            it "when version is incorrectly formatted" $ do
                path <- Fixture.file "version-incorrect-format.json"
                Left (Json.SyntaxError _ details) <- decodeFile path
                details `shouldBe` "Error in $.dependencies.direct['elm/browser']: version is invalid: \"1.0\""

            it "when version has a part with leading zeros" $ do
                path <- Fixture.file "version-leading-zeros.json"
                Left (Json.SyntaxError _ details) <- decodeFile path
                details `shouldBe` "Error in $.dependencies.direct['elm/browser']: version is invalid: \"1.0.02\""


decodeFile :: FilePath -> IO (Either Json.DecodeFileError ElmJson)
decodeFile =
    Json.decodeFile
