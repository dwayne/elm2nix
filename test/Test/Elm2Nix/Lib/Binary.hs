module Test.Elm2Nix.Lib.Binary (main) where

import Test.Hspec

import qualified Elm2Nix.Data.Name as Name
import qualified Elm2Nix.Data.RegistryDat as RegistryDat
import qualified Elm2Nix.Lib.Binary as Binary
import qualified Test.Fixture as Fixture

import Elm2Nix.Data.Dependency (Dependency(..))
import Elm2Nix.Data.RegistryDat (RegistryDat)
import Elm2Nix.Data.Version (Version(..))


main :: IO ()
main = hspec $
    describe "Elm2Nix.Lib.Binary" decodeFileSpec


decodeFileSpec :: Spec
decodeFileSpec =
    describe "decodeFile" $ do
        describe "valid input" $
            it "example 1" $
                let
                    registryDat =
                        RegistryDat.fromList
                            [ Dependency Name.elmBrowser (Version 1 0 2)
                            , Dependency Name.elmCore (Version 1 0 5)
                            , Dependency Name.elmHtml (Version 1 0 0)
                            , Dependency Name.elmJson (Version 1 1 3)
                            , Dependency Name.elmTime (Version 1 0 0)
                            , Dependency Name.elmUrl (Version 1 0 0)
                            , Dependency Name.elmVirtualDom (Version 1 0 3)
                            ]
                in
                (decodeFile =<< Fixture.file "registry.dat") `shouldReturn` Right registryDat

        describe "invalid input" $ do
            it "when the file does not exist" $
                decodeFile "path/to/missing/registry.dat" `shouldReturn` Left (Binary.FileNotFound "path/to/missing/registry.dat")

            it "when the file is corrupted" $ do
                path <- Fixture.file "corrupted.dat"
                Left (Binary.DecodeError _ details) <- decodeFile path
                details `shouldBe` "not enough bytes"


decodeFile :: FilePath -> IO (Either Binary.DecodeFileError RegistryDat)
decodeFile =
    Binary.decodeFile
