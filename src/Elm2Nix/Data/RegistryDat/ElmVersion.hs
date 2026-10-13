module Elm2Nix.Data.RegistryDat.ElmVersion (ElmVersion(..), fromString) where


data ElmVersion
  = V0_19_1
  | V0_19_3
  deriving (Eq)


instance Show ElmVersion where
  show V0_19_1 = "0.19.1"
  show V0_19_3 = "0.19.3"


fromString :: String -> Maybe ElmVersion
fromString s =
  case s of
    "0.19.1" ->
      Just V0_19_1

    "0.19.3" ->
      Just V0_19_3

    _ ->
      Nothing
