{ callPackage
, lib

, elmVersion ? "0.19.1"
}:

let
  elm2nix = callPackage ./elm2nix.nix {};
in
rec {
  buildElmApplication = lib.makeOverridable (callPackage ./build-elm-application.nix { inherit generateRegistryDat prepareElmHomeScript; });
  generateRegistryDat = callPackage ./generate-registry-dat.nix { inherit elm2nix; };
  prepareElmHomeScript = callPackage ./prepare-elm-home-script.nix { inherit dotElmLinks; };
  dotElmLinks = callPackage ./dot-elm-links.nix { inherit elmVersion symbolicLinksToPackagesScript; };
  symbolicLinksToPackagesScript = callPackage ./symbolic-links-to-packages-script.nix { inherit fetchElmPackage; };
  fetchElmPackage = callPackage ./fetch-elm-package.nix {};
}
