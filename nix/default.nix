{ callPackage
, lib

, elmVersion ? "0.19.1"
}:

rec {
  buildElmApplication = lib.makeOverridable (callPackage ./build-elm-application.nix { inherit prepareElmHomeScript; });
  prepareElmHomeScript = callPackage ./prepare-elm-home-script.nix { inherit dotElmLinks; };
  dotElmLinks = callPackage ./dot-elm-links.nix { inherit elmVersion symbolicLinksToPackagesScript; };
  symbolicLinksToPackagesScript = callPackage ./symbolic-links-to-packages-script.nix { inherit fetchElmPackage; };
  fetchElmPackage = callPackage ./fetch-elm-package.nix {};
}
