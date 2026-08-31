{ callPackage
, lib
, elmPackages

, elmHome ? ".elm"
, elmVersion ? elmPackages.elm.version
}:

let
  elm2nix = callPackage ./elm2nix.nix {};
in
rec {
  buildElmApplication = lib.makeOverridable (callPackage ./build-elm-application.nix { inherit generateRegistryDat installPatchesScript prepareElmHomeScript; });
  generateRegistryDat = callPackage ./generate-registry-dat.nix { inherit elm2nix; };
  prepareElmHomeScript = callPackage ./prepare-elm-home-script.nix { inherit dotElmLinks elmHome; };
  installPatchesScript = callPackage ./install-patches-script.nix { inherit installPatchScript mkPatch; };
  installPatchScript = callPackage ./install-patch-script.nix { inherit elmHome elmVersion; };
  mkPatch = callPackage ./mk-patch.nix {};
  dotElmLinks = callPackage ./dot-elm-links.nix { inherit elmVersion symbolicLinksToPackagesScript; };
  symbolicLinksToPackagesScript = callPackage ./symbolic-links-to-packages-script.nix { inherit fetchElmPackage; };
  fetchElmPackage = callPackage ./fetch-elm-package.nix {};
}
