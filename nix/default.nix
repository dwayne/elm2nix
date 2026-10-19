{ elmPackages
, haskell
, lib
, newScope
}:

lib.makeScope newScope (self: {
  elmHome = ".elm";
  elm = elmPackages.elm;
  elmVersion = self.elm.version;

  elm2nix = self.haskellPackages.elm2nix;
  haskellPackages = self.baseHaskellPackages.extend self.haskellPackagesExtension;
  baseHaskellPackages = haskell.packages.ghc9103;
  haskellPackagesExtension = self.callPackage ./haskell-packages-extension.nix {};

  buildElmApplication = lib.makeOverridable (self.callPackage ./build-elm-application.nix {});
  generateRegistryDat = self.callPackage ./generate-registry-dat.nix {};
  prepareElmHomeScript = self.callPackage ./prepare-elm-home-script.nix {};
  installPatchesScript = self.callPackage ./install-patches-script.nix {};
  installPatchScript = self.callPackage ./install-patch-script.nix {};
  mkPatch = self.callPackage ./mk-patch.nix {};
  dotElmLinks = self.callPackage ./dot-elm-links.nix {};
  symbolicLinksToPackagesScript = self.callPackage ./symbolic-links-to-packages-script.nix {};
  fetchElmPackage = self.callPackage ./fetch-elm-package.nix {};
})
