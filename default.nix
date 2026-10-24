{
  lib = {
    mkLib = pkgs: pkgs.callPackage ./nix {};

    elmPackagePatches = import ./nix/elm-package-patches.nix;
    elmSafeVirtualDomPatches = import ./nix/elm-safe-virtual-dom-patches.nix;
  };

  overlays.default = final: prev: {
    elmPackages = prev.elmPackages.overrideScope (efinal: eprev: {
      elm = efinal.callPackage ./nix/elm-packages/elm.nix {};
      elm-test = efinal.callPackage ./nix/elm-packages/elm-test.nix {};
    });
  };
}
