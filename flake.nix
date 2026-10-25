{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-utils-src = { url = "github:dwayne/nix-utils"; flake = false; };
  };

  outputs = { self, nixpkgs, nix-utils-src }:
    let
      lib = nixpkgs.lib;
      nixUtils = import nix-utils-src lib;
    in
    nixUtils.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };

        elm2nixPackages = pkgs.elm2nixPackages;
        elm2nix = elm2nixPackages.elm2nix;
        elm2nixWithCheck = pkgs.haskell.lib.doCheck elm2nix;
      in
      {
        devShells = {
          default = pkgs.mkShell {
            name = "elm2nix";

            inputsFrom = [ elm2nixWithCheck.env ];

            packages = [
              pkgs.actionlint
              pkgs.cabal2nix
              pkgs.cabal-install
              pkgs.elmPackages.elm
              pkgs.hlint
              pkgs.just
            ];
          };

          ci = pkgs.mkShell {
            name = "elm2nix-ci";

            inputsFrom = [ elm2nixWithCheck.env ];

            packages = [
              pkgs.actionlint
              pkgs.hlint
              pkgs.just
            ];

            env = {
              HSPEC_SKIP = "(skip:network)";
            };
          };
        };

        legacyPackages = elm2nixPackages;

        packages = {
          inherit elm2nix elm2nixWithCheck;
          default = elm2nix;
        };

        apps = {
          default = self.apps.${system}.elm2nix;
          elm2nix = {
            type = "app";
            program = lib.getExe elm2nix;
            meta.description = "The elm2nix CLI application";
          };
        };

        checks = {
          inherit elm2nixWithCheck;
        };
      }
    ) // {
      lib = {
        mkElm2nixPackages = pkgs: pkgs.callPackage ./nix {};

        elmPackagePatches = import ./nix/elm-package-patches.nix;
        elmSafeVirtualDomPatches = import ./nix/elm-safe-virtual-dom-patches.nix;
      };

      overlays.default = final: prev: {
        elmPackages = prev.elmPackages.overrideScope (efinal: eprev: {
          elm = efinal.callPackage ./nix/elm-packages/elm.nix {};
          elm-test = efinal.callPackage ./nix/elm-packages/elm-test.nix {};
        });

        elm2nixPackages = final.callPackage ./nix {};
      };
    };
}
