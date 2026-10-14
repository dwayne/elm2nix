{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      lib = nixpkgs.lib;
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = f:
        lib.genAttrs supportedSystems (system:
          let
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ self.overlays.default ];
            };
          in
          f system pkgs
        );
    in
    {
      devShells = forAllSystems (system: pkgs:
        {
          default = pkgs.mkShell {
            name = "elm2nix";

            inputsFrom = [
              self.packages.${system}.elm2nix.env
            ];

            packages = [
              pkgs.actionlint
              pkgs.cabal2nix
              pkgs.cabal-install
              pkgs.elmPackages.elm
              pkgs.hlint
              pkgs.just
            ];

            env = {
              HSPEC_SKIP = "(skip:network)";
            };
          };
        }
      );

      packages = forAllSystems (system: pkgs:
        {
          default = self.packages.${system}.elm2nix;
          elm2nix = pkgs.callPackage ./nix/elm2nix.nix {};
        }
      );

      apps = forAllSystems (system: pkgs:
        {
          default = self.apps.${system}.elm2nix;
          elm2nix = {
            type = "app";
            program = lib.getExe self.packages.${system}.elm2nix;
            meta.description = "The elm2nix CLI application";
          };
        }
      );

      checks = forAllSystems (system: _:
        {
          elm2nix = self.packages.${system}.elm2nix;
        }
      );

      lib =
        rec {
          elm2nix = pkgs: pkgs.callPackage ./nix {};

          patches = {
            #
            # N.B. The values of all the keys in this attribute set
            # corresponds to the arguments that can be passed to mkPatch.
            #
            # As a result, these values can also be passed directly to
            # the elmPackagePatches option of the buildElmApplication
            # function.
            #

            lydellBrowser = {
              fromOwner = "lydell";
              toOwner = "elm";
              repo = "browser";
              version = "1.0.2";
              rev = "f5de544c8033d934285501f78f09e2eaf0171d55";
              hash = "sha256-29axLnzXcLDeKG+CBX49pjt2ZcYVdVg04XVnfAfImvI=";
            };

            lydellHtml = {
              fromOwner = "lydell";
              toOwner = "elm";
              repo = "html";
              version = "1.0.1";
              rev = "b35c476a69f0ba9bf8282d8c15df65e63aefea8f";
              hash = "sha256-xyL/AvKdsxTl4RgfBCdTuWndM55eNM6whPD3YqptcKM=";
            };

            lydellVirtualDom = {
              fromOwner = "lydell";
              toOwner = "elm";
              repo = "virtual-dom";
              version = "1.0.5";
              rev = "e1fae6aabd65539db2c94a98220a45cfc624b633";
              hash = "sha256-XpbRMCpIx151eHHoph7wkGYhtDp5bTBwUOefiWKItOc=";
            };

            omnibsElmCss = {
              fromOwner = "omnibs";
              toOwner = "rtfeldman";
              repo = "elm-css";
              version = "18.0.0";
              rev = "e54998ce73b64c374b1457d5734c85d3f5b909fb";
              hash = "sha256-rmil+7lAKUm7Fm0MCba23xyCA0CWrDb1ej5gPeXS2oU=";
            };
          };

          #
          # A convenient collection of patches based on
          # lydell/elm-safe-virtual-dom.
          #
          elmSafeVirtualDom = rec {
            default = elmHtml;

            elmHtml = [
              patches.lydellBrowser
              patches.lydellHtml
              patches.lydellVirtualDom
            ];

            elmCss = [
              patches.lydellBrowser
              patches.omnibsElmCss
              patches.lydellVirtualDom
            ];
          };
        };

      overlays.default = final: prev: {
        elmPackages = prev.elmPackages.overrideScope (efinal: eprev: {
          elm = efinal.callPackage ./nix/elm-packages/elm.nix {};
          elm-test = efinal.callPackage ./nix/elm-packages/elm-test.nix {};
        });
      };
    };
}
