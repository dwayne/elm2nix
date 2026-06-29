{
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        elm2nix = pkgs.callPackage ./nix/elm2nix.nix {};

        mkApp = { drv, description }: {
          type = "app";
          program = "${drv}";
          meta.description = description;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          name = "elm2nix";

          packages = [
            pkgs.ghc
            pkgs.hlint
            pkgs.cabal-install
            pkgs.cabal2nix
            pkgs.elmPackages.elm
            pkgs.actionlint
          ];

          shellHook = ''
            export PS1="($name)\n$PS1"
            export PROJECT_ROOT="$PWD"
            export HSPEC_SKIP="(skip:network)"

            b () {
              cabal build "$@"
            }

            lint () {
              hlint "$PROJECT_ROOT/src" "$PROJECT_ROOT/tests" "$@"
            }
            alias l=lint

            t () {
              cabal test "$@"
            }

            c () {
              lint
              t
              actionlint
              nix flake check -L
              (cd example && nix flake check -L)
            }

            alias d='cabal haddock'
            alias do='cabal haddock --open'

            echo "Type 'b' to build"
            echo "Type 'l' to lint"
            echo "Type 't' to test"
            echo "Type 'c' to run all checks"
            echo "Type 'd' to generate Haddock docs"
            echo "Type 'do' to generate Haddock docs and open it in a browser"
          '';
        };

        packages = {
          default = elm2nix;
          inherit elm2nix;
        };

        apps = {
          default = self.apps.${system}.elm2nix;
          elm2nix = mkApp {
            drv = elm2nix;
            description = "The elm2nix CLI application";
          };
        };

        checks = {
          inherit elm2nix;
        };
      }
    ) // {
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
    };
}
