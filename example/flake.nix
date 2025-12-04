{
  inputs = {
    elm2nix = {
      url = "./..";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, elm2nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (final: prev: {
              elmPackages = prev.elmPackages.overrideScope (sFinal: sPrev: {
                elm-review = sPrev.elm-review.overrideAttrs (old: {
                  patches = (old.patches or []) ++ [
                    (prev.fetchpatch {
                      url = "https://github.com/jfmengels/node-elm-review/commit/c766aca85a30b39396e8555c3a21d69f421ce65a.patch";
                      hash = "sha256-lrm9h2RXPUwckgoa0pahUkC+V1mwf4U1hbEN9IRdckE=";
                    })
                  ];
                });
              });
            })
          ];
        };

        elm-review = pkgs.elmPackages.elm-review;

        inherit (elm2nix.lib.elm2nix pkgs) mkElmDerivation;

        example = mkElmDerivation {
          name = "example";
          src = ./.;
          elmLock = ./elm.lock;
          registryDat = ./registry.dat;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          name = "example";

          packages = [
            elm2nix.packages.${system}.default
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-review
            pkgs.elmPackages.elm-test
          ];

          shellHook = ''
            export PS1="($name) $PS1"
          '';
        };

        packages = rec {
          inherit example elm-review;
          default = example;
          debugExample = example.override {
            enableDebugger = true;
            output = "debug.js";
          };
          checkedExample = example.override {
            doElmFormat = true;
            elmFormatSourceFiles = [ "review/src" "src" "tests" ];
            doElmTest = true;
            output = "checked.js";
          };
          reviewedExample = checkedExample.override {
            doElmReview = true;
            elmReviewElmLock = ./review/elm.lock;
            elmReviewRegistryDat = ./review/registry.dat;
            output = "reviewed.js";
          };
          optimizedExample = checkedExample.override {
            output = "optimized.js";
            enableOptimizations = true;
            optimizeLevel = 2;
            doMinification = true;
            doCompression = true;
            doReporting = true;
          };
          combinedExample1 = checkedExample.override {
            entry = [ "src/Main.elm" "src/Workshop.elm" ];
            output = "combined1.js";
          };
          combinedExample2 = optimizedExample.override {
            entry = [ "src/Main.elm" "src/Workshop.elm" ];
            output = "combined2.js";
          };
          hashedExample = optimizedExample.override {
            output = "hashed.js";
            doContentHashing = true;
            hashLength = 12;
            keepFilesWithNoHashInFilenames = true;
          };
        };
      }
    );
}
