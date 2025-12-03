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
        pkgs = nixpkgs.legacyPackages.${system};
        mkElmDerivation = (elm2nix.lib.elm2nix pkgs).mkElmDerivation;

        example = mkElmDerivation {
          name = "example";
          src = ./.;
          elmLock = ./elm.lock;
          registryDat = ./registry.dat;
        };

        example2 = mkElmDerivation (finalAttrs: {
          pname = "example";
          version = "1.0.0";
          src = ./.;
          elmLock = ./elm.lock;
          registryDat = ./registry.dat;
          dontConfigure = false;
          preConfigure = builtins.trace finalAttrs ''
            echo ${finalAttrs.version}
          '';
        });
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
          inherit example example2;
          default = example;
          debugExample = example.override {
            enableDebugger = true;
            output = "debug.js";
          };
          checkedExample = example.override (prev: {
            doElmFormat = true;
            #
            # How will I be able to do something like:
            #
            # elmFormatSourceFiles = prev.elmFormatSourceFiles ++ [ "review/src" "tests" ];
            #
            elmFormatSourceFiles = builtins.trace prev [ "review/src" "src" "tests" ];
            doElmTest = true;
            output = "checked.js";
          });
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
