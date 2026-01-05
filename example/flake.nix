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
        inherit (elm2nix.lib.elm2nix pkgs)
          buildElmApplication
          generateRegistryDat
          prepareElmHomeScript
          dotElmLinks
          symbolicLinksToPackagesScript
          fetchElmPackage
          ;

        elmLock = ./elm.lock;
        registryDat = generateRegistryDat {
          inherit elmLock;
        };

        exampleFetchElmPackage = fetchElmPackage {
          author = "elm";
          package = "browser";
          version = "1.0.2";
          sha256 = "0863nw2hhbpm3s03lm1imi5x28wwknzrwg2p79s5mydgvdvgwjf0";
        };

        exampleSymbolicLinksToPackagesScript = symbolicLinksToPackagesScript {
          inherit elmLock;
        };

        exampleDotElmLinks = dotElmLinks {
          inherit elmLock registryDat;
        };

        examplePrepareElmHomeScript = prepareElmHomeScript {
          inherit elmLock registryDat;
        };

        example = buildElmApplication {
          name = "example";
          src = ./.;
          elmLock = ./elm.lock;
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
            export PS1="($name)\n$PS1"
          '';
        };

        packages = rec {
          inherit
            exampleFetchElmPackage
            exampleDotElmLinks
            example;

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

          reviewedExample = example.override {
            doElmReview = true;
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

        scripts = {
          inherit
            examplePrepareElmHomeScript
            exampleSymbolicLinksToPackagesScript;
        };
      }
    );
}
