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

          debuggedExample = example.override {
            enableDebugger = true;
            output = "debugged.js";
          };

          formattingCheckedExample = example.override {
            doElmFormat = true;
            elmFormatSourceFiles = [ "review/src" "src" "tests" ];
            output = "formatting-checked.js";
          };

          testedExample = formattingCheckedExample.override {
            doElmTest = true;
            output = "tested.js";
          };

          reviewedExample = testedExample.override {
            doElmReview = true;
            output = "reviewed.js";
          };

          optimizedExample = reviewedExample.override {
            enableOptimizations = true;
            output = "optimized.js";
          };

          optimized2Example = optimizedExample.override {
            optimizeLevel = 2;
            output = "optimized2.js";
          };

          optimized3Example = optimizedExample.override {
            optimizeLevel = 3;
            output = "optimized3.js";
          };

          combined1Example = optimizedExample.override {
            entry = [ "src/Main.elm" "src/Workshop.elm" ];
            output = "combined1.js";
          };

          #
          # N.B.: The following isn't allowed since elm-optimize-level-2 doesn't support multiple entries.
          #
          # When you attempt to build this derivation it will fail as expected.
          #
          combined2Example = optimized2Example.override {
            entry = [ "src/Main.elm" "src/Workshop.elm" ];
            output = "combined2.js";
          };

          minifiedExample = optimized2Example.override {
            doMinification = true;
            useTerser = true;
            output = "minified.js";
          };

          compressedExample = minifiedExample.override {
            doCompression = true;
            output = "compressed.js";
          };

          reportedExample = compressedExample.override {
            doReporting = true;
            output = "reported.js";
          };

          hashedExample = reportedExample.override {
            doContentHashing = true;
            hashLength = 12;
            keepFilesWithNoHashInFilenames = true;
            output = "hashed.js";
          };

          #
          # N.B. The finalExample derivation is equivalent to the hashedExample derivation.
          #
          finalExample = buildElmApplication {
            name = "example";
            src = ./.;
            elmLock = ./elm.lock;

            doElmFormat = true;
            elmFormatSourceFiles = [ "review/src" "src" "tests" ];

            doElmTest = true;
            doElmReview = true;

            enableOptimizations = true;
            optimizeLevel = 2;

            doMinification = true;
            useTerser = true;

            doCompression = true;
            doReporting = true;

            doContentHashing = true;
            hashLength = 12;
            keepFilesWithNoHashInFilenames = true;

            output = "hashed.js";
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
