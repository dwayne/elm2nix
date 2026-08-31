{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

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
        fs = pkgs.lib.fileset;
        inherit (elm2nix.lib.elm2nix pkgs)
          buildElmApplication
          generateRegistryDat
          prepareElmHomeScript
          mkPatch
          installPatchScript
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

        lydellBrowser = mkPatch elm2nix.lib.patches.lydellBrowser;
        lydellHtml = mkPatch elm2nix.lib.patches.lydellHtml;
        lydellVirtualDom = mkPatch elm2nix.lib.patches.lydellVirtualDom;
        omnibsElmCss = mkPatch elm2nix.lib.patches.omnibsElmCss;

        installLydellBrowserScript = installPatchScript lydellBrowser;
        installLydellHtmlScript = installPatchScript lydellHtml;
        installLydellVirtualDomScript = installPatchScript lydellVirtualDom;
        installOmnibsElmCssScript = installPatchScript omnibsElmCss;

        elmSrc = fs.toSource {
          root = ./.;
          fileset = fs.unions [
            ./review
            ./src
            ./tests
            ./elm.json
          ];
        };

        example = buildElmApplication {
          name = "example";
          src = elmSrc;
          elmLock = ./elm.lock;
        };

        testScripts = pkgs.runCommand "test-scripts" {} ''
          mkdir "$out"
          echo "${examplePrepareElmHomeScript}" > "$out/examplePrepareElmHomeScript.txt"
          echo "${exampleSymbolicLinksToPackagesScript}" > "$out/exampleSymbolicLinksToPackagesScript.txt"
          echo "${installLydellBrowserScript}" > "$out/installLydellBrowserScript.txt"
          echo "${installLydellHtmlScript}" > "$out/installLydellHtmlScript.txt"
          echo "${installLydellVirtualDomScript}" > "$out/installLydellVirtualDomScript.txt"
          echo "${installOmnibsElmCssScript}" > "$out/installOmnibsElmCssScript.txt"
        '';
      in
      {
        devShells.default = pkgs.mkShell {
          name = "example";

          packages = [
            elm2nix.packages.${system}.default
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-json
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
            example
            lydellBrowser
            lydellHtml
            lydellVirtualDom
            omnibsElmCss
            testScripts
            ;

          default = example;

          debuggedExample = example.override {
            name = "debugged-example";
            enableDebugger = true;
            output = "debugged.js";
          };

          formattingCheckedExample = example.override {
            name = "formatting-checked-example";
            doElmFormat = true;
            elmFormatSourceFiles = [ "review/src" "src" "tests" ];
            output = "formatting-checked.js";
          };

          elmSafeVirtualDomExample = example.override {
            name = "elm-safe-virtual-dom-example";
            elmPackagePatches = [
              lydellBrowser
              lydellHtml
              lydellVirtualDom
            ];
          };

          #
          # N.B. The omnibsElmCss patch won't be installed since we haven't installed
          #      rtfeldman/elm-css version 18.0.0 into our example project.
          #
          # Steps to make it work:
          #
          # 1. elm install rtfeldman/elm-css
          # 2. elm2nix lock elm.json review/elm.json
          # 3. nix build .#elmSafeVirtualDomElmCssExample -L
          #
          elmSafeVirtualDomElmCssExample = example.override {
            name = "elm-safe-virtual-dom-elm-css-example";
            #
            # Try using only the arguments to mkPatch
            #
            elmPackagePatches = elm2nix.lib.elmSafeVirtualDom.elmCss;
          };

          testedExample = formattingCheckedExample.override {
            name = "tested-example";
            doElmTest = true;
            output = "tested.js";
          };

          reviewedExample = testedExample.override {
            name = "reviewed-example";
            doElmReview = true;
            output = "reviewed.js";
          };

          optimizedExample = reviewedExample.override {
            name = "optimized-example";
            enableOptimizations = true;
            output = "optimized.js";
          };

          optimized2Example = optimizedExample.override {
            name = "optimized2-example";
            optimizeLevel = 2;
            output = "optimized2.js";
          };

          optimized3Example = optimizedExample.override {
            name = "optimized3-example";
            optimizeLevel = 3;
            output = "optimized3.js";
          };

          combined1Example = optimizedExample.override {
            name = "combined1-example";
            entry = [ "src/Main.elm" "src/Workshop.elm" ];
            output = "combined1.js";
          };

          #
          # N.B.: The following isn't allowed since elm-optimize-level-2 doesn't support multiple entries.
          #
          # When you attempt to build this derivation it will fail as expected.
          #
          # combined2Example = optimized2Example.override {
          #   name = "combined2-example";
          #   entry = [ "src/Main.elm" "src/Workshop.elm" ];
          #   output = "combined2.js";
          # };
          #

          minifiedExample = optimized2Example.override {
            name = "minified-example";
            doMinification = true;
            useTerser = true;
            output = "minified.js";
          };

          compressedExample = minifiedExample.override {
            name = "compressed-example";
            doCompression = true;
            output = "compressed.js";
          };

          reportedExample = compressedExample.override {
            name = "reported-example";
            doReporting = true;
            output = "reported.js";
          };

          hashedExample = reportedExample.override {
            name = "hashed-example";
            doContentHashing = true;
            hashLength = 12;
            keepFilesWithNoHashInFilenames = true;
            output = "hashed.js";
          };

          #
          # N.B. The finalExample derivation is equivalent to the hashedExample derivation.
          #
          finalExample = buildElmApplication {
            name = "final-example";
            src = elmSrc;
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

        checks = {
          inherit
            exampleFetchElmPackage
            exampleDotElmLinks
            example
            lydellBrowser
            lydellHtml
            lydellVirtualDom
            omnibsElmCss
            testScripts
            ;

          inherit (self.packages.${system})
            debuggedExample
            formattingCheckedExample
            elmSafeVirtualDomExample
            elmSafeVirtualDomElmCssExample
            testedExample
            reviewedExample
            optimizedExample
            optimized2Example
            optimized3Example
            combined1Example
            minifiedExample
            compressedExample
            reportedExample
            hashedExample
            finalExample
            ;
        };
      }
    );
}
