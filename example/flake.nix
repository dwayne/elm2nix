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
          inherit example;
          default = example;
          debugExample = example.override {
            enableDebugger = true;
            output = "debug.js";
          };
          checkedExample = example.override {
            doValidateFormat = true;
            doElmTest = true;
            output = "checked.js";
          };
          optimizedExample = checkedExample.override {
            output = "optimized.js";
            enableOptimizations = true;
            optimizeLevel = 2;
            enableMinification = true;
            enableCompression = true;
            showStats = true;
          };
          combinedExample = optimizedExample.override {
            entry = [ "src/Main.elm" "src/Workshop.elm" ];
            output = "combined.js";
          };
          hashedExample = optimizedExample.override {
            output = "hashed.js";
            enableHashedFilenames = true;
            hashLength = 12;
            replaceWithHashedFilenames = false;
          };
        };
      }
    );
}
