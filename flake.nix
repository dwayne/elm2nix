{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-utils-src = { url = "github:dwayne/nix-utils"; flake = false; };
    elm2nix-src = { url = ./.; flake = false; };
  };

  outputs = { self, nixpkgs, nix-utils-src, elm2nix-src }:
    let
      lib = nixpkgs.lib;
      nixUtils = import nix-utils-src lib;
      elm2nix = import elm2nix-src;
    in
    nixUtils.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ elm2nix.overlays.default ];
        };

        elm2nixLib = elm2nix.lib.mkLib pkgs;
      in
      {
        devShells = {
          default = pkgs.mkShell {
            name = "elm2nix";

            inputsFrom = [ self.packages.${system}.elm2nixWithCheck.env ];

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

            inputsFrom = [ self.packages.${system}.elm2nixWithCheck.env ];

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

        packages = {
          default = elm2nixLib.elm2nix;
          elm2nix = elm2nixLib.elm2nix;
          elm2nixWithCheck = pkgs.haskell.lib.doCheck elm2nixLib.elm2nix;
        };

        apps = {
          default = self.apps.${system}.elm2nix;
          elm2nix = {
            type = "app";
            program = lib.getExe self.packages.${system}.elm2nix;
            meta.description = "The elm2nix CLI application";
          };
        };

        checks = {
          elm2nixWithCheck = self.packages.${system}.elm2nixWithCheck;
        };
      }
    ) // elm2nix;
}
