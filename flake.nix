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
        }
      );

      packages = forAllSystems (system: pkgs:
        let
          elm2nix = (self.lib.mkLib pkgs).elm2nix;
        in
        {
          inherit elm2nix;
          default = elm2nix;
          elm2nixWithCheck = pkgs.haskell.lib.doCheck elm2nix;
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
          elm2nixWithCheck = self.packages.${system}.elm2nixWithCheck;
        }
      );

      lib =
        rec {
          mkLib = pkgs: pkgs.callPackage ./nix {};

          elmPackagePatches = import ./nix/elm-package-patches.nix;
          elmSafeVirtualDomPatches = import ./nix/elm-safe-virtual-dom-patches.nix;
        };

      overlays.default = final: prev: {
        elmPackages = prev.elmPackages.overrideScope (efinal: eprev: {
          elm = efinal.callPackage ./nix/elm-packages/elm.nix {};
          elm-test = efinal.callPackage ./nix/elm-packages/elm-test.nix {};
        });
      };
    };
}
