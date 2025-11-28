{
  description = "elm2nix developer shell, package, and app";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        elm2nix = (pkgs.haskellPackages.callPackage ./elm2nix.nix {}).overrideAttrs (old: {
          preCheck = (old.preCheck or "") + ''
            export HSPEC_SKIP="(skip)"
          '';
        });
      in
      {
        devShells.default = pkgs.mkShell {
          name = "dev";

          packages = [
            pkgs.ghc
            pkgs.hlint
            pkgs.cabal-install
            pkgs.cabal2nix
          ];

          shellHook = ''
            export PS1="($name) $PS1"
            export PROJECT_ROOT="$PWD"
            export HSPEC_SKIP="(skip:network)"
          '';
        };

        packages = {
          default = elm2nix;
          inherit elm2nix;
        };

        apps = {
          default = self.apps.${system}.elm2nix;
          elm2nix = flake-utils.lib.mkApp { drv = elm2nix; };
        };
      }
    ) // {
      lib.elm2nix = pkgs: pkgs.callPackage ./nix {};
    };
}
