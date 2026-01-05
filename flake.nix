{
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        elm2nix = pkgs.callPackage ./nix/elm2nix.nix {};
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
          ];

          shellHook = ''
            export PS1="($name)\n$PS1"
            export PROJECT_ROOT="$PWD"
            export HSPEC_SKIP="(skip:network)"

            lint () {
              hlint "$PROJECT_ROOT/src" "$PROJECT_ROOT/test"
            }
            export -f lint

            alias b='cabal build'
            alias l='lint'
            alias t='cabal test'
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
