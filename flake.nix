{
  description = "elm2nix developer shell, package, and app";

  outputs = { self, nixpkgs, flake-utils }:
    let
      compiler = "ghc9102";
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        elm2nix = (pkgs.haskell.packages.${compiler}.callPackage ./elm2nix.nix {}).overrideAttrs (old: {
          preCheck = (old.preCheck or "") + ''
            export HSPEC_SKIP="(skip)"
          '';
        });
      in
      {
        devShells.default = pkgs.mkShell {
          name = "dev";

          packages = [
            pkgs.haskell.compiler.${compiler}
            pkgs.hlint
            pkgs.cabal-install
            pkgs.cabal2nix
          ];

          shellHook = ''
            export PROJECT_ROOT="$PWD"
            export PS1="($name) $PS1"
            export HSPEC_SKIP="(skip:network)"
          '';
        };

        packages.default = elm2nix;
        apps.default = flake-utils.lib.mkApp { drv = elm2nix; };
      }
    );
}
