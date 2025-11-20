{
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";

      pkgs = nixpkgs.legacyPackages.${system};

      compiler = "ghc9102";
      elm2nix = (pkgs.haskell.packages.${compiler}.callPackage ./elm2nix.nix {}).overrideAttrs (old: {
        preCheck = (old.preCheck or "") + ''
          export HSPEC_SKIP="(skip)"
        '';
      });
    in
    {
      devShells.${system}.default = pkgs.mkShell {
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

      packages.${system} = {
        inherit elm2nix;
        default = elm2nix;
      };
    };
}
