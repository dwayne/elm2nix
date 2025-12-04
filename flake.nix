{
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        #
        # FIXME: Given that I'm setting `doCheck = false` maybe
        # I don't need to add this preCheck anymore.
        #
        # But maybe I should still keep it in case someone turns
        # on the checks downstream?
        #
        elm2nix = (pkgs.haskellPackages.callPackage ./elm2nix.nix {}).overrideAttrs (old: {
          preCheck = (old.preCheck or "") + ''
            export HSPEC_SKIP="(skip)"
          '';
        });
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
