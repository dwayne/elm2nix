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
          ];

          shellHook = ''
            export PS1="($name) $PS1"
          '';
        };

        packages = {
          inherit example;
          default = example;
          checkedExample = example.override {
            doValidateFormat = true;
          };
        };
      }
    );
}
