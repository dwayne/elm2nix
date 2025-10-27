{
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";

      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        name = "dev";

        packages = [
          pkgs.haskell.compiler.ghc9102
          pkgs.hlint
          pkgs.cabal-install
        ];

        shellHook = ''
          export PS1="($name) $PS1"
        '';
      };
    };
}
