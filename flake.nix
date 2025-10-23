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
          pkgs.jq
        ];

        shellHook = ''
          export PS1="($name) $PS1"
        '';
      };
    };
}
