#
# Extends a Haskell package set with elm2nix and its dependencies.
# Apply it with `haskellPackages.extend`.
#
{ haskell, lib }:

final: _:

let
  fs = lib.fileset;
  inherit (haskell.lib.compose) overrideSrc overrideCabal;
in
{
  json-parser = final.callPackage ./generated/json-parser.nix {};
  json-codec = final.callPackage ./generated/json-codec.nix {};

  elm2nix = lib.pipe (final.callPackage ./generated/elm2nix.nix {}) [
    (overrideSrc {
      src = fs.toSource {
        root = ../.;
        fileset = fs.unions [
          ../app
          ../src
          ../tests
          ../elm2nix.cabal
          ../LICENSE
        ];
      };
    })

    #
    # N.B. Even though `doCheck = false` by default I add this `preCheck`
    #      to ensure that downstream users don't have to worry about it.
    #
    (overrideCabal (old: {
      preCheck = (old.preCheck or "") + ''
        export HSPEC_SKIP="(skip)"
      '';
    }))
  ];
}
