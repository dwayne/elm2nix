{ lib

, fetchElmPackage
}:

{ elmLock }:

#
# Assumptions:
#
#   - $root is assumed to be correctly set, see ./dot-elm-links.nix
#

builtins.foldl'
  (script: { author, package, version, sha256 } @ dep:
    script + ''
      mkdir -p "$root/${author}/${package}"
      ln -s "${fetchElmPackage dep}" "$root/${author}/${package}/${version}"
    ''
  )
  ""
  (lib.importJSON elmLock)
