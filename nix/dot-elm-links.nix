{ runCommand

, elmVersion
, symbolicLinksToPackagesScript
}:

{ elmLock
, registryDat
}:

runCommand "dot-elm-links" {} ''
  root="$out/${elmVersion}/packages"
  mkdir -p "$root"

  ln -s "${registryDat}" "$root/registry.dat"

  ${symbolicLinksToPackagesScript { inherit elmLock; }}
''
