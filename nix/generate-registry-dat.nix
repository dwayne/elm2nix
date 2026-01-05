{ elm2nix, runCommand }:

{ elmLock }:

runCommand "registry.dat" {} ''
  ${elm2nix}/bin/elm2nix registry generate --input ${elmLock} --output "$out"
''
