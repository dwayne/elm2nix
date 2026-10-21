{ elm2nix, elmVersion, runCommand }:

{ elmLock }:

let
  elm-version =
    if builtins.elem elmVersion [ "0.19.1" "0.19.2" ] then
      # Generate the 0.19.1 format for the registry.dat
      "0.19.1"
    else
      # Generate the 0.19.3 format for the registry.dat
      "0.19.3";
in
runCommand "registry.dat" {
  nativeBuildInputs = [ elm2nix ];
} ''
  elm2nix registry generate --elm-version "${elm-version}" --input "${elmLock}" --output "$out"
''
