{ mkDerivation, aeson, aeson-pretty, base, binary, bytestring
, containers, hspec, indexed-traversable, lib, optparse-applicative
, text, typed-process, unliftio
}:
mkDerivation {
  pname = "elm2nix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base binary bytestring containers
    indexed-traversable optparse-applicative text typed-process
    unliftio
  ];
  executableHaskellDepends = [ base text ];
  testHaskellDepends = [
    aeson base binary bytestring containers hspec text
  ];
  description = "Create Elm artifacts to be used when compiling Elm applications with Nix";
  license = lib.licenses.bsd3;
  mainProgram = "elm2nix";
}
