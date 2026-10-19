{ mkDerivation, base, binary, bytestring, containers, hspec
, indexed-traversable, json-codec, json-parser, lib
, optparse-applicative, text, typed-process, unliftio
}:
mkDerivation {
  pname = "elm2nix";
  version = "0.3";
  src = ../..;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base binary bytestring containers indexed-traversable json-codec
    json-parser optparse-applicative text typed-process unliftio
  ];
  executableHaskellDepends = [ base text ];
  testHaskellDepends = [
    base binary bytestring containers hspec json-codec text
  ];
  doHaddock = false;
  doCheck = false;
  description = "Create Elm support files to be used when compiling Elm applications with Nix";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
  mainProgram = "elm2nix";
}
