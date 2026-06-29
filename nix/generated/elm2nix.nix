{ mkDerivation, base, binary, bytestring, containers, hspec
, hspec-discover, indexed-traversable, json-codec, json-parser, lib
, optparse-applicative, text, typed-process, unliftio
}:
mkDerivation {
  pname = "elm2nix";
  version = "0.1.0.0";
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
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  doCheck = false;
  description = "Create Elm support files to be used when compiling Elm applications with Nix";
  license = lib.licenses.bsd3;
  mainProgram = "elm2nix";
}
