{ mkDerivation, base, containers, fetchzip, hspec, hspec-discover
, json-parser, lib, text
}:
mkDerivation {
  pname = "json-codec";
  version = "0.1.0.0";
  src = fetchzip {
    url = "https://github.com/dwayne/hs-json-codec/archive/838de8206a5b6192e910ce0dc2fe61e0da1aea12.tar.gz";
    sha256 = "0skpjb9yffi0ip24qpvq9y3k1f07cf0hmd0alpz71dq4nrpdxv00";
  };
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base containers json-parser text ];
  testHaskellDepends = [ base hspec json-parser text ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  doCheck = false;
  description = "JSON decoders and encoders";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
