{ mkDerivation, base, bytestring, directory, fetchzip, filepath
, hspec, hspec-discover, hspec-megaparsec, lib, megaparsec
, scientific, text
}:
mkDerivation {
  pname = "json-parser";
  version = "0.1.0.0";
  src = fetchzip {
    url = "https://github.com/dwayne/hs-json-parser/archive/5764dec55b7928f42b4d35ba062f50f1e23b8041.tar.gz";
    sha256 = "0y3rkppc8a0x3hwjsqlzf2qypzl2qfghjz0s1h5idanqvn9qlky1";
  };
  libraryHaskellDepends = [
    base bytestring megaparsec scientific text
  ];
  testHaskellDepends = [
    base bytestring directory filepath hspec hspec-megaparsec
    megaparsec text
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  doCheck = false;
  description = "A JSON parser compliant with RFC 8259";
  license = lib.meta.getLicenseFromSpdxId "BSD-3-Clause";
}
