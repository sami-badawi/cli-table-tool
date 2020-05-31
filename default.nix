{ mkDerivation, base, hpack, hspec, optparse-applicative, split
, stdenv, text
}:
mkDerivation {
  pname = "cli-table-tool";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base optparse-applicative split text ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base optparse-applicative split text
  ];
  testHaskellDepends = [
    base hspec optparse-applicative split text
  ];
  prePatch = "hpack";
  homepage = "https://github.com/sami-badawi/cli-table-tool#readme";
  license = stdenv.lib.licenses.mit;
}
