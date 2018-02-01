{ mkDerivation, aeson, async, base, binary, bytestring, containers
, criterion, deepseq, directory, doctest, enumerate, frisby
, generic-lens, hashable, hspec, http-client, http-client-tls
, http-types, lens, lens-aeson, QuickCheck, schematic, semigroups
, show-prettyprint, spiros, stdenv, tar, text, time
, unordered-containers, vector, vinyl, wreq, zip-archive, zlib
}:
mkDerivation {
  pname = "magic-card-search";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson async base binary bytestring containers deepseq directory
    enumerate frisby generic-lens hashable http-client http-client-tls
    lens lens-aeson schematic semigroups show-prettyprint spiros tar
    text time unordered-containers vector vinyl zlib
  ];
  executableHaskellDepends = [
    base bytestring http-client http-client-tls http-types lens
    lens-aeson tar wreq zip-archive zlib
  ];
  testHaskellDepends = [ base doctest hspec QuickCheck ];
  benchmarkHaskellDepends = [ base criterion deepseq ];
  homepage = "http://github.com/sboosali/magic-card-search#readme";
  description = "TODO";
  license = stdenv.lib.licenses.bsd3;
}
