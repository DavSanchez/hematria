{ mkDerivation, base, containers, directory, hspec, http-client
, http-conduit, lib, network-uri, optparse-applicative, QuickCheck
, random, tar, text, yaml, zlib
}:
mkDerivation {
  pname = "hematria";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers directory http-client http-conduit network-uri
    random tar text yaml zlib
  ];
  executableHaskellDepends = [ base optparse-applicative text ];
  testHaskellDepends = [
    base containers directory hspec QuickCheck text yaml
  ];
  homepage = "https://davsanchez.github.io/hematria";
  description = "Perform gematria with the command line";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
