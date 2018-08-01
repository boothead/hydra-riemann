{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, hriemann, hspec, network, optparse-applicative, QuickCheck
, scientific, stdenv, text, time, uri-encode, websockets
}:
mkDerivation {
  pname = "hydra-riemann";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base containers hriemann network
    optparse-applicative scientific text uri-encode websockets
  ];
  executableHaskellDepends = [
    base hriemann network optparse-applicative
  ];
  testHaskellDepends = [
    aeson attoparsec base bytestring hriemann hspec QuickCheck time
  ];
  homepage = "https://github.com/boothead/hydra-riemann";
  description = "Export build metrics from the hydra CI system to riemann";
  license = stdenv.lib.licenses.mit;
}
