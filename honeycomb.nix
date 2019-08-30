{ mkDerivation, aeson, base, bytestring, hashable, http-client
, http-client-tls, http-types, microlens, microlens-mtl, mtl
, network-uri, random, resourcet, scientific, stdenv, stm, text
, time, unliftio, unordered-containers, uuid
}:
mkDerivation {
  pname = "honeycomb";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring hashable http-client http-client-tls
    http-types microlens microlens-mtl mtl network-uri random resourcet
    scientific stm text time unliftio unordered-containers uuid
  ];
  homepage = "https://github.com/EarnestResearch/honeycomb#readme";
  license = stdenv.lib.licenses.asl20;
}
