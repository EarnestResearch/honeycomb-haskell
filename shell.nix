{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, hashable
      , http-client, http-client-tls, http-types, microlens
      , microlens-mtl, mtl, network-uri, random, resourcet, rio
      , scientific, stdenv, stm, text, time, unliftio, uuid
      }:
      mkDerivation {
        pname = "honeycomb";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base bytestring hashable http-client http-client-tls
          http-types microlens microlens-mtl mtl network-uri random resourcet
          rio scientific stm text time unliftio uuid
        ];
        homepage = "https://github.com/EarnestResearch/honeycomb#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
