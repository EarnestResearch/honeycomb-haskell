{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:

with import <nixpkgs> {};

let
  drv = haskellPackages.callPackage ./. { inherit pkgs compiler; };
in
  drv.env.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [ haskellPackages.ghcid cabal-install ];
  })
