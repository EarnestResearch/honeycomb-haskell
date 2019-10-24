{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:

let
  haskellPackages = pkgs.haskell.packages.${compiler};
in
  haskellPackages.callCabal2nix "honeycomb" ./. {}
