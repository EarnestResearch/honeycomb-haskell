{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
{ honeycomb = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./honeycomb.nix { };
}
