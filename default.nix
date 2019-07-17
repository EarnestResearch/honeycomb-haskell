{ pkgs ? import <nixpkgs> {} }:

let
  spec = builtins.fromJSON (builtins.readFile ./haskell-nix-src.json);
  haskell = import(pkgs.fetchgit {
    name = "haskell-lib";
    inherit (spec) url rev sha256 fetchSubmodules;
  }) {};

  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = import (haskell.callStackToNix {
      src = ./.;
    });
    pkg-def-extras = [];
    modules = [];
  };
in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
