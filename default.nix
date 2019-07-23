{ pkgs ? import <nixpkgs> {} }:

let
  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/9840ec47efd9e2a4434f0e1eef47d6347ec108fd.tar.gz) { inherit pkgs; };

  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = import nix/pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs
