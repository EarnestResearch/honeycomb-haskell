{ pkgs ? import <nixpkgs> {} }:

let
  hsPkgs = import ./default.nix { inherit pkgs; };
in
  hsPkgs.shellFor {
      packages = ps: [ps.honeycomb];
      withHoogle = true;
  }
