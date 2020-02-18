{ pkgs ? import ./nixpkgs {} }:

let
  hsPkgs = import ./default.nix { inherit pkgs; };
in

hsPkgs.shellFor {
  exactDeps = true;

  inherit (pkgs.earnestresearch.pre-commit-check) shellHook;
}
