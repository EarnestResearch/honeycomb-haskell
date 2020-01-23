{ pkgs ? import ./nixpkgs {} }:

let
  hsPkgs = import ./default.nix { inherit pkgs; };
in

hsPkgs.shellFor {
  buildInputs = [ pkgs.cabal-install ];
  exactDeps = true;

  inherit (pkgs.earnestresearch.pre-commit-check) shellHook;
}
