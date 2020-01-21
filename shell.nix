{ pkgs ? import ./nixpkgs {} }:

let
  hsPkgs = import ./default.nix { inherit pkgs; };
in

hsPkgs.shellFor {
  packages = ps: with ps; [
    honeycomb
  ];

  buildInputs = with pkgs; [
    cabal-install
    hlint
    ormolu
  ];

  inherit (pkgs.earnestresearch.pre-commit-check) shellHook;
}
