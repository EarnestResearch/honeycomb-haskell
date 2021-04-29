{ pkgs ? import ./nixpkgs { } }:
let
  hsPkgs = import ./default.nix { inherit pkgs; };
in
hsPkgs.shellFor {
  buildInputs = [ pkgs.git ];

  tools = {
    cabal = "3.4.0.0";
  };

  exactDeps = true;

  shellHook = ''
    ${hsPkgs.pre-commit-check.shellHook}
  '';
}
