{ pkgs ? import ./nixpkgs {}
, ghc ? "ghc883"
}:
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { name = "honeycomb-haskell"; src = ./.; };
  compiler-nix-name = ghc;
  index-state = "2020-05-19T00:00:00Z";
}
