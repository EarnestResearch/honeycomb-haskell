{ pkgs ? import ./nixpkgs {}
, ghc ? "ghc865"
}:
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  ghc = pkgs.haskell-nix.compiler.${ghc};
  index-state = "2020-01-15T00:00:00Z";

  # update everything below if cabal plan changes
  plan-sha256 = "1sxq32damvz5r0kn40nwzibh82y12b9165q39yg5q2rkayql7szj";
  materialized = ./materialized;
}
