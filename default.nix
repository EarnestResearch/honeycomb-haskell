{ pkgs ? import ./nixpkgs {}
, ghc ? "ghc865"
}:
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  ghc = pkgs.haskell-nix.compiler.${ghc};
  index-state = "2020-01-15T00:00:00Z";

  # update everything below if cabal plan changes
  plan-sha256 = "10y5d3lcmx0bggm0qymjafzw653mc236hjm21q2r7f2ja64dsw21";
  materialized = ./materialized;
}
