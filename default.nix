{ pkgs ? import ./nixpkgs {}
, ghc ? "ghc883"
}:
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { name = "honeycomb-haskell"; src = ./.; };
  ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${ghc};
  index-state = "2020-04-15T00:00:00Z";
}
