{ pkgs ? import ./nixpkgs {}
, ghc ? "ghc865"
}:
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  ghc = pkgs.haskell-nix.compiler.${ghc};
  index-state = "2020-01-15T00:00:00Z";
}
