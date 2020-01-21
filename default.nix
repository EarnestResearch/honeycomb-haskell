{ pkgs ? import ./nixpkgs {}
}:
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  ghc = pkgs.haskell-nix.compiler.ghc865;
  index-state = "2019-10-20T00:00:00Z";
}
