{ pkgs ? import ./nixpkgs {}
, ghc ? "ghc865"
}:
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  ghc = pkgs.haskell-nix.compiler.${ghc};
  index-state = "2020-01-15T00:00:00Z";
  # update if build changes
  plan-sha256 = "1gs3wnj91ck4wx41fqx55pljv2s2rh2wa338cz6lbz9fwl88n9gn";
}
