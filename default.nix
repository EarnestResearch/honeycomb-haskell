{ pkgs ? import ./nixpkgs { }
, ghc ? "ghc884"
}:
pkgs.haskell-nix.project
  {
    src = pkgs.haskell-nix.haskellLib.cleanGit { name = "honeycomb-haskell"; src = ./.; };
    compiler-nix-name = ghc;
    index-state = "2021-04-24T00:00:00Z";
  } // {
  pre-commit-check = pkgs.pre-commit-hooks.run {
    src = ./.;
    hooks = {
      cabal-fmt.enable = true;
      hlint.enable = true;
      nixpkgs-fmt.enable = true;
    };
  };
}
