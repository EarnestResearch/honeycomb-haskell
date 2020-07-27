{ pkgs ? import ./nixpkgs {}
, ghc ? "ghc883"
}:
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { name = "honeycomb-haskell"; src = ./.; };
  compiler-nix-name = ghc;
  index-state = "2020-06-29T00:00:00Z";
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
