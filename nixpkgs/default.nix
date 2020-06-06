{ system ? builtins.currentSystem }:
let
  er-nix = import (
    builtins.fetchGit {
      url = "https://github.com/EarnestResearch/er-nix.git";
      ref = "refs/heads/master";
      # git ls-remote git@github.com:EarnestResearch/er-nix refs/heads/master | awk '{ print "rev = \""$1"\";" }'
      rev = "264c9b1992891b8929f2e86a4b8b40f804ac8177";
    }
  );
in

er-nix.pkgsForSystem (system)
