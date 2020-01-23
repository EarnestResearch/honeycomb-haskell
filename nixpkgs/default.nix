{ system ? builtins.currentSystem }:
let
  er-nix = import (
    builtins.fetchGit {
      url = "https://github.com/EarnestResearch/er-nix.git";
      ref = "refs/heads/master";
      # git ls-remote git@github.com:EarnestResearch/er-nix refs/heads/master | awk '{ print "rev = \""$1"\";" }'
      rev = "ca1226a70855bea7d311ef27e85e945158c084fe";
    }
  );
in

er-nix.pkgsForSystem (system)
