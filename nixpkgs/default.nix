{ system ? builtins.currentSystem }:
let
  er-nix = import (
    builtins.fetchGit {
      url = "https://github.com/EarnestResearch/er-nix.git";
      ref = "refs/heads/master";
      # git ls-remote git@github.com:EarnestResearch/er-nix refs/heads/master | awk '{ print "rev = \""$1"\";" }'
      rev = "dc79aebc8538a66d35ae556772c84567b12e570b";
    }
  );
in
er-nix.pkgsForSystem (system)
