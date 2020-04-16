{ system ? builtins.currentSystem }:
let
  er-nix = import (
    builtins.fetchGit {
      url = "https://github.com/EarnestResearch/er-nix.git";
      ref = "refs/heads/master";
      # git ls-remote git@github.com:EarnestResearch/er-nix refs/heads/master | awk '{ print "rev = \""$1"\";" }'
      rev = "5620dd27db8071391141137c1c91a38fd61d4bde";
    }
  );
in

er-nix.pkgsForSystem (system)
