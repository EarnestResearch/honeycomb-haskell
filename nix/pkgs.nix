{
  extras = hackage:
    {
      packages = {
        "rio" = (((hackage.rio)."0.1.10.0").revisions).default;
        } // { honeycomb = ./honeycomb.nix; };
      };
  resolver = "lts-13.27";
  }