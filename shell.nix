let
  hsPkgs = import ./default.nix {};
in
  hsPkgs.honeycomb.components.all
