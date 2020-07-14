let
  hsPkgs = import ./. {};
in
{
  honeycomb = hsPkgs.honeycomb.checks;
  honeycomb-wai = hsPkgs.honeycomb-wai.checks;
  honeycomb-servant = hsPkgs.honeycomb-servant.checks;
}
