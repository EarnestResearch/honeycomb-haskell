{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  ({
    flags = {};
    package = {
      specVersion = "0";
      identifier = { name = "honeycomb"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2019 Author name here";
      maintainer = "example@example.com";
      author = "Author name here";
      homepage = "https://github.com/githubuser/honeycomb#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on Github at <https://github.com/EarnestResearch/libhoney-hs#readme>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.aeson)
          (hsPkgs.base)
          (hsPkgs.conduit)
          (hsPkgs.http-client)
          (hsPkgs.http-client-tls)
          (hsPkgs.http-conduit)
          (hsPkgs.http-types)
          (hsPkgs.microlens)
          (hsPkgs.microlens-mtl)
          (hsPkgs.network-uri)
          (hsPkgs.random)
          (hsPkgs.resourcet)
          (hsPkgs.rio)
          (hsPkgs.scientific)
          (hsPkgs.stm)
          (hsPkgs.uuid)
          (hsPkgs.wai)
          ];
        };
      exes = {
        "honeycomb-example-exe" = {
          depends = [
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.conduit)
            (hsPkgs.honeycomb)
            (hsPkgs.http-client)
            (hsPkgs.http-client-tls)
            (hsPkgs.http-conduit)
            (hsPkgs.http-types)
            (hsPkgs.microlens)
            (hsPkgs.microlens-mtl)
            (hsPkgs.network-uri)
            (hsPkgs.optparse-simple)
            (hsPkgs.random)
            (hsPkgs.resourcet)
            (hsPkgs.rio)
            (hsPkgs.scientific)
            (hsPkgs.stm)
            (hsPkgs.uuid)
            (hsPkgs.wai)
            ];
          };
        };
      tests = {
        "honeycomb-test" = {
          depends = [
            (hsPkgs.aeson)
            (hsPkgs.base)
            (hsPkgs.conduit)
            (hsPkgs.honeycomb)
            (hsPkgs.hspec)
            (hsPkgs.http-client)
            (hsPkgs.http-client-tls)
            (hsPkgs.http-conduit)
            (hsPkgs.http-types)
            (hsPkgs.microlens)
            (hsPkgs.microlens-mtl)
            (hsPkgs.network-uri)
            (hsPkgs.random)
            (hsPkgs.resourcet)
            (hsPkgs.rio)
            (hsPkgs.scientific)
            (hsPkgs.stm)
            (hsPkgs.transformers)
            (hsPkgs.uuid)
            (hsPkgs.wai)
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover))
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.; }) // {
    cabal-generator = "hpack";
    }