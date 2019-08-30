{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "honeycomb"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "Copyright (c) 2019 Earnest Research";
      maintainer = "gcoady@earnestresearch.com";
      author = "Gary Coady";
      homepage = "https://github.com/EarnestResearch/honeycomb-haskell";
      url = "";
      synopsis = "Honeycomb monitoring client";
      description = "Honeycomb monitoring client";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.aeson)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.hashable)
          (hsPkgs.http-client)
          (hsPkgs.http-client-tls)
          (hsPkgs.http-types)
          (hsPkgs.microlens)
          (hsPkgs.microlens-mtl)
          (hsPkgs.mtl)
          (hsPkgs.network-uri)
          (hsPkgs.scientific)
          (hsPkgs.stm)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.unliftio)
          (hsPkgs.unordered-containers)
          (hsPkgs.uuid)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }