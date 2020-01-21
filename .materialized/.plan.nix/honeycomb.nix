let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
    '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
    '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
    '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
    '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
    '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
    '';
in
{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "honeycomb"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Earnest Research";
      maintainer = "gcoady@earnestresearch.com";
      author = "Gary Coady";
      homepage = "https://github.com/EarnestResearch/honeycomb-haskell#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on Github at <https://github.com/EarnestResearch/honeycomb-haskell#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [ "README.md" "ChangeLog.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."hashable" or (buildDepError "hashable"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."microlens" or (buildDepError "microlens"))
          (hsPkgs."microlens-mtl" or (buildDepError "microlens-mtl"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."network-uri" or (buildDepError "network-uri"))
          (hsPkgs."random" or (buildDepError "random"))
          (hsPkgs."scientific" or (buildDepError "scientific"))
          (hsPkgs."stm" or (buildDepError "stm"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."unliftio" or (buildDepError "unliftio"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."uuid" or (buildDepError "uuid"))
        ];
        buildable = true;
        modules = [
          "Paths_honeycomb"
          "Honeycomb"
          "Honeycomb/Api"
          "Honeycomb/Api/Events"
          "Honeycomb/Api/Types"
          "Honeycomb/Api/Types/ApiHost"
          "Honeycomb/Api/Types/ApiKey"
          "Honeycomb/Api/Types/Dataset"
          "Honeycomb/Api/Types/Event"
          "Honeycomb/Api/Types/HoneyObject"
          "Honeycomb/Api/Types/HoneyValue"
          "Honeycomb/Api/Types/RequestOptions"
          "Honeycomb/Trace"
          "Honeycomb/Trace/Types"
          "Honeycomb/Trace/Types/ServiceName"
          "Honeycomb/Trace/Types/SpanContext"
          "Honeycomb/Trace/Types/SpanId"
          "Honeycomb/Trace/Types/SpanName"
          "Honeycomb/Trace/Types/SpanReference"
          "Honeycomb/Trace/Types/TraceId"
          "Honeycomb/Transport"
          "Honeycomb/Core"
          "Honeycomb/Core/Types"
          "Honeycomb/Core/Types/Honey"
          "Honeycomb/Core/Types/HoneyEvent"
          "Honeycomb/Core/Types/HoneyException"
          "Honeycomb/Core/Types/HoneyOptions"
          "Honeycomb/Core/Types/HoneyResponse"
          "Honeycomb/Core/Types/HoneyServerOptions"
          "Honeycomb/Core/Types/TransportState"
        ];
        hsSourceDirs = [ "src" ];
      };
    };
  } // rec { src = (pkgs.lib).mkDefault ../.; }
