{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# override scripts with custom configuration
, customConfig ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix cardano-explorer-api --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
, gitrev ? pkgs.iohkNix.commitIdFromGitRepoOrZero ./.git
}:
with pkgs; with commonLib;
let

  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages cardanoRestHaskellPackages);

  scripts = callPackage ./nix/scripts.nix { inherit customConfig; };

  # NixOS tests
  #nixosTests = import ./nix/nixos/tests {
  #  inherit pkgs;
  #};

  dockerImages = let
    stateDir = "/data";
    defaultConfig = rec {
      services.cardano-submit-api.socketPath = stateDir + "/node.socket";
    };
    customConfig' = lib.mkMerge [ defaultConfig customConfig ];
  in pkgs.callPackage ./nix/docker.nix {
    inherit (self) cardano-explorer-api;
    inherit (self) cardano-submit-api;
    scripts = callPackage ./nix/scripts.nix { customConfig = customConfig'; };
  };

  self = {
    inherit haskellPackages
      scripts
      dockerImages
      #nixosTests
    ;

    # Grab the executable component of our package.
    inherit (haskellPackages.cardano-explorer-api.components.exes)
      cardano-explorer-api cardano-explorer-api-compare cardano-explorer-api-validate;
    inherit (haskellPackages.cardano-submit-api.components.exes)
      cardano-submit-api;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
};
in self
