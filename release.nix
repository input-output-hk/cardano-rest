############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ cardano-rest ? { outPath = ./.; rev = "abcdef"; }

# Function arguments to pass to the project
, projectArgs ? {
    config = { allowUnfree = false; inHydra = true; };
    gitrev = cardano-rest.rev;
  }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]

# The systems used for cross-compiling
, supportedCrossSystems ? [ "x86_64-linux" ]

# A Hydra option
, scrubJobs ? true

# Dependencies overrides
, sourcesOverride ? {}

# Import pkgs, including IOHK common nix lib
, pkgs ? import ./nix { inherit sourcesOverride; }

}:

with (import pkgs.iohkNix.release-lib) {
  inherit pkgs;
  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import cardano-rest;
  gitrev = cardano-rest.rev;
};

with pkgs.lib;

let
  nixosTests = (import ./. {}).nixosTests;
  getArchDefault = system: let
    table = {
      x86_64-linux = import ./. { system = "x86_64-linux"; };
      x86_64-darwin = import ./. { system = "x86_64-darwin"; };
      x86_64-windows = import ./. { system = "x86_64-linux"; crossSystem = "x86_64-windows"; };
    };
  in table.${system};
  default = getArchDefault builtins.currentSystem;
  makeScripts = cluster: let
    getScript = name: {
      x86_64-linux = (getArchDefault "x86_64-linux").scripts.${cluster}.${name};
      x86_64-darwin = (getArchDefault "x86_64-darwin").scripts.${cluster}.${name};
    };
  in {
    node = getScript "node";
  };
  wrapDockerImage = cluster: let
    images = (getArchDefault "x86_64-linux").dockerImage;
    wrapImage = image: pkgs.runCommand "${image.name}-hydra" {} ''
      mkdir -pv $out/nix-support/
      cat <<EOF > $out/nix-support/hydra-build-products
      file dockerimage-${image.name} ${image}
      EOF
    '';
  in {
    node = wrapImage images.${cluster};
  };
  makeRelease = cluster: {
    name = cluster;
    value = {
      scripts = makeScripts cluster;
      dockerImage = wrapDockerImage cluster;
    };
  };
  extraBuilds = let
    # only build nixos tests for linux
    default = getArchDefault "x86_64-linux";
  in {
    inherit nixosTests;
  } // (builtins.listToAttrs (map makeRelease [
    "mainnet"
    "staging"
    "shelley_staging_short"
    "shelley_staging"
    "testnet"
  ]));

  testsSupportedSystems = [ "x86_64-linux" ];
  # Recurse through an attrset, returning all test derivations in a list.
  collectTests' = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  # Adds the package name to the test derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectTests = ds: concatLists (
    mapAttrsToList (packageName: package:
      map (drv: drv // { inherit packageName; }) (collectTests' package)
    ) ds);

  inherit (systems.examples) mingwW64 musl64;

  jobs = {
    native = mapTestOn (__trace (__toJSON (packagePlatforms project)) (packagePlatforms project));
    musl64 = mapTestOnCross musl64 (packagePlatformsCross project);
    # Fully-static linux binary (placeholder - does not build)
    cardano-rest-linux64 = let
      name = "cardano-rest";
      tarname = "${name}-linux64.tar.gz";
    in pkgs.runCommand "${name}-linux64" {
      buildInputs = with pkgs.buildPackages; [ gnutar gzip binutils ];
    } ''
      cp -R ${jobs.musl64.cardano-explorer-api.x86_64-linux}/bin ${name}
      cp -R ${jobs.musl64.cardano-submit-api.x86_64-linux}/bin ${name}
      chmod -R 755 ${name}
      strip ${name}/*

      mkdir -p $out/nix-support
      tar -czf $out/${tarname} ${name}
      echo "file binary-dist $out/${tarname}" > $out/nix-support/hydra-build-products
    '';

    # macOS binary and dependencies in tarball
    cardano-rest-macos64 = let
      name = "cardano-rest";
      tarname = "${name}-macos64.tar.gz";
    in pkgs.runCommand "${name}-macos64" {
      buildInputs = with pkgs.buildPackages; [ gnutar gzip binutils nix ];
    } ''
      mkdir -p ${name}/bin
      cp -R ${jobs.native.cardano-explorer-api.x86_64-darwin}/bin ${name}
      cp -R ${jobs.native.cardano-submit-api.x86_64-darwin}/bin ${name}
      chmod -R 755 ${name}

      mkdir -p $out/nix-support
      tar -czf $out/${tarname} ${name}
      echo "file binary-dist $out/${tarname}" > $out/nix-support/hydra-build-products
    '';

  } // (mkRequiredJob (
      collectTests jobs.native.checks ++
      collectTests jobs.native.benchmarks ++ [
      jobs.native.cardano-explorer-api.x86_64-linux
      jobs.native.cardano-explorer-api-compare.x86_64-linux
      jobs.native.cardano-explorer-api-validate.x86_64-linux
      jobs.native.cardano-submit-api.x86_64-linux
    ]));

in jobs
