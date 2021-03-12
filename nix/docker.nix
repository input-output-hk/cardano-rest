############################################################################
# Docker image builder
#
# To build and load into the Docker engine:
#
#   docker load -i $(nix-build -A dockerImages.explorerApi --no-out-link)
#
#  cardano-explorer-api
#    docker run -v $PATH_TO/pgpass:/configuration/pgpass inputoutput/cardano-explorer-api:<TAG>
#
#  See the docker-compose.yml for demonstration of using Docker secrets instead of mounting a pgpass
#
############################################################################

{ iohkNix
, commonLib
, dockerTools

# The main contents of the image.
, cardano-explorer-api
, scripts

# Get the current commit
, gitrev ? iohkNix.commitIdFromGitRepoOrZero ../.git

# Other things to include in the image.
, bashInteractive
, cacert
, coreutils
, curl
, glibcLocales
, iana-etc
, iproute
, iputils
, socat
, utillinux
, writeScript
, writeScriptBin
, runtimeShell
, lib
, customConfig

, explorerApiRepoName ? "inputoutput/cardano-explorer-api"
}:

let
  explorerApiPort = customConfig.services.cardano-explorer-api.port or 8100;

  # Layer of tools which aren't going to change much between versions.
  baseImage = dockerTools.buildImage {
    name = "base-env";
    contents = [
      bashInteractive   # Provide the BASH shell
      cacert            # X.509 certificates of public CA's
      coreutils         # Basic utilities expected in GNU OS's
      curl              # CLI tool for transferring files via URLs
      glibcLocales      # Locale information for the GNU C Library
      iana-etc          # IANA protocol and port number assignments
      iproute           # Utilities for controlling TCP/IP networking
      iputils           # Useful utilities for Linux networking
      socat             # Utility for bidirectional data transfer
      utillinux         # System utilities for Linux
    ];
  };
  # this will ensure the entry-point script is in a seperate layer, making the diff's smaller
  explorerApiWithoutConfigImage = dockerTools.buildImage {
    name = "explorer-api-without-config";
    fromImage = baseImage;
    contents = [
      cardano-explorer-api
    ];
  };
  explorerApiDockerImage = let
    genPgPass = writeScript "gen-pgpass" ''
      #!${runtimeShell}
      set -euo pipefail
      SECRET_DIR=$1
      echo "Generating PGPASS file"
      POSTGRES_DB=''${POSTGRES_DB:-$(< ''${SECRET_DIR}/postgres_db)}
      POSTGRES_USER=''${POSTGRES_USER:-$(< ''${SECRET_DIR}/postgres_user)}
      POSTGRES_PASSWORD=''${POSTGRES_PASSWORD:-$(< ''${SECRET_DIR}/postgres_password)}
      POSTGRES_PORT=''${POSTGRES_PORT:-5432}
      echo "''${POSTGRES_HOST}:''${POSTGRES_PORT}:''${POSTGRES_DB}:''${POSTGRES_USER}:''${POSTGRES_PASSWORD}" > /configuration/pgpass
      chmod 0600 /configuration/pgpass
    '';
    clusterStatements = lib.concatStringsSep "\n" (lib.mapAttrsToList (_: value: value) (commonLib.forEnvironmentsCustom (env: ''
      elif [[ "$NETWORK" == "${env.name}" ]]; then
        exec ${scripts.${env.name}.explorer-api}
    '') scripts.environments));
    entry-point = writeScriptBin "entry-point" ''
      #!${runtimeShell}
      set -euo pipefail
      # set up /tmp (override with TMPDIR variable)
      mkdir -p /configuration
      mkdir -p -m 1777 tmp
      if [ ! -f /configuration/pgpass ]
      then
        ${genPgPass} /run/secrets
      fi
      export PGPASSFILE="/configuration/pgpass";
      if [[ -z "$NETWORK" ]]; then
         exec ${cardano-explorer-api}/bin/cardano-explorer-api $@
         ${clusterStatements}
      else
        echo "Managed configuration for network "$NETWORK" does not exist"
      fi
    '';
  in dockerTools.buildImage {
    name = "${explorerApiRepoName}";
    fromImage = explorerApiWithoutConfigImage;
    tag = "${gitrev}";
    created = "now";   # Set creation date to build time. Breaks reproducibility
    contents = [ entry-point ];
    config = {
      EntryPoint = [ "${entry-point}/bin/entry-point" ];
      ExposedPorts = {
        "${toString explorerApiPort}/tcp" = {};
      };
    };
  };

in {
  explorerApi = explorerApiDockerImage;
}
