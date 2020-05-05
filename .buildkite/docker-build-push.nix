# This script will load nix-built docker image of cardano-explorer-api application
# into the Docker daemon (must be running), and then push to the Docker Hub.
# Credentials for the hub must already be installed with # "docker login".
#
# There is a little bit of bash logic to replace the default repo and
# tag from the nix-build (../nix/docker.nix).

{ restPackages ?  import ../. {}

# Build system's Nixpkgs. We use this so that we have the same docker
# version as the docker daemon.
, hostPkgs ? import <nixpkgs> {}
}:

with hostPkgs;
with hostPkgs.lib;

let
  images = map impureCreated [
    restPackages.dockerImages.explorerApi
    restPackages.dockerImages.submitApi
  ];

  # Override Docker image, setting its creation date to the current time rather than the UNIX epoch.
  impureCreated = image: image.overrideAttrs (oldAttrs: { created = "now"; }) // { inherit (image) imageName; };

in
  writeScript "docker-build-push" (''
    #!${runtimeShell}

    set -euo pipefail

    export PATH=${lib.makeBinPath [ docker gnused ]}

  '' + concatMapStringsSep "\n" (image: ''
    fullrepo="${image.imageName}"
    branch="''${BUILDKITE_BRANCH:-}"
    event="''${GITHUB_EVENT_NAME:-}"

    gitrev="${image.imageTag}"

    echo "Loading $fullrepo:$gitrev"
    docker load -i ${image}

    # If there is a release, it needs to be tagged with the release
    # version (e.g. "v0.0.28") AND the "latest" tag
    if [[ "$event" = release ]]; then
      ref="''${GITHUB_REF:-}"
      version="$(echo $ref | sed -e 's/refs\/tags\///')"

      echo "Tagging with a version number: $fullrepo:$version"
      docker tag $fullrepo:$gitrev $fullrepo:$version
      echo "Pushing $fullrepo:$version"
      docker push "$fullrepo:$version"

      echo "Tagging as latest"
      docker tag $fullrepo:$version $fullrepo:latest
      echo "Pushing $fullrepo:latest"
      docker push "$fullrepo:latest"
    # Every commit to master needs to be tagged with master
    elif [[ "$branch" = master ]]; then
      echo "Tagging as master"
      docker tag $fullrepo:$gitrev $fullrepo:$branch
      echo "Pushing $fullrepo:$branch"
      docker push "$fullrepo:$branch"
    fi

    echo "Cleaning up with docker system prune"
    docker system prune -f
  '') images)
