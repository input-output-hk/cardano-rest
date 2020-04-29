# Monolithic Docker Container

*NOTE*: This is deprecated. Please use the docker-compose file that runs each service in it's own container.

Postgres is configured for RAM >= 8 GB in this monolithic docker container.

# Building

To build the image as a tarball:

`nix build -f nix/docker-monolithic image`

Helper script that auto-loads image:

`nix build -f nix/docker-monolithic testDockerImage && ./result`

# Usage

`docker run --rm -t -i -p 81:80 --tty --cap-add SYS_PTRACE --name test-image --volume explorer-testnet:/var/ docker-image:${network}`

# Monitoring

This image provides prometheus exporters at the node, db-sync, submit-api and postgres levels.

| Service | Port | Path |
| cardano-node | 12798 | /metrics |
| cardano-db-sync | 8080 | / |
| cardano-submit-api | 8081 | / |
| postgres | 9187 | /metrics |

# Nix build docker image

If you don't have nix, you can utilize the dockerfile that will build all the components in the docker image.

Note that this creates a rather large image (multiple GB).

`docker build -t cardano-explorer-docker -f nix/docker-monolithic`
