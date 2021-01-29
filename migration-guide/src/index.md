# Getting Started

## End of life

After several years of service, cardano-rest is now considered deprecated. There will be no further development nor support done on this component stack after the [END OF LIFE DATE].
Note that it **will** however survive the Mary hardfork going happening soon. Clients currently integrated with this component are therefore encouraged to upgrade their integration to
use either [cardano-graphql](https://github.com/input-output-hk/cardano-graphql) or [cardano-rosetta](https://github.com/input-output-hk/cardano-rosetta) which are more capable,
production-ready and better documented components still under active development.

!!! info
    This guide is meant to help existing customers migrating from cardano-rest to new components by showing how information from cardano-rest can be retrieved from each components. Note that
    this guide does not cover new components installation and build instructions. For these, refer to each component's README and wiki page on their respective Github repositories.

### cardano-graphql

`Cardano-graphql` is **cross-platform**, **typed**, and **queryable API** for Cardano. The project contains multiple packages for composing GraphQL services to meet specific application demands, and a docker-compose stack serving the included cardano-graphql-server Dockerfile and the extended hasura Dockerfile. The schema is defined in native .graphql, and used to generate a TypeScript package for client-side static typing.

### cardano-rosetta

`Cardano-rosetta` is an implementation of the [Rosetta](https://www.rosetta-api.org/docs/1.4.4/welcome.html) specification for Cardano. Rosetta is an open-source specification and set of tools that makes integrating with blockchains **simpler**, **faster**, and **more reliable**.
