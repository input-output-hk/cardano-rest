<p align="center">
  <big><strong>Cardano REST</strong></big>
</p>

<p align="center">
  <img width="200" src=".github/images/cardano-logo.png"/>
</p>

<p align="center">
  <a href="https://buildkite.com/input-output-hk/cardano-rest/builds?branch=master"><img src="https://img.shields.io/buildkite/f77fe35896f31d3fd6afd32a4700457e0ad9fbaab10b88b746/master?color=%232ecc71&label=%20%F0%9F%AA%81%20BUILD&style=for-the-badge"/></a>
  <a href="https://github.com/input-output-hk/cardano-rest/releases"><img src="https://img.shields.io/github/v/release/input-output-hk/cardano-rest?color=%239b59b6&label=%20%F0%9F%9A%80%20RELEASE&sort=semver&style=for-the-badge"/></a>
</p>

<hr/>

## Overview

Cardano REST provides a set of APIs for interacting with on-chain data
through JSON over HTTP.

:warning: These APIs are the currently supported APIs. In future they may be augmented or replaced by a GraphQL API of [cardano-graphql](https://github.com/input-output-hk/cardano-graphql), when GraphQL component will be released. These APIs will be supported for some time, even once alternatives become available.

## Getting Started

1. Clone the repository.

```
$ git clone git@github.com:input-output-hk/cardano-rest.git
$ cd cardano-rest
```

2. Start `cardano-node`, `postgresql` and `cardano-rest` components using Docker:

```
$ NETWORK=testnet docker-compose up
```

3. Query the API :tada:

```
$ curl http://localhost:8100/api/txs/last 
```

For more information, have a look at the [Wiki :book:](https://github.com/input-output-hk/cardano-rest/wiki).

## How to install (Linux / Mac OS / Docker)

### Docker (recommended)

See [Using Docker](https://github.com/input-output-hk/cardano-rest/wiki/Docker).

### From Binaries 

See assets available for each [release](https://github.com/input-output-hk/cardano-rest/releases).

## Documentation

- [API Documentation (cardano-explorer-api)](https://input-output-hk.github.io/cardano-rest/explorer-api)
- [API Documentation (cardano-submit-api)](https://input-output-hk.github.io/cardano-rest/submit-api)

<hr/>

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-rest/blob/master/LICENSE"><img src="https://img.shields.io/github/license/input-output-hk/cardano-rest.svg?style=for-the-badge" /></a>
</p>
