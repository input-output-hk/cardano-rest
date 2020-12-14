# Changelog

## 3.1.1 -- December 2020

* ø (patch in submit-api).

## 3.1.0 -- December 2020

* Fix an internal error with 'InnerJoin' requests on block's transaction summary.
* Fix inconsistency in the OpenAPI 2.0 specification (and consequently, the documentation).
* Upgrade dependencies to more recent version of the Cardano eco-system (cardano-db-sync-7.1.0 & cardano-node 1.24.2)

## 3.0.0 -- October 2020

 * Upgrade dependencies to more recent version of the Cardano eco-system (cardano-db-sync-6.0.0) 

## 2.1.3 -- August 2020

 * Fix bug regarding Shelley addresses not being decoded correctly (#75)
 * Fix block number overflow (#79)

## 2.1.2 -- July 2020

 * Fix bug with cbeSlot (#77)
 * Update dependencies for compatibility with `cardano-db-sync` 3.1.0.

## 2.1.1 -- July 2020

 * Update dependencies for compatibility with `cardano-node` 1.18.0.

## 2.1.0 -- July 2020

 * You can now specify the webserver settings on the command line.
 * Moving some sharable code into a cardano-rest-common/ package.
 * Tidyied the way the explorer prints its connection details on startup.
 * Adding a '--random-port' flag..
 * Internal refactoring.
 * Bugfixes for cardano-explorer-api-validate.
 * Fixed defaults for the command line flags.

## 2.0.0 -- March 2020

* Moved 'cardano-explorer-api' into its own repository
* Renamed service and build artifacts

## 1.3.0 -- January 2020

* Update dependencies to latest versions.
* Docker image: log all runit services to stdout
* Initial documentation on how to use build and run the components in docker

## 1.2.2 -- January 2020

* Swagger docs https://input-output-hk.github.io/cardano-explorer/
* Fix /api/blocks/txs/{blkHash} endpoint (#195)
* Fix Ada/Lovelace denomination bug (#197)
* Fix JSON rendering for addresses to match old API
* Add validation for genesis address paging (#219)
* Add additional tests (#222, #227)
* Update dependencies to latest versions.

## 1.2.1 -- January 2020

* Update dependencies to latest versions.

## 1.2.0 -- December 2019

* Update dependencies to latest versions.

## 1.1.0 -- December 2019

* Renamed from cardano-explorer to cardano-explorer-webapi (#193).

* Remove unused/unsupported endpoints (#198)

* Provide more info in /api/txs/summary/{txHash} endpoint (#174).

  Specifically, for each transaction replace '(address, coin)' with
  a struct that also contains the transaction '(hash, index)' pair.

* Provide more info about transactions in some endpoints (#174, #131).

  Specifically:
    * /api/blocks/txs/{blockHash}
    * /api/txs/summary/{txHash}

* Add ChainTip info to address endpoints (#177, #130)

* Run all transactions at an isolation level of Serializable (#189, #133)

* Document atomicity of API interactions

* Fix validation error (#191)

* Add additional tests to validiate against the old API (#190)

## 1.0.0 -- November 2019

* First release of new explorer API server based on new cardano-explorer-node.
* New modular design, no longer integrated with the cardano-sl node.
* Gets all data from a local PostgreSQL DB.
* Compatible with the old explorer HTTP API and old web frontend.
* Some small compatible API extensions
