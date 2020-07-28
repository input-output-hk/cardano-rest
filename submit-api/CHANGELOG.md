# Changelog

## 2.1.1 -- July 2020

 * Update dependencies for compatibility with `cardano-node` 1.18.0.

## 2.1.0 -- July 2020

 * Fixing a cabal warning caused by flags in the wrong place.
 * Adding a '--random-port' flag..
 * You can now specify the webserver settings on the command line.
 * Add support for multiple protocol modes, defaulting to Cardano
 * Internal refactoring.

## 2.0.0 -- March 2020

* Moved 'cardano-explorer-api' into its own repository
* Renamed service and build artifacts
* Add swagger documentation for cardano-submit-api
* API **breaking** changes:
  * returns 400 Bad Request on errors, and 202 for successful submission.
  * returns plain text transaction id on success, and plain text error on errors (cf API documentation)

## 1.3.0 -- January 2020

* API change: require content-type `application/cbor` for posted transactions.
* API change: require raw transaction binary format for posted transactions.
* Add more specific error message when posted transaction is hex encoded.
* Include example testnet configuration and update README on using it.
* Documentation for submission API and how to generate example transactions.
* Update dependencies to latest versions.
* Docker image: log all runit services to stdout
* Initial documentation on how to use build and run the components in docker

## 1.2.? -- ?

* Require content-type `application/cbor` for posted transaction.
* Add more specific error message when posted transaction is hex encoded.

## 1.2.2 -- January 2020

* Update dependencies to latest versions.
* Service added to docker files.

## 1.2.1 -- January 2020

* Update dependencies to latest versions.
* Improve logging of tx submit responses.
* Add a README.
* Add QA document on how to test the component.

## 1.2.0 -- December 2019

* First release of the Tx Submission endpoint
