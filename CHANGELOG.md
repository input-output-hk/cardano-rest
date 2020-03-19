# Changelog

## [2.0.0] -- 2020-03-19

### Added 

* Added swagger documentation for cardano-submit-api

### Changed

* Moved 'cardano-explorer' component into their own repository
* Renamed service and build artifacts
* API **breaking** changes: 
  * Now returns 400 Bad Request on errors, and 202 for successful submission.
  * Now returns plain text transaction id on success, and plain text error on errors (cf API documentation)

### Removed

* Obsolete instructions 
