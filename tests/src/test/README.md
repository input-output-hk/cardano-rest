# Tests

## Functional

This package contains functional tests against `cardano-rest`. 

### Smoke Tests

Tests to run basic response and schema validation against all endpoints. Estimated time to run ~20s. 

### Data Validation

Tests to check expected values returned against hardcoded data. The values set in `tests_functional-data-validation.xml`
are used as parameters.

### Data Intensive

Tests to run schema validation against all endpoints but with `n` values. This takes some time to complete because of
a 1s delay between each call in order to prevent accidental load testing!

### Oracle

Oracle tests compare two versions of the RestAPI to ensure Data Parity. Note, this won't identify legacy errors but will identify regressions. 
Known breaking changes will cause these tests to fail, and they should be ignored or updated accordingly. You can set the
Oracle in `config.properties`.

## Simulations

This package contains non-functional performance tests against `cardano-rest`.

### Performance

Gatling performance tests are ran against each endpoint. The configuration for these tests can be found in `config.properties`.

## Data Used

Data is available for both testnet and mainnet networks. DBSync was queried for a maximum of 2500 random values which 
are stored in the `resources` folder. You can specify the network in the `config.properties` file or get more test data using the
example SQL queries (`resources/example_queries_to_get_data.sql`). 

## Default Configuration

- host=https://explorer.cardano-testnet.iohkdev.io/api/
- oracle=https://explorer.cardano-testnet.iohkdev.io/api/
- network=testnet
- pauseBetweenTests=5
- pauseBetweenRequests=5
- startingUsers=1
- maximumUsers=10
- timeFrameToIncreaseUsers=15
- maxTestDuration=30
