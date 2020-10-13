# Cardano Rest Tests

Run a particular test: 
```
mvn clean test -Dsurefire.suiteXmlFiles=<PATH_TO_TESTFILE.XML>
```

Run all performance tests:
```
mvn gatling:test -Dhost={HostName}
```

Run a specific performance test:
```
mvn gatling:test -Dhost={HostName} -Dgatling.SimulationClass={PathToTestClass}
```

## Reports

To access the functional test report, this command will set up allure reports at http://localhost:8080:
```
allure serve target/allure-results -p 8080
```

Gatling reports are stored in `/target/gatling/{GatlingTestRun}/index.html`

To install Allure: 
```
curl -o allure-2.6.0.tgz -Ls https://dl.bintray.com/qameta/generic/io/qameta/allure/allure/2.6.0/allure-2.6.0.tgz 
sudo tar -zxvf allure-2.6.0.tgz -C /opt/   
sudo ln -s /opt/allure-2.6.0/bin/allure /usr/bin/allure  
allure --version
```

## Docker

You can run the tests by running this command. Edit the `Dockerfile` to pick which tests are ran:
```
docker-compose up --build rest-tests
```

The Allure report (which covers functional tests) can be found here:
```
http://localhost:80/
```

Performance reports can be found here:
```
addresses/summary/{address} =>         http://localhost/gatling/addressessummaryaddresssimulation	
block/{blockhash}/address/{address} => http://localhost/gatling/blockblockhashaddressaddresssimulation	

blocks/pages =>                        http://localhost/gatling/blockspagessimulation			
blocks/pages/total =>                  http://localhost/gatling/blockspagestotalsimulation		
blocks/summary/{blockhash} =>          http://localhost/gatling/blockssummaryblockhashsimulation	
blocks/txs/{blockhash} =>              http://localhost/gatling/blockstxsblockhashsimulation	

epochs/{epoch} =>                      http://localhost/gatling/epochsepochsimulation			
epochs/{epoch}/slots/{slot} =>         http://localhost/gatling/epochsepochslotsslotsimulation		

genesis/address =>                     http://localhost/gatling/genesisaddresssimulation
genesis/summary =>                     http://localhost/gatling/genesissummarysimulation
genesis/address/pages/total            http://localhost/gatling/genesisaddresspagestotalsimulation
supply/ada =>                          http://localhost/gatling/supplyadasimulation

txs/summary/{tx} =>                    http://localhost/gatling/txssummarytxsimulation
txs/last =>                            http://localhost/gatling/txslastsimulation
stats/txs =>                           http://localhost/gatling/statstxssimulation
```

## How does the project work? 

The tests are ran as part of the docker container so that I can ensure they work as part of CI. The results are hosted on an nginx web server which is accessed through port 80. More details can be found in the src readme.

### Configuration

`config.properties` contains the network and performance testing parameters. You can choose to run with `testnet` or `mainnet` data, and any sort of customised performance test you might want. Note: be careful you don't DoS `mainnet`! The default network is `testnet` with low performance parameters. 

### Test data

Test data is randomly selected from data files in `resources/data/<network>`. If you want to test specific `addresses`, `blocks`, or `transactions`, you can replace the values in those files. `Epoch` and `slot` values are randomly selected from a range of ints suitable for Byron and Shelley, and can be found in the `DataStore.scala` file. 
