# explorer-api

The following requests are made on local instances connected to the Cardano TestNet. It is also important to note that many GraphQL queries
below are in reality much more flexible and capable than the queries given in examples. 

!!! tip
    Any GraphQL query can be ran in the Cardano GraphQL playground which is accessible on http://localhost:3100/graphql (assuming you've already started a local instance of cardano-graphql with the default configuration.

## Blocks

#### [/api/blocks/pages](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_blocksPages)

=== "cardano-rest"

    ```console
    $ curl "https://localhost:8090/api/blocks/pages?page=221038"
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
        "Right": [
            221038,
            [
                {
                    "cbeEpoch": 106,
                    "cbeSlot": 306880,
                    "cbeBlkHeight": 2210372,
                    "cbeBlkHash": "904f180c9271d96b6086623daa402e9f033a05ecf1bf35f9f5508b4584b6fed8",
                    "cbeTimeIssued": 1610098496,
                    "cbeTxNum": 0,
                    "cbeTotalSent": {
                        "getCoin": "0"
                    },
                    "cbeSize": 3,
                    "cbeBlockLead": "6df3e1b4b8a84c63c805076a85e5aa00924997a4eae85fddf0aee3ca",
                    "cbeFees": {
                        "getCoin": "0"
                    }
                },
                {
                    "cbeEpoch": 106,
                    "cbeSlot": 306800,
                    "cbeBlkHeight": 2210371,
                    "cbeBlkHash": "8c262185505782726c09d68cb0b84aab12f84117cb554b25ab2212d69eaed083",
                    "cbeTimeIssued": 1610098416,
                    "cbeTxNum": 0,
                    "cbeTotalSent": {
                        "getCoin": "0"
                    },
                    "cbeSize": 3,
                    "cbeBlockLead": "b3873a254459f506e47b9a252ee7912e538b364447f31576a170db65",
                    "cbeFees": {
                        "getCoin": "0"
                    }
                }
            ]
        ]
    }
    ```
    </details>

=== "cardano-graphql"

    **query.graphql**
    ```graphql
    {
        blocks(limit: 2, where: { epoch: { number: { _eq: 86 } } }) {
            slotNo,
            slotInEpoch,
            number,
            hash,
            forgedAt,
            transactionsCount,
            transactions_aggregate { aggregate { sum { totalOutput } } },
            slotLeader { hash },
            size,
            fees,
        }
    }
    ```

    ```
    $ curl -X POST http://localhost:3100/graphql \ 
        -H "Content-Type: application/json" \
        -d "{ \"query\": \"$(cat query.graphql | tr -d '\n' )\" }"
    ```


    <details>
      <summary>see JSON response</summary>
    ```json
    {
        "data": {
            "blocks": [
                {
                    "slotNo": 7214371,
                    "slotInEpoch": 431971,
                    "number": 1871955,
                    "hash": "1fd784ef1814fd3e1bdf35c9cd9966ed3d92ba36b68e91504cf414493a657da6",
                    "forgedAt": "2020-10-01T20:19:47Z",
                    "transactionsCount": "1",
                    "transactions_aggregate": {
                        "aggregate": {
                            "sum": {
                                "totalOutput": "9825611"
                            }
                        }
                    },
                    "slotLeader": {
                        "hash": "b3873a254459f506e47b9a252ee7912e538b364447f31576a170db65"
                    },
                    "size": 290,
                    "fees": 174389
                },
                {
                    "slotNo": 7214363,
                    "slotInEpoch": 431963,
                    "number": 1871954,
                    "hash": "94453b8518843387c02921906bac68f70da6f3604eb9f920daa7a758ce2f018e",
                    "forgedAt": "2020-10-01T20:19:39Z",
                    "transactionsCount": "1",
                    "transactions_aggregate": {
                        "aggregate": {
                            "sum": {
                                "totalOutput": "9825611"
                            }
                        }
                    },
                    "slotLeader": {
                        "hash": "dc27de163b122622dc64176236656c6b9622f96f3d2eea74f89414ae"
                    },
                    "size": 290,
                    "fees": 174389
                }
            ]
        }
    }
    ```
    </details>

=== "cardano-rosetta"

    TODO

#### /api/blocks/pages/total

#### /api/blocks/summary/{blockHash}

#### /api/blocks/txs/{blockHash}

## Transactions

#### /api/txs/last
 
#### /api/txs/summary/{txId}

#### /api/stats/txs

## Addresses

#### /api/addresses/summary/{address}

#### /api/block/{blockHash}/address/{address}

## Epochs 

#### /api/epochs/{epoch}

#### /api/epochs/{epoch}/{slot}

## Genesis

#### /api/genesis/summary

#### /api/genesis/address/pages/total

#### /api/genesis/address

#### /api/supply/ada
