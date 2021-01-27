# explorer-api

The following requests are made on the Cardano TestNet. It is also important to note that many GraphQL queries
below are in reality much more flexible and capable than the queries given in the examples.

!!! tip
    Any GraphQL query can be run interactively in the Cardano GraphQL playground, which is accessible [here](https://cardano-graphql-testnet.daedalus-operations.com/) online.

## Blocks

#### [/api/blocks/pages](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_blocksPages)

Get the list of blocks, contained in pages.

=== "cardano-rest"

    ```console
    $ curl "https://explorer.cardano-testnet.iohkdev.io/api/blocks/pages?page=221038"
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "Right": [
        222550,
        [
          {
            "cbeEpoch": 106,
            "cbeSlot": 307143,
            "cbeBlkHeight": 2210380,
            "cbeBlkHash": "42c7121010f7eede53e07ef0bea954f1704155567ac4b29bcc106ab9d70bdd44",
            "cbeTimeIssued": 1610098759,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 3,
            "cbeBlockLead": "43cc785cf7817a8c79ca7b569fefec98ca22323b0f1051d3bb8bf9b9",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 106,
            "cbeSlot": 307120,
            "cbeBlkHeight": 2210379,
            "cbeBlkHash": "59d98df243ee10436f8946f7d9fa3463ad32648f454f661dbf0ac1681e0414d3",
            "cbeTimeIssued": 1610098736,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 3,
            "cbeBlockLead": "bf07107c6f632de95e34af7e009d2aafa19916c7ba89b944fbedcd72",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 106,
            "cbeSlot": 307040,
            "cbeBlkHeight": 2210378,
            "cbeBlkHash": "7afbe86ba5447d81fba9ca94526fe3d89addaca5164bcde1de39f8853e601a33",
            "cbeTimeIssued": 1610098656,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 3,
            "cbeBlockLead": "de665a71064706f946030505eae950583f08c316f0f58997961092b1",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 106,
            "cbeSlot": 307030,
            "cbeBlkHeight": 2210377,
            "cbeBlkHash": "be7cd8f8b5ae388b612f9a2f944f1b25836b5dd451cdac83bbbd7567997ce403",
            "cbeTimeIssued": 1610098646,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 3,
            "cbeBlockLead": "9f36c4c67b76a8cea05a1684f8feaa64711f4c0053fe039978b203af",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 106,
            "cbeSlot": 306963,
            "cbeBlkHeight": 2210376,
            "cbeBlkHash": "82785f52a832d11352bdbbea6e39807488cc0b20560aab2235003085d8909ad2",
            "cbeTimeIssued": 1610098579,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 3,
            "cbeBlockLead": "1ce0eaa92a03f854e22a5acc42fde7a4fd1d48c27fbec77162d45c12",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 106,
            "cbeSlot": 306960,
            "cbeBlkHeight": 2210375,
            "cbeBlkHash": "f88e1b71f9e710a9b630e57283271decf4410794af2205c04576db7958c84d2e",
            "cbeTimeIssued": 1610098576,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 3,
            "cbeBlockLead": "bd5933d3c5417f17a64c7214711a26abc3bc03e2c90dc1bb38e0c39f",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 106,
            "cbeSlot": 306958,
            "cbeBlkHeight": 2210374,
            "cbeBlkHash": "a835446a0eeb40d437b03964ab5907e7c4145bdd034b549a8b05a2e740090301",
            "cbeTimeIssued": 1610098574,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 3,
            "cbeBlockLead": "8f49108228169c7c766d5db245ed834ac8c32b0ec7f06c7a3e70e50d",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 106,
            "cbeSlot": 306911,
            "cbeBlkHeight": 2210373,
            "cbeBlkHash": "852e54e98e7e498a1edf767faaea4cfdcea714b2931297f7adad1d5a55040d5b",
            "cbeTimeIssued": 1610098527,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 3,
            "cbeBlockLead": "d0c94502e24095349ccc813e0e9ff304b4680a9b347006095960fb51",
            "cbeFees": {
              "getCoin": "0"
            }
          },
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
    query getBlocks($limit: Int!, $epoch: Int!) {
      blocks(limit: $limit, where: { epoch: { number: { _eq: $epoch } } }) {
        slotNo # cbeSlot
        slotInEpoch # cbeEpoch
        number # cbeBlkHeight
        hash # cbeBlkHash
        forgedAt # cbeTimeIssued
        transactionsCount # cbeTxNum
        transactions_aggregate {
          aggregate {
            sum {
              totalOutput # cbeTotalSent
            }
          }
        }
        slotLeader {
          hash # cbeBlockLead
        }
        size # cbeSize
        fees # cbeFees
      }
    }
    ```
    **variables.json**
    ```json
    {
      "limit": 2,
      "epoch": 86
    }
    ```

    ```console
    $ curl 'https://cardano-graphql-testnet.daedalus-operations.com/' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: https://cardano-graphql-testnet.daedalus-operations.com' --data-binary '{"query":"    query getBlocks($limit: Int!, $epoch: Int!) {\n      blocks(limit: $limit, where: { epoch: { number: { _eq: $epoch } } }) {\n        slotNo # cbeSlot\n        slotInEpoch # cbeEpoch\n        number # cbeBlkHeight\n        hash # cbeBlkHash\n        forgedAt # cbeTimeIssued\n        transactionsCount # cbeTxNum\n        transactions_aggregate {\n          aggregate {\n            sum {\n              totalOutput # cbeTotalSent\n            }\n          }\n        }\n        slotLeader {\n          hash # cbeBlockLead\n        }\n        size # cbeSize\n        fees # cbeFees\n      }\n    }\n","variables":{"limit":2,"epoch":86}}' --compressed
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

!!! info
    Neither GraphQL, nor Rosetta are paged, but instead allow to set a limit and an offset to queries.

#### [/api/blocks/pages/total](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_blocksPagesTotal)

Get the list of total pages.

!!! info
    Neither GraphQL, nor Rosetta are paged, but instead allow to set a limit and an offset to queries.

#### [/api/blocks/summary/{blockHash}](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_blocksSummary)

Get block's summary information.

=== "cardano-rest"

    ```console
    $ https://explorer.cardano-testnet.iohkdev.io/api/blocks/summary/1fd784ef1814fd3e1bdf35c9cd9966ed3d92ba36b68e91504cf414493a657da6
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "Right": {
        "cbsEntry": {
          "cbeEpoch": 86,
          "cbeSlot": 431971,
          "cbeBlkHeight": 1871955,
          "cbeBlkHash": "1fd784ef1814fd3e1bdf35c9cd9966ed3d92ba36b68e91504cf414493a657da6",
          "cbeTimeIssued": 1601583587,
          "cbeTxNum": 1,
          "cbeTotalSent": {
            "getCoin": "9825611"
          },
          "cbeSize": 290,
          "cbeBlockLead": "b3873a254459f506e47b9a252ee7912e538b364447f31576a170db65",
          "cbeFees": {
            "getCoin": "174389"
          }
        },
        "cbsPrevHash": "94453b8518843387c02921906bac68f70da6f3604eb9f920daa7a758ce2f018e",
        "cbsNextHash": "d5535fc0fb15875445a9565009aa9b99cd141f47f4ad7bf5e282c6361f6e7ddb",
        "cbsMerkleRoot": ""
      }
    }
    ```
    </details>

=== "cardano-graphql"

    **query.graphql**
    ```graphql
    query getBlock($hash: Hash32Hex!) {
      blocks(limit: 1, where: { hash: { _eq: $hash } }) {
        slotInEpoch # cbeEpoch
        slotNo # cbeSlot
        number # cbeBlkHeight
        hash # cbeBlkHash
        forgedAt # cbeTimeIssued
        transactionsCount # cbeTxNum
        transactions_aggregate {
          aggregate {
            sum {
              totalOutput # cbeTotalSent
            }
          }
        }
        size # cbeSize
        slotLeader {
          hash # cbeBlockLead
        }
        fees # cbeFees
        previousBlock {
          hash # cbsPrevHash
        }
        nextBlock {
          hash # cbsNextHash
        }
        merkelRoot # cbsMerkleRoot
      }
    }
    ```
    **variables.json**
    ```json
    {
      "hash": "1fd784ef1814fd3e1bdf35c9cd9966ed3d92ba36b68e91504cf414493a657da6"
    }
    ```

    ```console
    $ curl 'https://cardano-graphql-testnet.daedalus-operations.com/' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: https://cardano-graphql-testnet.daedalus-operations.com' --data-binary '{"query":"query getBlock($hash: Hash32Hex!) {\n  blocks(limit: 1, where: { hash: { _eq: $hash } }) {\n    slotInEpoch # cbeEpoch\n    slotNo # cbeSlot\n    number # cbeBlkHeight\n    hash # cbeBlkHash\n    forgedAt # cbeTimeIssued\n    transactionsCount # cbeTxNum\n    transactions_aggregate {\n      aggregate {\n        sum {\n          totalOutput # cbeTotalSent\n        }\n      }\n    }\n    size # cbeSize\n    slotLeader {\n      hash # cbeBlockLead\n    }\n    fees # cbeFees\n    previousBlock {\n      hash # cbsPrevHash\n    }\n    nextBlock {\n      hash # cbsNextHash\n    }\n    merkelRoot # cbsMerkleRoot\n  }\n}","variables":{"hash":"1fd784ef1814fd3e1bdf35c9cd9966ed3d92ba36b68e91504cf414493a657da6"}}' --compressed
    ```


    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "data": {
        "blocks": [
          {
            "slotInEpoch": 431971,
            "slotNo": 7214371,
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
            "size": 290,
            "slotLeader": {
              "hash": "b3873a254459f506e47b9a252ee7912e538b364447f31576a170db65"
            },
            "fees": 174389,
            "previousBlock": {
              "hash": "94453b8518843387c02921906bac68f70da6f3604eb9f920daa7a758ce2f018e"
            },
            "nextBlock": {
              "hash": "d5535fc0fb15875445a9565009aa9b99cd141f47f4ad7bf5e282c6361f6e7ddb"
            },
            "merkelRoot": null
          }
        ]
      }
    }
    ```
    </details>

=== "cardano-rosetta"

 ```console
    $ curl -X POST 'http://localhost:8080/block' -H "Content-Type: application/json" -d '{ "network_identifier": {"blockchain": "cardano", "network": "mainnet" }, "metadata": {}, "block_identifier": {"index": "5264122" }}'
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
{
  "block": {
    "block_identifier": {
      "index": 5264122,
      "hash": "b5426334221805b3c161ec07b02722728ced7b5c38a9cc60962e819620ecbf9a"
    },
    "parent_block_identifier": {
      "index": 5264121,
      "hash": "ff677921d61672d2fb14025f6d8d8b1f93d89266986f045d48ddc674459134ea"
    },
    "timestamp": 1611751783000,
    "transactions": [
      {
        "transaction_identifier": {
          "hash": "2d3d33d26c87d3e8ef0bc6cb25be3fb34f4db43bf23d7c624955a82cd8815772"
        },
        "operations": [
          {
            "operation_identifier": {
              "index": 0
            },
            "type": "input",
            "status": "success",
            "account": {
              "address": "DdzFFzCqrhsqKd92VGNM9Ts1Ms62J2FaSRmf8t1bQa1VugDmcUJzeU8TRFnGDDUR6f1m9VaJJG1GfnzxVjKGBbBAVGT9sPBseREYzP3E"
            },
            "amount": {
              "value": "-4998821898",
              "currency": {
                "symbol": "ADA",
                "decimals": 6
              }
            },
            "coin_change": {
              "coin_identifier": {
                "identifier": "002da5ef961fd9d826332db916fe7de5e198b8a5f00f9c535f9c329bb14f01e8:0"
              },
              "coin_action": "coin_spent"
            }
          },
 
            "coin_change": {
              "coin_identifier": {
                "identifier": "03bd6f6b65f4020b0d35a24b79809e1d5e9db11d67260aa465f2bb281912d5ea:0"
              },
              "coin_action": "coin_spent"
            }
          },
  
    ],
    "metadata": {
      "transactionsCount": 6,
      "createdBy": "ShelleyGenesis-1d4f2e1fda43070d",
      "size": 9559,
      "epochNo": 244,
      "slotNo": 20185492
    }
  }
}
    ```
    </details>

#### [/api/blocks/txs/{blockHash}](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_blocksTxs)

Get brief information about transactions.

=== "cardano-rest"

    ```console
    $ https://explorer.cardano-testnet.iohkdev.io/api/blocks/txs/1fd784ef1814fd3e1bdf35c9cd9966ed3d92ba36b68e91504cf414493a657da6
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "Right": [
        {
          "ctbId": "db5d4cce4b87434fd142409f1fd1680769ee2669989883c763c292952793da31",
          "ctbTimeIssued": 1601583587,
          "ctbInputs": [
            {
              "ctaAddress": "addr_test1qpdmel32d5hcqlna8fdpsypmfn8ggfcarmlurzaa6yxflcywsfrv2aayw5akn8pstwxxdrpnr27yact8t7kvpg9ggtjqpkqf2t",
              "ctaAmount": {
                "getCoin": "10000000"
              },
              "ctaTxHash": "bd122e2a7440c6710e2bd3350cce18621acb1c0d58d4c066878a60341761f71c",
              "ctaTxIndex": 0
            }
          ],
          "ctbOutputs": [
            {
              "ctaAddress": "addr_test1qpw39hnntn0ls8vckussxz39zh8ev8ne29h3965dhd5txzg0ld2hh665wg0vpy9u0v9fkp5pqmfk5703n7sr4srtvzas7932tv",
              "ctaAmount": {
                "getCoin": "2000000"
              },
              "ctaTxHash": "db5d4cce4b87434fd142409f1fd1680769ee2669989883c763c292952793da31",
              "ctaTxIndex": 0
            },
            {
              "ctaAddress": "addr_test1qpdmel32d5hcqlna8fdpsypmfn8ggfcarmlurzaa6yxflcywsfrv2aayw5akn8pstwxxdrpnr27yact8t7kvpg9ggtjqpkqf2t",
              "ctaAmount": {
                "getCoin": "7825611"
              },
              "ctaTxHash": "db5d4cce4b87434fd142409f1fd1680769ee2669989883c763c292952793da31",
              "ctaTxIndex": 1
            }
          ],
          "ctbInputSum": {
            "getCoin": "10000000"
          },
          "ctbOutputSum": {
            "getCoin": "9825611"
          },
          "ctbFees": {
            "getCoin": "174389"
          }
        }
      ]
    }
    ```
    </details>

=== "cardano-graphql"

    **query.graphql**
    ```graphql
    query getBlockTxs($hash: Hash32Hex!) {
      blocks(limit: 1, where: { hash: { _eq: $hash } }) {
        transactions {
          hash # ctbId
          includedAt # ctbTimeIssued
          fee # ctbFees
          inputs {
            # ctbInputs
            address # ctaAddress
            value # ctaAmount
            sourceTxHash # ctaTxHash
            sourceTxIndex # ctaTxIndex
          }
          inputs_aggregate {
            aggregate {
              sum {
                value # ctbInputSum
              }
            }
          }
          outputs { # ctbOutputs
            address # ctaAddress
            index # ctaTxIndex
            value # ctaAmount
            transaction {
              hash # ctaTxHash
            }
          }
          outputs_aggregate {
            aggregate {
              sum {
                value # ctbOutputSum
              }
            }
          }
        }
      }
    }
    ```
    **variables.json**
    ```json
    {
      "hash": "1fd784ef1814fd3e1bdf35c9cd9966ed3d92ba36b68e91504cf414493a657da6"
    }
    ```

    ```console
    $ curl 'https://cardano-graphql-testnet.daedalus-operations.com/' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: https://cardano-graphql-testnet.daedalus-operations.com' --data-binary '{"query":"query getBlockTxs($hash: Hash32Hex!) {\n  blocks(limit: 1, where: { hash: { _eq: $hash } }) {\n    transactions {\n      hash # ctbId\n      includedAt # ctbTimeIssued\n      fee # ctbFees\n      inputs {\n        # ctbInputs\n        address # ctaAddress\n        value # ctaAmount\n        sourceTxHash # ctaTxHash\n        sourceTxIndex # ctaTxIndex\n      }\n      inputs_aggregate {\n        aggregate {\n          sum {\n            value # ctbInputSum\n          }\n        }\n      }\n      outputs { # ctbOutputs\n        address # ctaAddress\n        index # ctaTxIndex\n        value # ctaAmount\n        transaction {\n          hash # ctaTxHash\n        }\n      }\n      outputs_aggregate {\n        aggregate {\n          sum {\n            value # ctbOutputSum\n          }\n        }\n      }\n    }\n  }\n}","variables":{"hash":"1fd784ef1814fd3e1bdf35c9cd9966ed3d92ba36b68e91504cf414493a657da6"}}' --compressed
    ```


    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "data": {
        "blocks": [
          {
            "transactions": [
              {
                "hash": "db5d4cce4b87434fd142409f1fd1680769ee2669989883c763c292952793da31",
                "includedAt": "2020-10-01T20:19:47Z",
                "fee": 174389,
                "inputs": [
                  {
                    "address": "addr_test1qpdmel32d5hcqlna8fdpsypmfn8ggfcarmlurzaa6yxflcywsfrv2aayw5akn8pstwxxdrpnr27yact8t7kvpg9ggtjqpkqf2t",
                    "value": "10000000",
                    "sourceTxHash": "bd122e2a7440c6710e2bd3350cce18621acb1c0d58d4c066878a60341761f71c",
                    "sourceTxIndex": 0
                  }
                ],
                "inputs_aggregate": {
                  "aggregate": {
                    "sum": {
                      "value": "10000000"
                    }
                  }
                },
                "outputs": [
                  {
                    "address": "addr_test1qpw39hnntn0ls8vckussxz39zh8ev8ne29h3965dhd5txzg0ld2hh665wg0vpy9u0v9fkp5pqmfk5703n7sr4srtvzas7932tv",
                    "index": 0,
                    "value": "2000000",
                    "transaction": {
                      "hash": "db5d4cce4b87434fd142409f1fd1680769ee2669989883c763c292952793da31"
                    }
                  },
                  {
                    "address": "addr_test1qpdmel32d5hcqlna8fdpsypmfn8ggfcarmlurzaa6yxflcywsfrv2aayw5akn8pstwxxdrpnr27yact8t7kvpg9ggtjqpkqf2t",
                    "index": 1,
                    "value": "7825611",
                    "transaction": {
                      "hash": "db5d4cce4b87434fd142409f1fd1680769ee2669989883c763c292952793da31"
                    }
                  }
                ],
                "outputs_aggregate": {
                  "aggregate": {
                    "sum": {
                      "value": "9825611"
                    }
                  }
                }
              }
            ]
          }
        ]
      }
    }
    ```
    </details>

=== "cardano-rosetta"

```console
    $ curl -X POST 'http://localhost:8080/block' -H "Content-Type: application/json" -d '{ "network_identifier": {"blockchain": "cardano", "network": "mainnet" }, "metadata": {}, "block_identifier": {"index": "5264122" }}'
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
{
  "block": {
    "block_identifier": {
      "index": 5264122,
      "hash": "b5426334221805b3c161ec07b02722728ced7b5c38a9cc60962e819620ecbf9a"
    },
    "parent_block_identifier": {
      "index": 5264121,
      "hash": "ff677921d61672d2fb14025f6d8d8b1f93d89266986f045d48ddc674459134ea"
    },
    "timestamp": 1611751783000,
    "transactions": [
      {
        "transaction_identifier": {
          "hash": "2d3d33d26c87d3e8ef0bc6cb25be3fb34f4db43bf23d7c624955a82cd8815772"
        },
        "operations": [
          {
            "operation_identifier": {
              "index": 0
            },
            "type": "input",
            "status": "success",
            "account": {
              "address": "DdzFFzCqrhsqKd92VGNM9Ts1Ms62J2FaSRmf8t1bQa1VugDmcUJzeU8TRFnGDDUR6f1m9VaJJG1GfnzxVjKGBbBAVGT9sPBseREYzP3E"
            },
            "amount": {
              "value": "-4998821898",
              "currency": {
                "symbol": "ADA",
                "decimals": 6
              }
            },
            "coin_change": {
              "coin_identifier": {
                "identifier": "002da5ef961fd9d826332db916fe7de5e198b8a5f00f9c535f9c329bb14f01e8:0"
              },
              "coin_action": "coin_spent"
            }
          },
 
            "coin_change": {
              "coin_identifier": {
                "identifier": "03bd6f6b65f4020b0d35a24b79809e1d5e9db11d67260aa465f2bb281912d5ea:0"
              },
              "coin_action": "coin_spent"
            }
          },
  
    ],
    "metadata": {
      "transactionsCount": 6,
      "createdBy": "ShelleyGenesis-1d4f2e1fda43070d",
      "size": 9559,
      "epochNo": 244,
      "slotNo": 20185492
    }
  }
}
    ```
    </details>



## Transactions

#### [/api/txs/last](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_txsLast)

Get information about the N latest transactions.

=== "cardano-rest"

    ```console
    $ https://explorer.cardano-testnet.iohkdev.io/api/txs/last
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "Right": [
        {
          "cteId": "382a5274ebf102910c6c923a8b11f108e79ecedb5d7433cd0dd15a8a443f0fa5",
          "cteTimeIssued": 1610595750,
          "cteAmount": {
            "getCoin": "979116175"
          }
        },
        {
          "cteId": "99ab43fb82ce38864e29af00cd2806d2abc7d72a99ecc1d1e3a3b52ecc43c31f",
          "cteTimeIssued": 1610595536,
          "cteAmount": {
            "getCoin": "989292940"
          }
        },
        {
          "cteId": "1e98525288b1330e6e873f1e595b2d090e1f74e78898d9284a8c3aced426d5f9",
          "cteTimeIssued": 1610595376,
          "cteAmount": {
            "getCoin": "994469705"
          }
        },
        {
          "cteId": "6268dc17740befdeffb9d9d952afa01b831c605bc418903c7595f20fdad01dd6",
          "cteTimeIssued": 1610594096,
          "cteAmount": {
            "getCoin": "997646470"
          }
        },
        {
          "cteId": "1dcdd2827be7eb45e884b093a783b05de8f0b578ecc9e9eb4d636a76e824def7",
          "cteTimeIssued": 1610593638,
          "cteAmount": {
            "getCoin": "999823235"
          }
        },
        {
          "cteId": "f7c08043890ef4fbd425764fb5263eb29dffe0f4f3e3cce2ec18b856c41598e8",
          "cteTimeIssued": 1610591247,
          "cteAmount": {
            "getCoin": "3969639296"
          }
        },
        {
          "cteId": "ace78170121cca8f5e54b8f011743c03b17cb3d4af36868e6186342cc7920a16",
          "cteTimeIssued": 1610590736,
          "cteAmount": {
            "getCoin": "9983997133111"
          }
        },
        {
          "cteId": "e50623e7923bf8b796828a4f303516d0f9b46e3c50e91db07fa5e45e3e409fea",
          "cteTimeIssued": 1610583856,
          "cteAmount": {
            "getCoin": "1000000"
          }
        },
        {
          "cteId": "924f31ae1b54b62bda19474c317bd62cb1f9b847cd7b836ce5e2c0d9b7adec1e",
          "cteTimeIssued": 1610583241,
          "cteAmount": {
            "getCoin": "9988997980724"
          }
        },
        {
          "cteId": "f94095802cb727457bf61cbb619c329a1c06b608b47eadc844cb095342867738",
          "cteTimeIssued": 1610581581,
          "cteAmount": {
            "getCoin": "4820683"
          }
        },
        {
          "cteId": "d0eb47df3a808b8629ba8d57d606e1ea6e724c23911421eea5ce998a04712a93",
          "cteTimeIssued": 1610581535,
          "cteAmount": {
            "getCoin": "200128611870982"
          }
        },
        {
          "cteId": "233c7ac393e6edacef112ddc45ebd326d060aca31a6b538f114889072c0f119e",
          "cteTimeIssued": 1610581429,
          "cteAmount": {
            "getCoin": "100056607520356"
          }
        },
        {
          "cteId": "0a6c576975e53a1839e4094f6d4cc84cc81e86b7d1ad2ca203bb37210bef7208",
          "cteTimeIssued": 1610581215,
          "cteAmount": {
            "getCoin": "670178176"
          }
        },
        {
          "cteId": "10f0f282fa12ec88a19b442ae03525508b1b6eca66cdcb6cece8e153910c3160",
          "cteTimeIssued": 1610581136,
          "cteAmount": {
            "getCoin": "100072004528183"
          }
        },
        {
          "cteId": "53bf7f7cf10d4a165a7daa5708cde84ab005b50071b3535d0ce2a73d0f693f21",
          "cteTimeIssued": 1610580727,
          "cteAmount": {
            "getCoin": "9926467248113"
          }
        },
        {
          "cteId": "754f6604068cb457af7a468b27babcb28714789f0fdc75a78cafa91608acb7b9",
          "cteTimeIssued": 1610580096,
          "cteAmount": {
            "getCoin": "100056607691709"
          }
        },
        {
          "cteId": "0c3ada754aabab097627b185e85bc0d579155ab7faca84200b2fed08d687a93e",
          "cteTimeIssued": 1610580016,
          "cteAmount": {
            "getCoin": "1077645766086"
          }
        },
        {
          "cteId": "588ddaf2c6a5c1e15f31e282c589d4422e57083f28030da42d62420ab03085b4",
          "cteTimeIssued": 1610579536,
          "cteAmount": {
            "getCoin": "1000000"
          }
        },
        {
          "cteId": "58628178082d005faec360421cff55d31f786f22100586fdbda105da9e6ba03b",
          "cteTimeIssued": 1610579216,
          "cteAmount": {
            "getCoin": "1077645949891"
          }
        },
        {
          "cteId": "16e73c7f887825152831eaa656c496aa45f69c755bb6e03a3454555e0c790196",
          "cteTimeIssued": 1610575838,
          "cteAmount": {
            "getCoin": "591707773"
          }
        }
      ]
    }
    ```
    </details>

=== "cardano-graphql"

    **query.graphql**
    ```graphql
    query getTxs($limit: Int!) {
      transactions(limit: $limit, order_by: { includedAt: desc }) {
        hash # cteId
        includedAt # cteTimeIssued
        outputs_aggregate {
          aggregate {
            sum {
              value # cteAmount
            }
          }
        }
      }
    }
    ```

    **variables.json**
    ```json
    {
      "limit": 20
    }
    ```

    ```console
    $ curl 'https://cardano-graphql-testnet.daedalus-operations.com/' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: https://cardano-graphql-testnet.daedalus-operations.com' --data-binary '{"query":"query getTxs($limit: Int!) {\n  transactions(limit: $limit, order_by: { includedAt: desc }) {\n    hash # cteId\n    includedAt # cteTimeIssued\n    outputs_aggregate {\n      aggregate {\n        sum {\n          value # cteAmount\n        }\n      }\n    }\n  }\n}\n","variables":{"limit":20}}' --compressed
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "data": {
        "transactions": [
          {
            "hash": "382a5274ebf102910c6c923a8b11f108e79ecedb5d7433cd0dd15a8a443f0fa5",
            "includedAt": "2021-01-14T03:42:30Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "979116175"
                }
              }
            }
          },
          {
            "hash": "99ab43fb82ce38864e29af00cd2806d2abc7d72a99ecc1d1e3a3b52ecc43c31f",
            "includedAt": "2021-01-14T03:38:56Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "989292940"
                }
              }
            }
          },
          {
            "hash": "1e98525288b1330e6e873f1e595b2d090e1f74e78898d9284a8c3aced426d5f9",
            "includedAt": "2021-01-14T03:36:16Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "994469705"
                }
              }
            }
          },
          {
            "hash": "6268dc17740befdeffb9d9d952afa01b831c605bc418903c7595f20fdad01dd6",
            "includedAt": "2021-01-14T03:14:56Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "997646470"
                }
              }
            }
          },
          {
            "hash": "1dcdd2827be7eb45e884b093a783b05de8f0b578ecc9e9eb4d636a76e824def7",
            "includedAt": "2021-01-14T03:07:18Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "999823235"
                }
              }
            }
          },
          {
            "hash": "f7c08043890ef4fbd425764fb5263eb29dffe0f4f3e3cce2ec18b856c41598e8",
            "includedAt": "2021-01-14T02:27:27Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "3969639296"
                }
              }
            }
          },
          {
            "hash": "ace78170121cca8f5e54b8f011743c03b17cb3d4af36868e6186342cc7920a16",
            "includedAt": "2021-01-14T02:18:56Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "9983997133111"
                }
              }
            }
          },
          {
            "hash": "e50623e7923bf8b796828a4f303516d0f9b46e3c50e91db07fa5e45e3e409fea",
            "includedAt": "2021-01-14T00:24:16Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "1000000"
                }
              }
            }
          },
          {
            "hash": "924f31ae1b54b62bda19474c317bd62cb1f9b847cd7b836ce5e2c0d9b7adec1e",
            "includedAt": "2021-01-14T00:14:01Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "9988997980724"
                }
              }
            }
          },
          {
            "hash": "f94095802cb727457bf61cbb619c329a1c06b608b47eadc844cb095342867738",
            "includedAt": "2021-01-13T23:46:21Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "4820683"
                }
              }
            }
          },
          {
            "hash": "d0eb47df3a808b8629ba8d57d606e1ea6e724c23911421eea5ce998a04712a93",
            "includedAt": "2021-01-13T23:45:35Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "200128611870982"
                }
              }
            }
          },
          {
            "hash": "233c7ac393e6edacef112ddc45ebd326d060aca31a6b538f114889072c0f119e",
            "includedAt": "2021-01-13T23:43:49Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "100056607520356"
                }
              }
            }
          },
          {
            "hash": "0a6c576975e53a1839e4094f6d4cc84cc81e86b7d1ad2ca203bb37210bef7208",
            "includedAt": "2021-01-13T23:40:15Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "670178176"
                }
              }
            }
          },
          {
            "hash": "10f0f282fa12ec88a19b442ae03525508b1b6eca66cdcb6cece8e153910c3160",
            "includedAt": "2021-01-13T23:38:56Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "100072004528183"
                }
              }
            }
          },
          {
            "hash": "53bf7f7cf10d4a165a7daa5708cde84ab005b50071b3535d0ce2a73d0f693f21",
            "includedAt": "2021-01-13T23:32:07Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "9926467248113"
                }
              }
            }
          },
          {
            "hash": "754f6604068cb457af7a468b27babcb28714789f0fdc75a78cafa91608acb7b9",
            "includedAt": "2021-01-13T23:21:36Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "100056607691709"
                }
              }
            }
          },
          {
            "hash": "0c3ada754aabab097627b185e85bc0d579155ab7faca84200b2fed08d687a93e",
            "includedAt": "2021-01-13T23:20:16Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "1077645766086"
                }
              }
            }
          },
          {
            "hash": "588ddaf2c6a5c1e15f31e282c589d4422e57083f28030da42d62420ab03085b4",
            "includedAt": "2021-01-13T23:12:16Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "1000000"
                }
              }
            }
          },
          {
            "hash": "58628178082d005faec360421cff55d31f786f22100586fdbda105da9e6ba03b",
            "includedAt": "2021-01-13T23:06:56Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "1077645949891"
                }
              }
            }
          },
          {
            "hash": "16e73c7f887825152831eaa656c496aa45f69c755bb6e03a3454555e0c790196",
            "includedAt": "2021-01-13T22:10:38Z",
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "591707773"
                }
              }
            }
          }
        ]
      }
    }
    ```
    </details>

=== "cardano-rosetta"

 ```console
    $ curl -X POST 'http://localhost:8080/network/status' -H "Content-Type: application/json" -d '{ "network_identifier": { "blockchain": "cardano", "network": "mainnet" }, "metadata": {} }' | jq
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
{
  "current_block_identifier": {
    "index": 5264121,
    "hash": "8590ee232c112db080b89944d5a6143a319392fe8dfc8ec231fd1b79218c8f40"
  },
  "current_block_timestamp": 1611753255000,
  "genesis_block_identifier": {
    "index": 0,
    "hash": "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
  },
  "peers": [
    {
      "peer_id": "relays-new.cardano-mainnet.iohk.io"
    }
  ]
}
    ```
    </details>
    
 ```console
    $ curl -X POST 'http://localhost:8080/block' -H "Content-Type: application/json" -d '{ "network_identifier": {"blockchain": "cardano", "network": "mainnet" }, "metadata": {}, "block_identifier": {"index": "5264122" }}'
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
{
  "block": {
    "block_identifier": {
      "index": 5264122,
      "hash": "b5426334221805b3c161ec07b02722728ced7b5c38a9cc60962e819620ecbf9a"
    },
    "parent_block_identifier": {
      "index": 5264121,
      "hash": "ff677921d61672d2fb14025f6d8d8b1f93d89266986f045d48ddc674459134ea"
    },
    "timestamp": 1611751783000,
    "transactions": [
      {
        "transaction_identifier": {
          "hash": "2d3d33d26c87d3e8ef0bc6cb25be3fb34f4db43bf23d7c624955a82cd8815772"
        },
        "operations": [
          {
            "operation_identifier": {
              "index": 0
            },
            "type": "input",
            "status": "success",
            "account": {
              "address": "DdzFFzCqrhsqKd92VGNM9Ts1Ms62J2FaSRmf8t1bQa1VugDmcUJzeU8TRFnGDDUR6f1m9VaJJG1GfnzxVjKGBbBAVGT9sPBseREYzP3E"
            },
            "amount": {
              "value": "-4998821898",
              "currency": {
                "symbol": "ADA",
                "decimals": 6
              }
            },
            "coin_change": {
              "coin_identifier": {
                "identifier": "002da5ef961fd9d826332db916fe7de5e198b8a5f00f9c535f9c329bb14f01e8:0"
              },
              "coin_action": "coin_spent"
            }
          },
 
            "coin_change": {
              "coin_identifier": {
                "identifier": "03bd6f6b65f4020b0d35a24b79809e1d5e9db11d67260aa465f2bb281912d5ea:0"
              },
              "coin_action": "coin_spent"
            }
          },
  
    ],
    "metadata": {
      "transactionsCount": 6,
      "createdBy": "ShelleyGenesis-1d4f2e1fda43070d",
      "size": 9559,
      "epochNo": 244,
      "slotNo": 20185492
    }
  }
}
    ```
    </details>


#### [/api/txs/summary/{txId}](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_txsSummary)

Get summary information about a transaction.

=== "cardano-rest"

    ```console
    $ https://explorer.cardano-testnet.iohkdev.io/api/txs/summary/382a5274ebf102910c6c923a8b11f108e79ecedb5d7433cd0dd15a8a443f0fa5
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "Right": {
        "ctsId": "382a5274ebf102910c6c923a8b11f108e79ecedb5d7433cd0dd15a8a443f0fa5",
        "ctsTxTimeIssued": 1610595750,
        "ctsBlockTimeIssued": 1610595750,
        "ctsBlockHeight": 2225547,
        "ctsBlockEpoch": 107,
        "ctsBlockSlot": 372134,
        "ctsBlockHash": "0b8eac7fc7551cb4a2a4381e12e64997921a751ca9bafaa134092ef396b0c124",
        "ctsRelayedBy": null,
        "ctsTotalInput": {
          "getCoin": "979292940"
        },
        "ctsTotalOutput": {
          "getCoin": "979116175"
        },
        "ctsFees": {
          "getCoin": "176765"
        },
        "ctsInputs": [
          {
            "ctaAddress": "addr_test1qq6e9s3ac28mu888k2t5ur0vdntf6s5v5sn28rxwf2agt66h7kkw890p8nmaqpnueyhvdrvvx48auu6wl8c7nctf7x5qpnntdn",
            "ctaAmount": {
              "getCoin": "979292940"
            },
            "ctaTxHash": "99ab43fb82ce38864e29af00cd2806d2abc7d72a99ecc1d1e3a3b52ecc43c31f",
            "ctaTxIndex": 1
          }
        ],
        "ctsOutputs": [
          {
            "ctaAddress": "addr_test1qqle2e7lluxdhxd7cq2zzrtstgs4kxm0pvy7xa6hr48zyu3s85npa69rf0hqja83fhrw3vjcal3ywtnv7q9pu8hc6nrqw6dy2c",
            "ctaAmount": {
              "getCoin": "20000000"
            },
            "ctaTxHash": "382a5274ebf102910c6c923a8b11f108e79ecedb5d7433cd0dd15a8a443f0fa5",
            "ctaTxIndex": 0
          },
          {
            "ctaAddress": "addr_test1qq6e9s3ac28mu888k2t5ur0vdntf6s5v5sn28rxwf2agt66h7kkw890p8nmaqpnueyhvdrvvx48auu6wl8c7nctf7x5qpnntdn",
            "ctaAmount": {
              "getCoin": "959116175"
            },
            "ctaTxHash": "382a5274ebf102910c6c923a8b11f108e79ecedb5d7433cd0dd15a8a443f0fa5",
            "ctaTxIndex": 1
          }
        ]
      }
    }
    ```
    </details>

=== "cardano-graphql"

    **query.graphql**
    ```graphql
    query getTxSummary($hash: Hash32Hex!) {
      transactions(where: { hash: { _eq: $hash } }) {
        hash # ctsId
        includedAt # ctsTxTimeIssued
        fee # ctsFees
        block {
          epochNo # ctsBlockEpoch
          slotNo # ctsBlockSlot
          slotInEpoch
          number # ctsBlockHeight
          hash # ctsBlockHash
        }
        inputs { # ctsInputs
          address # ctaAddress
          value # ctaAmount
          sourceTxHash # ctaTxHash
          sourceTxIndex # ctaTxIndex
        }
        inputs_aggregate {
          aggregate {
            sum {
              value # ctsTotalInput
            }
          }
        }
        outputs { # ctsOutputs
          address # ctaAddress
          value # ctaAmount
          index # ctaTxIndex
          txHash # ctaTxHash
        }
        outputs_aggregate {
          aggregate {
            sum {
              value # ctsTotalOutput
            }
          }
        }
      }
    }
    ```

    **variables.json**
    ```json
    {
      "hash": "382a5274ebf102910c6c923a8b11f108e79ecedb5d7433cd0dd15a8a443f0fa5"
    }
    ```

    ```console
    $ curl 'https://cardano-graphql-testnet.daedalus-operations.com/' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: https://cardano-graphql-testnet.daedalus-operations.com' --data-binary '{"query":"query getTxSummary($hash: Hash32Hex!) {\n  transactions(where: { hash: { _eq: $hash } }) {\n    hash # ctsId\n    includedAt # ctsTxTimeIssued\n    fee # ctsFees\n    block {\n      epochNo # ctsBlockEpoch\n      slotNo # ctsBlockSlot\n      slotInEpoch\n      number # ctsBlockHeight\n      hash # ctsBlockHash\n    }\n    inputs { # ctsInputs\n      address # ctaAddress\n      value # ctaAmount\n      sourceTxHash # ctaTxHash\n      sourceTxIndex # ctaTxIndex\n    }\n    inputs_aggregate {\n      aggregate {\n        sum {\n          value # ctsTotalInput\n        }\n      }\n    }\n    outputs { # ctsOutputs\n      address # ctaAddress\n      value # ctaAmount\n      index # ctaTxIndex\n      txHash # ctaTxHash\n    }\n    outputs_aggregate {\n      aggregate {\n        sum {\n          value # ctsTotalOutput\n        }\n      }\n    }\n  }\n}\n","variables":{"hash":"382a5274ebf102910c6c923a8b11f108e79ecedb5d7433cd0dd15a8a443f0fa5"}}' --compressed
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "data": {
        "transactions": [
          {
            "hash": "382a5274ebf102910c6c923a8b11f108e79ecedb5d7433cd0dd15a8a443f0fa5",
            "includedAt": "2021-01-14T03:42:30Z",
            "fee": 176765,
            "block": {
              "epochNo": 107,
              "slotNo": 16226534,
              "slotInEpoch": 372134,
              "number": 2225547,
              "hash": "0b8eac7fc7551cb4a2a4381e12e64997921a751ca9bafaa134092ef396b0c124"
            },
            "inputs": [
              {
                "address": "addr_test1qq6e9s3ac28mu888k2t5ur0vdntf6s5v5sn28rxwf2agt66h7kkw890p8nmaqpnueyhvdrvvx48auu6wl8c7nctf7x5qpnntdn",
                "value": "979292940",
                "sourceTxHash": "99ab43fb82ce38864e29af00cd2806d2abc7d72a99ecc1d1e3a3b52ecc43c31f",
                "sourceTxIndex": 1
              }
            ],
            "inputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "979292940"
                }
              }
            },
            "outputs": [
              {
                "address": "addr_test1qq6e9s3ac28mu888k2t5ur0vdntf6s5v5sn28rxwf2agt66h7kkw890p8nmaqpnueyhvdrvvx48auu6wl8c7nctf7x5qpnntdn",
                "value": "959116175",
                "index": 1,
                "txHash": "382a5274ebf102910c6c923a8b11f108e79ecedb5d7433cd0dd15a8a443f0fa5"
              },
              {
                "address": "addr_test1qqle2e7lluxdhxd7cq2zzrtstgs4kxm0pvy7xa6hr48zyu3s85npa69rf0hqja83fhrw3vjcal3ywtnv7q9pu8hc6nrqw6dy2c",
                "value": "20000000",
                "index": 0,
                "txHash": "382a5274ebf102910c6c923a8b11f108e79ecedb5d7433cd0dd15a8a443f0fa5"
              }
            ],
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "979116175"
                }
              }
            }
          }
        ]
      }
    }
    ```
    </details>

=== "cardano-rosetta"

 ```console
    $ curl -X POST 'http://localhost:8080/block' -H "Content-Type: application/json" -d '{ "network_identifier": {"blockchain": "cardano", "network": "mainnet" }, "metadata": {}, "block_identifier": {"index": "5264122" }}'
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
{
  "block": {
    "block_identifier": {
      "index": 5264122,
      "hash": "b5426334221805b3c161ec07b02722728ced7b5c38a9cc60962e819620ecbf9a"
    },
    "parent_block_identifier": {
      "index": 5264121,
      "hash": "ff677921d61672d2fb14025f6d8d8b1f93d89266986f045d48ddc674459134ea"
    },
    "timestamp": 1611751783000,
    "transactions": [
      {
        "transaction_identifier": {
          "hash": "2d3d33d26c87d3e8ef0bc6cb25be3fb34f4db43bf23d7c624955a82cd8815772"
        },
        "operations": [
          {
            "operation_identifier": {
              "index": 0
            },
            "type": "input",
            "status": "success",
            "account": {
              "address": "DdzFFzCqrhsqKd92VGNM9Ts1Ms62J2FaSRmf8t1bQa1VugDmcUJzeU8TRFnGDDUR6f1m9VaJJG1GfnzxVjKGBbBAVGT9sPBseREYzP3E"
            },
            "amount": {
              "value": "-4998821898",
              "currency": {
                "symbol": "ADA",
                "decimals": 6
              }
            },
            "coin_change": {
              "coin_identifier": {
                "identifier": "002da5ef961fd9d826332db916fe7de5e198b8a5f00f9c535f9c329bb14f01e8:0"
              },
              "coin_action": "coin_spent"
            }
          },
 
            "coin_change": {
              "coin_identifier": {
                "identifier": "03bd6f6b65f4020b0d35a24b79809e1d5e9db11d67260aa465f2bb281912d5ea:0"
              },
              "coin_action": "coin_spent"
            }
          },
  
    ],
    "metadata": {
      "transactionsCount": 6,
      "createdBy": "ShelleyGenesis-1d4f2e1fda43070d",
      "size": 9559,
      "epochNo": 244,
      "slotNo": 20185492
    }
  }
}
    ```
    </details>

 ```console
    $ curl -X POST 'http://localhost:8080/block/transaction' -H "Content-Type: application/json" -d '{ "network_identifier": { "blockchain": "cardano", "network": "mainnet" }, "metadata": {}, "block_identifier": { "index": "5264122", "hash": "b5426334221805b3c161ec07b02722728ced7b5c38a9cc60962e819620ecbf9a" }, "transaction_identifier": { "hash": "2d3d33d26c87d3e8ef0bc6cb25be3fb34f4db43bf23d7c624955a82cd8815772"}}' | jq
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
{
  "transaction": {
    "transaction_identifier": {
      "hash": "2d3d33d26c87d3e8ef0bc6cb25be3fb34f4db43bf23d7c624955a82cd8815772"
    },
    "operations": [
      {
        "operation_identifier": {
          "index": 0
        },
        "type": "input",
        "status": "success",
        "account": {
          "address": "DdzFFzCqrhsqKd92VGNM9Ts1Ms62J2FaSRmf8t1bQa1VugDmcUJzeU8TRFnGDDUR6f1m9VaJJG1GfnzxVjKGBbBAVGT9sPBseREYzP3E"
        },
        "amount": {
          "value": "-4998821898",
          "currency": {
            "symbol": "ADA",
            "decimals": 6
          }
        },
        "coin_change": {
          "coin_identifier": {
            "identifier": "002da5ef961fd9d826332db916fe7de5e198b8a5f00f9c535f9c329bb14f01e8:0"
          },
          "coin_action": "coin_spent"
        }
      },
      {
        "operation_identifier": {
          "index": 1
        },
        "type": "input",
        "status": "success",
        "account": {
          "address": "DdzFFzCqrhspNxbM9rETXrrW3XL4WcuGgajfxzH3j3R7UQr4AgicTVCMbybX6naYn8ngYAgpGvRyyGecsx26fHgS43a71yUJRmjcdvJE"
        },
        "amount": {
          "value": "-4000000000",
          "currency": {
            "symbol": "ADA",
            "decimals": 6
          }
        },
        "coin_change": {
          "coin_identifier": {
            "identifier": "0151758ddf6525b9c4fb5dc00a10abb519bff36894716bc1377ba400ae995f28:0"
          },
          "coin_action": "coin_spent"
        }
      },
      
    ```
    </details>

#### [/api/stats/txs](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_statsTxs)

Get statistics about transactions.

=== "cardano-rest"

    ```console
    $ https://explorer.cardano-testnet.iohkdev.io/api/stats/txs?page=221444
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "Right": [
        222563,
        [
          [
            "c62c7f356bae7c1397ce5af171c7f884bd5b11eebc62e35f78ff60862004b6a5",
            233
          ]
        ]
      ]
    }
    ```
    </details>

=== "cardano-graphql"

    GraphQL doesn't have pagination. Instead we can specify start and end of
    block height and achieve the same result.

    **query.graphql**
    ```graphql
    query getTxsStats($start: Int!, $end: Int!) {
      blocks(
        order_by: { forgedAt: desc }
        where: {
          number: { _gte: $start, _lte: $end }
          transactionsCount: { _gt: "0" }
        }
      ) {
        transactions {
          hash
          size
        }
      }
    }
    ```

    **variables.json**
    ```json
    {
      "start": 2214430,
      "end": 2214440
    }
    ```

    ```console
    $ curl 'https://cardano-graphql-testnet.daedalus-operations.com/' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: https://cardano-graphql-testnet.daedalus-operations.com' --data-binary '{"query":"query getTxsStats($start: Int!, $end: Int!) {\n  blocks(\n    order_by: { forgedAt: desc }\n    where: {\n      number: { _gte: $start, _lte: $end }\n      transactionsCount: { _gt: \"0\" }\n    }\n  ) {\n    transactions {\n      hash\n      size\n    }\n  }\n}\n","variables":{"start":2214430,"end":2214440}}' --compressed
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    ```
    </details>

=== "cardano-rosetta"

 ```console
    $ curl -X POST 'http://localhost:8080/block/transaction' -H "Content-Type: application/json" -d '{ "network_identifier": { "blockchain": "cardano", "network": "mainnet" }, "metadata": {}, "block_identifier": { "index": "5264122", "hash": "b5426334221805b3c161ec07b02722728ced7b5c38a9cc60962e819620ecbf9a" }, "transaction_identifier": { "hash": "2d3d33d26c87d3e8ef0bc6cb25be3fb34f4db43bf23d7c624955a82cd8815772"}}' | jq
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
{
  "transaction": {
    "transaction_identifier": {
      "hash": "2d3d33d26c87d3e8ef0bc6cb25be3fb34f4db43bf23d7c624955a82cd8815772"
    },
    "operations": [
      {
        "operation_identifier": {
          "index": 0
        },
        "type": "input",
        "status": "success",
        "account": {
          "address": "DdzFFzCqrhsqKd92VGNM9Ts1Ms62J2FaSRmf8t1bQa1VugDmcUJzeU8TRFnGDDUR6f1m9VaJJG1GfnzxVjKGBbBAVGT9sPBseREYzP3E"
        },
        "amount": {
          "value": "-4998821898",
          "currency": {
            "symbol": "ADA",
            "decimals": 6
          }
        },
        "coin_change": {
          "coin_identifier": {
            "identifier": "002da5ef961fd9d826332db916fe7de5e198b8a5f00f9c535f9c329bb14f01e8:0"
          },
          "coin_action": "coin_spent"
        }
      },
      {
        "operation_identifier": {
          "index": 1
        },
        "type": "input",
        "status": "success",
        "account": {
          "address": "DdzFFzCqrhspNxbM9rETXrrW3XL4WcuGgajfxzH3j3R7UQr4AgicTVCMbybX6naYn8ngYAgpGvRyyGecsx26fHgS43a71yUJRmjcdvJE"
        },
        "amount": {
          "value": "-4000000000",
          "currency": {
            "symbol": "ADA",
            "decimals": 6
          }
        },
        "coin_change": {
          "coin_identifier": {
            "identifier": "0151758ddf6525b9c4fb5dc00a10abb519bff36894716bc1377ba400ae995f28:0"
          },
          "coin_action": "coin_spent"
        }
      },
      
    ```
    </details>
    
## Addresses

#### [/api/addresses/summary/{address}](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_addressSummary)

Get summary information about an address.

=== "cardano-rest"

    ```console
    $ https://explorer.cardano-testnet.iohkdev.io/api/addresses/summary/addr_test1qrvf30r0e6r8zjzmv22a4r3h8kzj2xf3l2ekezvqd66mcj6kvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqrfd5g5
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "Right": {
        "caAddress": "addr_test1qrvf30r0e6r8zjzmv22a4r3h8kzj2xf3l2ekezvqd66mcj6kvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqrfd5g5",
        "caType": "CPubKeyAddress",
        "caChainTip": {
          "ctBlockNo": 2225942,
          "ctSlotNo": 16238640,
          "ctBlockHash": "8c892cd82dcb7265e0714346961afddc6d2334a8cd46e0607bd512046ac1db0e"
        },
        "caTxNum": 1,
        "caBalance": {
          "getCoin": "590707773"
        },
        "caTotalInput": {
          "getCoin": "590707773"
        },
        "caTotalOutput": {
          "getCoin": "0"
        },
        "caTotalFee": {
          "getCoin": "172101"
        },
        "caTxList": [
          {
            "ctbId": "16e73c7f887825152831eaa656c496aa45f69c755bb6e03a3454555e0c790196",
            "ctbTimeIssued": 1610575838,
            "ctbInputs": [
              {
                "ctaAddress": "addr_test1qp6dnz3gs84zn7cjx3mrhquj99zfd0acqh9qjldeec9rp4zkvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqs553m2",
                "ctaAmount": {
                  "getCoin": "591879874"
                },
                "ctaTxHash": "984bf469edee7bcd19ddcf9b7c5cc7e107e10df5061d58dff99d81bb29466094",
                "ctaTxIndex": 1
              }
            ],
            "ctbOutputs": [
              {
                "ctaAddress": "addr_test1qrvf30r0e6r8zjzmv22a4r3h8kzj2xf3l2ekezvqd66mcj6kvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqrfd5g5",
                "ctaAmount": {
                  "getCoin": "590707773"
                },
                "ctaTxHash": "16e73c7f887825152831eaa656c496aa45f69c755bb6e03a3454555e0c790196",
                "ctaTxIndex": 0
              },
              {
                "ctaAddress": "addr_test1qrlh7p9th5c9ps938ry05vq96j92lzuhqr29v46caydf2wzkvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqt48df3",
                "ctaAmount": {
                  "getCoin": "1000000"
                },
                "ctaTxHash": "16e73c7f887825152831eaa656c496aa45f69c755bb6e03a3454555e0c790196",
                "ctaTxIndex": 1
              }
            ],
            "ctbInputSum": {
              "getCoin": "591879874"
            },
            "ctbOutputSum": {
              "getCoin": "591707773"
            },
            "ctbFees": {
              "getCoin": "172101"
            }
          }
        ]
      }
    }
    ```
    </details>

=== "cardano-graphql"

    !!! info
        The following fields from the REST endpoint are not supported: `caType`, `caChainTip`.

    **query.graphql**
    ```graphql
    query getAddressSummary($address: String!) {
      caTxNum: transactions_aggregate(
        where: {
          _or: [
            { inputs: { address: { _eq: $address } } }
            { outputs: { address: { _eq: $address } } }
          ]
        }
      ) {
        aggregate {
          count # caTxNum
        }
      }
      caTotalInput: transactions_aggregate(
        where: { inputs: { address: { _eq: $address } } }
      ) {
        aggregate {
          sum {
            totalOutput # caTotalInput
          }
        }
      }
      caTotalOutput: transactions_aggregate(
        where: { outputs: { address: { _eq: $address } } }
      ) {
        aggregate {
          sum {
            totalOutput # caTotalOutput
          }
        }
      }
      caBalance: utxos_aggregate(where: { address: { _eq: $address } }) {
        aggregate {
          sum {
            value # caBalance
          }
        }
      }
      caTxList: transactions(
        where: {
          _or: [
            { inputs: { address: { _eq: $address } } }
            { outputs: { address: { _eq: $address } } }
          ]
        }
      ) {
        hash # ctbId
        includedAt # ctbTimeIssued
        fee # ctbFees
        inputs(where: { address: { _eq: $address } }) {
          # ctbInputs
          address # ctaAddress
          value # ctaAmount
          txHash # ctaTxHash
          sourceTxIndex # ctaTxIndex
        }
        outputs(where: { address: { _eq: $address } }) {
          address # ctaAddress
          value # ctaAmount
          txHash # ctaTxHash
          # TODO: ctaTxIndex ??
        }
        inputs_aggregate(where: { address: { _eq: $address } }) {
          aggregate {
            sum {
              value # ctbInputSum
            }
          }
        }
        outputs_aggregate(where: { address: { _eq: $address } }) {
          aggregate {
            sum {
              value # ctbOutputSum
            }
          }
        }
      }
    }
    ```

    **variables.json**
    ```json
    {
      "address": "addr_test1qrvf30r0e6r8zjzmv22a4r3h8kzj2xf3l2ekezvqd66mcj6kvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqrfd5g5"
    }
    ```

    ```console
    $ curl 'https://cardano-graphql-testnet.daedalus-operations.com/' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: https://cardano-graphql-testnet.daedalus-operations.com' --data-binary '{"query":"query getAddressSummary($address: String!) {\n  caTxNum: transactions_aggregate(\n    where: {\n      _or: [\n        { inputs: { address: { _eq: $address } } }\n        { outputs: { address: { _eq: $address } } }\n      ]\n    }\n  ) {\n    aggregate {\n      count # caTxNum\n    }\n  }\n  # TODO: caTotalFee ??\n  # TODO: caType ??\n  # TODO: caChainTip ??\n  caTotalInput: transactions_aggregate(\n    where: { inputs: { address: { _eq: $address } } }\n  ) {\n    aggregate {\n      sum {\n        totalOutput # caTotalInput ??\n      }\n    }\n  }\n  caTotalOutput: transactions_aggregate(\n    where: { outputs: { address: { _eq: $address } } }\n  ) {\n    aggregate {\n      sum {\n        totalOutput # caTotalOutput ??\n      }\n    }\n  }\n  caBalance: utxos_aggregate(where: { address: { _eq: $address } }) {\n    aggregate {\n      sum {\n        value # caBalance\n      }\n    }\n  }\n  caTxList: transactions(\n    where: {\n      _or: [\n        { inputs: { address: { _eq: $address } } }\n        { outputs: { address: { _eq: $address } } }\n      ]\n    }\n  ) {\n    hash # ctbId\n    includedAt # ctbTimeIssued\n    fee # ctbFees\n    inputs(where: { address: { _eq: $address } }) {\n      # ctbInputs\n      address # ctaAddress\n      value # ctaAmount\n      txHash # ctaTxHash\n      sourceTxIndex # ctaTxIndex\n    }\n    outputs(where: { address: { _eq: $address } }) {\n      address # ctaAddress\n      value # ctaAmount\n      txHash # ctaTxHash\n      # TODO: ctaTxIndex ??\n    }\n    inputs_aggregate(where: { address: { _eq: $address } }) {\n      aggregate {\n        sum {\n          value # ctbInputSum\n        }\n      }\n    }\n    outputs_aggregate(where: { address: { _eq: $address } }) {\n      aggregate {\n        sum {\n          value # ctbOutputSum\n        }\n      }\n    }\n  }\n}\n","variables":{"address":"addr_test1qrvf30r0e6r8zjzmv22a4r3h8kzj2xf3l2ekezvqd66mcj6kvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqrfd5g5"}}' --compressed
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "data": {
        "caTxNum": {
          "aggregate": {
            "count": "1"
          }
        },
        "caTotalInput": {
          "aggregate": {
            "sum": {
              "totalOutput": null
            }
          }
        },
        "caTotalOutput": {
          "aggregate": {
            "sum": {
              "totalOutput": "591707773"
            }
          }
        },
        "caBalance": {
          "aggregate": {
            "sum": {
              "value": "590707773"
            }
          }
        },
        "caTxList": [
          {
            "hash": "16e73c7f887825152831eaa656c496aa45f69c755bb6e03a3454555e0c790196",
            "includedAt": "2021-01-13T22:10:38Z",
            "fee": 172101,
            "inputs": [],
            "outputs": [
              {
                "address": "addr_test1qrvf30r0e6r8zjzmv22a4r3h8kzj2xf3l2ekezvqd66mcj6kvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqrfd5g5",
                "value": "590707773",
                "txHash": "16e73c7f887825152831eaa656c496aa45f69c755bb6e03a3454555e0c790196"
              }
            ],
            "inputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": null
                }
              }
            },
            "outputs_aggregate": {
              "aggregate": {
                "sum": {
                  "value": "590707773"
                }
              }
            }
          }
        ]
      }
    }
    ```
    </details>

=== "cardano-rosetta"

                  curl -X POST http://localhost:8080/account/balance \
                  -H "Content-Type: application/json" \
                  -d '{ "network_identifier": {
                  "blockchain": "cardano", 
                  "network": "mainnet" },
                  "metadata": {},
                  "account_identifier": {
                  "address": "$address" },
                  "metadata": {}}' | jq
                  
  ```console
    $ curl -X POST 'http://localhost:8080/account/balance' -H "Content-Type: application/json" -d '{ "network_identifier": { "blockchain": "cardano", "network": "mainnet" }, "metadata": {}, "account_identifier": { "address": "DdzFFzCqrhsqKd92VGNM9Ts1Ms62J2FaSRmf8t1bQa1VugDmcUJzeU8TRFnGDDUR6f1m9VaJJG1GfnzxVjKGBbBAVGT9sPBseREYzP3E" }, "metadata": {}}' | jq
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
{
  "block_identifier": {
    "index": 5264292,
    "hash": "3dc53a1991c70a160666f597c71cb751c2910ef3e14581e699e8011de9fc6839"
  },
  "balances": [
    {
      "value": "0",
      "currency": {
        "symbol": "ADA",
        "decimals": 6
      }
    }
  ],
  "coins": []
}  
    ```
    </details>

#### [/api/block/{blockHash}/address/{address}](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_blockAddress)

Get address information specific to a block.

=== "cardano-rest"

    ```console
    $ https://explorer.cardano-testnet.iohkdev.io/api/block/02788224fb087d9a5ed48a12a3db741ad7bd4429a63e7e5b6bc69d2f2956d205/address/addr_test1qrvf30r0e6r8zjzmv22a4r3h8kzj2xf3l2ekezvqd66mcj6kvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqrfd5g5
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "Right": {
        "caAddress": "addr_test1qrvf30r0e6r8zjzmv22a4r3h8kzj2xf3l2ekezvqd66mcj6kvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqrfd5g5",
        "caType": "CPubKeyAddress",
        "caChainTip": {
          "ctBlockNo": 2225979,
          "ctSlotNo": 16239920,
          "ctBlockHash": "0ad0938aab4a1db979fa84fc38ca742dd3b1d6ff548e50465fbf9b7a42d0b518"
        },
        "caTxNum": 1,
        "caBalance": {
          "getCoin": "590707773"
        },
        "caTotalInput": {
          "getCoin": "590707773"
        },
        "caTotalOutput": {
          "getCoin": "0"
        },
        "caTotalFee": {
          "getCoin": "172101"
        },
        "caTxList": [
          {
            "ctbId": "16e73c7f887825152831eaa656c496aa45f69c755bb6e03a3454555e0c790196",
            "ctbTimeIssued": 1610575838,
            "ctbInputs": [
              {
                "ctaAddress": "addr_test1qp6dnz3gs84zn7cjx3mrhquj99zfd0acqh9qjldeec9rp4zkvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqs553m2",
                "ctaAmount": {
                  "getCoin": "591879874"
                },
                "ctaTxHash": "16e73c7f887825152831eaa656c496aa45f69c755bb6e03a3454555e0c790196",
                "ctaTxIndex": 1
              }
            ],
            "ctbOutputs": [
              {
                "ctaAddress": "addr_test1qrvf30r0e6r8zjzmv22a4r3h8kzj2xf3l2ekezvqd66mcj6kvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqrfd5g5",
                "ctaAmount": {
                  "getCoin": "590707773"
                },
                "ctaTxHash": "16e73c7f887825152831eaa656c496aa45f69c755bb6e03a3454555e0c790196",
                "ctaTxIndex": 0
              },
              {
                "ctaAddress": "addr_test1qrlh7p9th5c9ps938ry05vq96j92lzuhqr29v46caydf2wzkvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqt48df3",
                "ctaAmount": {
                  "getCoin": "1000000"
                },
                "ctaTxHash": "16e73c7f887825152831eaa656c496aa45f69c755bb6e03a3454555e0c790196",
                "ctaTxIndex": 1
              }
            ],
            "ctbInputSum": {
              "getCoin": "591879874"
            },
            "ctbOutputSum": {
              "getCoin": "591707773"
            },
            "ctbFees": {
              "getCoin": "172101"
            }
          }
        ]
      }
    }
    ```
    </details>

=== "cardano-graphql"

    !!! info
        This query is WIP. A few fields are still missing.

    **query.graphql**
    ```graphql
    query getAddressBlockSummary($address: String!, $block: Hash32Hex!) {
      blocks(where: { hash: { _eq: $block } }) {
        transactions_aggregate(
          where: {
            _or: [
              { inputs: { address: { _eq: $address } } }
              { outputs: { address: { _eq: $address } } }
            ]
          }
        ) {
          aggregate {
            count # caTxNum
          }
        }
        transactions(
          where: {
            _or: [
              { inputs: { address: { _eq: $address } } }
              { outputs: { address: { _eq: $address } } }
            ]
          }
        ) {
          # caTxList
          hash # ctbId
          includedAt # ctbTimeIssued
          fee # ctbFees
          inputs(where: { address: { _eq: $address } }) {
            address # ctaAddress
            value # ctaAmount
            txHash # ctaTxHash
            sourceTxIndex # ctaTxIndex
          }
          outputs(where: { address: { _eq: $address } }) {
            address # ctaAddress
            value # ctaAmount
            txHash # ctaTxHash
          }
          inputs_aggregate(where: { address: { _eq: $address } }) {
            aggregate {
              sum {
                value # ctbInputSum
              }
            }
          }
          outputs_aggregate(where: { address: { _eq: $address } }) {
            aggregate {
              sum {
                value # ctbOutputSum
              }
            }
          }
        }
      }
    }
    ```

    **variables.json**
    ```json
    {
      "address": "addr_test1qrvf30r0e6r8zjzmv22a4r3h8kzj2xf3l2ekezvqd66mcj6kvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqrfd5g5",
      "block": "02788224fb087d9a5ed48a12a3db741ad7bd4429a63e7e5b6bc69d2f2956d205"
    }
    ```

    ```console
    $ curl 'https://cardano-graphql-testnet.daedalus-operations.com/' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: https://cardano-graphql-testnet.daedalus-operations.com' --data-binary '{"query":"query getAddressBlockSummary($address: String!, $block: Hash32Hex!) {\n  blocks(where: { hash: { _eq: $block } }) {\n    transactions_aggregate(\n      where: {\n        _or: [\n          { inputs: { address: { _eq: $address } } }\n          { outputs: { address: { _eq: $address } } }\n        ]\n      }\n    ) {\n      aggregate {\n        count # caTxNum\n      }\n    }\n    transactions(\n      where: {\n        _or: [\n          { inputs: { address: { _eq: $address } } }\n          { outputs: { address: { _eq: $address } } }\n        ]\n      }\n    ) {\n      # caTxList\n      hash # ctbId\n      includedAt # ctbTimeIssued\n      fee # ctbFees\n      inputs(where: { address: { _eq: $address } }) {\n        address # ctaAddress\n        value # ctaAmount\n        txHash # ctaTxHash\n        sourceTxIndex # ctaTxIndex\n      }\n      outputs(where: { address: { _eq: $address } }) {\n        address # ctaAddress\n        value # ctaAmount\n        txHash # ctaTxHash\n      }\n      inputs_aggregate(where: { address: { _eq: $address } }) {\n        aggregate {\n          sum {\n            value # ctbInputSum\n          }\n        }\n      }\n      outputs_aggregate(where: { address: { _eq: $address } }) {\n        aggregate {\n          sum {\n            value # ctbOutputSum\n          }\n        }\n      }\n    }\n  }\n}\n","variables":{"address":"addr_test1qrvf30r0e6r8zjzmv22a4r3h8kzj2xf3l2ekezvqd66mcj6kvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqrfd5g5","block":"02788224fb087d9a5ed48a12a3db741ad7bd4429a63e7e5b6bc69d2f2956d205"}}' --compressed
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "data": {
        "blocks": [
          {
            "transactions_aggregate": {
              "aggregate": {
                "count": "1"
              }
            },
            "transactions": [
              {
                "hash": "16e73c7f887825152831eaa656c496aa45f69c755bb6e03a3454555e0c790196",
                "includedAt": "2021-01-13T22:10:38Z",
                "fee": 172101,
                "inputs": [],
                "outputs": [
                  {
                    "address": "addr_test1qrvf30r0e6r8zjzmv22a4r3h8kzj2xf3l2ekezvqd66mcj6kvlatzplcfr8afde6wsr6weskqr8k3u80e957ecmkvkhqrfd5g5",
                    "value": "590707773",
                    "txHash": "16e73c7f887825152831eaa656c496aa45f69c755bb6e03a3454555e0c790196"
                  }
                ],
                "inputs_aggregate": {
                  "aggregate": {
                    "sum": {
                      "value": null
                    }
                  }
                },
                "outputs_aggregate": {
                  "aggregate": {
                    "sum": {
                      "value": "590707773"
                    }
                  }
                }
              }
            ]
          }
        ]
      }
    }
    ```
    </details>

=== "cardano-rosetta"

 ```console
    $ curl -X POST 'http://localhost:8080/account/balance' -H "Content-Type: application/json" -d '{ "network_identifier": { "blockchain": "cardano", "network": "mainnet" }, "metadata": {}, "account_identifier": { "address": "DdzFFzCqrhsqKd92VGNM9Ts1Ms62J2FaSRmf8t1bQa1VugDmcUJzeU8TRFnGDDUR6f1m9VaJJG1GfnzxVjKGBbBAVGT9sPBseREYzP3E" }, "metadata": {}, "block_identifier": {"index": "5264122","hash": "b5426334221805b3c161ec07b02722728ced7b5c38a9cc60962e819620ecbf9a" },"currencies": {"symbol": "ada", "decimals": 8 }}' | jq
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
{
  "block_identifier": {
    "index": 5264292,
    "hash": "3dc53a1991c70a160666f597c71cb751c2910ef3e14581e699e8011de9fc6839"
  },
  "balances": [
    {
      "value": "0",
      "currency": {
        "symbol": "ADA",
        "decimals": 6
      }
    }
  ],
  "coins": []
}  
    ```
    </details>



## Epochs

#### [/api/epochs/{epoch}](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_epochPages)

Get epoch pages, all the paged slots in the epoch.

=== "cardano-rest"

    ```console
    $ https://explorer.cardano-testnet.iohkdev.io/api/epochs/1?page=1
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "Right": [
        2161,
        [
          {
            "cbeEpoch": 1,
            "cbeSlot": 9,
            "cbeBlkHeight": 20579,
            "cbeBlkHash": "af3a74f0fb8a651923ee4dec365925460eed9527a0c4ecae2087e005f0d5a007",
            "cbeTimeIssued": 1564431796,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 667,
            "cbeBlockLead": "44e51b81adce8430a63c985594cc82b8908e3ab96a4d6b2f33259829",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 1,
            "cbeSlot": 8,
            "cbeBlkHeight": 20578,
            "cbeBlkHash": "0edb3f4a743744b907a99c51fe75ef233e1fae585c758dcdd5d32c979b52ab67",
            "cbeTimeIssued": 1564431776,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 667,
            "cbeBlockLead": "44e51b81adce8430a63c985594cc82b8908e3ab96a4d6b2f33259829",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 1,
            "cbeSlot": 7,
            "cbeBlkHeight": 20577,
            "cbeBlkHash": "7bca5147f6818c43c05c008adff93703ef5549ad3a82dc41ab0000c7167fc782",
            "cbeTimeIssued": 1564431756,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 667,
            "cbeBlockLead": "a08d73c9dd7f34ae2d68e7631cd80338bc2e88b0c64ed8df52099c91",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 1,
            "cbeSlot": 6,
            "cbeBlkHeight": 20576,
            "cbeBlkHash": "324e4810647798305fef632250fe7f5192edd56bde5f9be5fef48ec0cc505c2d",
            "cbeTimeIssued": 1564431736,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 667,
            "cbeBlockLead": "d6ff29d80d4007f5d7b883d34e4b73b04bc128fbe15a69eb75800934",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 1,
            "cbeSlot": 5,
            "cbeBlkHeight": 20575,
            "cbeBlkHash": "226916eec8547355fe9b2696cf2fabbee1a1f056ab5267b4acfe863069e16964",
            "cbeTimeIssued": 1564431716,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 667,
            "cbeBlockLead": "42186a6a0079ef39ec0414b69d06ae3526e50b4c4c25d043d357abc8",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 1,
            "cbeSlot": 4,
            "cbeBlkHeight": 20574,
            "cbeBlkHash": "b9de6862957ab26062c31e28d21f42c2cfa1105f10e29972d83879bbf697e84f",
            "cbeTimeIssued": 1564431696,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 3233,
            "cbeBlockLead": "2188aa3235aa3912c6ca9c26a1ada76b5aface7efa2b62078a598e23",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 1,
            "cbeSlot": 3,
            "cbeBlkHeight": 20573,
            "cbeBlkHash": "ef3f7fe64c0790631fee167d4271a4665fd87f06e351918b45f09c4033afed55",
            "cbeTimeIssued": 1564431676,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 3233,
            "cbeBlockLead": "44e51b81adce8430a63c985594cc82b8908e3ab96a4d6b2f33259829",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 1,
            "cbeSlot": 2,
            "cbeBlkHeight": 20572,
            "cbeBlkHash": "3898f6d5620bc2bd7ed491737472933ec0b94643e69d30dffa83f48b31d2cd23",
            "cbeTimeIssued": 1564431656,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 3233,
            "cbeBlockLead": "44e51b81adce8430a63c985594cc82b8908e3ab96a4d6b2f33259829",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 1,
            "cbeSlot": 1,
            "cbeBlkHeight": 20571,
            "cbeBlkHash": "51a5d5d52d555942b980023fb808ec99ed8702e37f0d0a44c4ef4e43563b5213",
            "cbeTimeIssued": 1564431636,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 1950,
            "cbeBlockLead": "0df4205606dcb8adf868cb937e4792a4826eb849193755aa29574af9",
            "cbeFees": {
              "getCoin": "0"
            }
          },
          {
            "cbeEpoch": 1,
            "cbeSlot": 0,
            "cbeBlkHeight": 20570,
            "cbeBlkHash": "10380e7e4a8147427e0f0431d38990c4ad0f90599f080039263f6678170563ce",
            "cbeTimeIssued": 1564431616,
            "cbeTxNum": 0,
            "cbeTotalSent": {
              "getCoin": "0"
            },
            "cbeSize": 667,
            "cbeBlockLead": "d6ff29d80d4007f5d7b883d34e4b73b04bc128fbe15a69eb75800934",
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
    query getEpoch($epoch: Int!, $block_limit: Int!) {
      epochs(where: { number: { _eq: $epoch } }) {
        number # cbeEpoch
        blocks(limit: $block_limit, order_by: { slotNo: asc }) {
          slotInEpoch # cbeSlot
          number # cbeBlkHeight
          hash # cbeBlkHash
          slotLeader {
            hash # cbeBlockLead
          }
          forgedAt # cbeTimeIssued
        }
        startedAt
        transactionsCount # cbeTxNum
        blocks_aggregate {
          aggregate {
            sum {
              size # cbeSize
              fees # cbeFees
            }
          }
        }
        output # cbeTotalSent
      }
    }
    ```

    **variables.json**
    ```json
    {
      "epoch": 1,
      "block_limit": 10
    }
    ```

    ```console
    $ curl 'https://cardano-graphql-testnet.daedalus-operations.com/' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: https://cardano-graphql-testnet.daedalus-operations.com' --data-binary '{"query":"query getEpoch($epoch: Int!, $block_limit: Int!) {\n  epochs(where: { number: { _eq: $epoch } }) {\n    number # cbeEpoch\n    blocks(limit: $block_limit, order_by: { slotNo: asc }) {\n      slotInEpoch # cbeSlot\n      number # cbeBlkHeight\n      hash # cbeBlkHash\n      slotLeader {\n        hash # cbeBlockLead\n      }\n      forgedAt # cbeTimeIssued\n    }\n    startedAt\n    transactionsCount # cbeTxNum\n    blocks_aggregate {\n      aggregate {\n        sum {\n          size # cbeSize\n          fees # cbeFees\n        }\n      }\n    }\n    output # cbeTotalSent\n  }\n}","variables":{"epoch":1,"block_limit":10}}' --compressed
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "data": {
        "epochs": [
          {
            "number": 1,
            "blocks": [
              {
                "slotInEpoch": 0,
                "number": 20570,
                "hash": "10380e7e4a8147427e0f0431d38990c4ad0f90599f080039263f6678170563ce",
                "slotLeader": {
                  "hash": "d6ff29d80d4007f5d7b883d34e4b73b04bc128fbe15a69eb75800934"
                },
                "forgedAt": "2019-07-29T20:20:16Z"
              },
              {
                "slotInEpoch": 1,
                "number": 20571,
                "hash": "51a5d5d52d555942b980023fb808ec99ed8702e37f0d0a44c4ef4e43563b5213",
                "slotLeader": {
                  "hash": "0df4205606dcb8adf868cb937e4792a4826eb849193755aa29574af9"
                },
                "forgedAt": "2019-07-29T20:20:36Z"
              },
              {
                "slotInEpoch": 2,
                "number": 20572,
                "hash": "3898f6d5620bc2bd7ed491737472933ec0b94643e69d30dffa83f48b31d2cd23",
                "slotLeader": {
                  "hash": "44e51b81adce8430a63c985594cc82b8908e3ab96a4d6b2f33259829"
                },
                "forgedAt": "2019-07-29T20:20:56Z"
              },
              {
                "slotInEpoch": 3,
                "number": 20573,
                "hash": "ef3f7fe64c0790631fee167d4271a4665fd87f06e351918b45f09c4033afed55",
                "slotLeader": {
                  "hash": "44e51b81adce8430a63c985594cc82b8908e3ab96a4d6b2f33259829"
                },
                "forgedAt": "2019-07-29T20:21:16Z"
              },
              {
                "slotInEpoch": 4,
                "number": 20574,
                "hash": "b9de6862957ab26062c31e28d21f42c2cfa1105f10e29972d83879bbf697e84f",
                "slotLeader": {
                  "hash": "2188aa3235aa3912c6ca9c26a1ada76b5aface7efa2b62078a598e23"
                },
                "forgedAt": "2019-07-29T20:21:36Z"
              },
              {
                "slotInEpoch": 5,
                "number": 20575,
                "hash": "226916eec8547355fe9b2696cf2fabbee1a1f056ab5267b4acfe863069e16964",
                "slotLeader": {
                  "hash": "42186a6a0079ef39ec0414b69d06ae3526e50b4c4c25d043d357abc8"
                },
                "forgedAt": "2019-07-29T20:21:56Z"
              },
              {
                "slotInEpoch": 6,
                "number": 20576,
                "hash": "324e4810647798305fef632250fe7f5192edd56bde5f9be5fef48ec0cc505c2d",
                "slotLeader": {
                  "hash": "d6ff29d80d4007f5d7b883d34e4b73b04bc128fbe15a69eb75800934"
                },
                "forgedAt": "2019-07-29T20:22:16Z"
              },
              {
                "slotInEpoch": 7,
                "number": 20577,
                "hash": "7bca5147f6818c43c05c008adff93703ef5549ad3a82dc41ab0000c7167fc782",
                "slotLeader": {
                  "hash": "a08d73c9dd7f34ae2d68e7631cd80338bc2e88b0c64ed8df52099c91"
                },
                "forgedAt": "2019-07-29T20:22:36Z"
              },
              {
                "slotInEpoch": 8,
                "number": 20578,
                "hash": "0edb3f4a743744b907a99c51fe75ef233e1fae585c758dcdd5d32c979b52ab67",
                "slotLeader": {
                  "hash": "44e51b81adce8430a63c985594cc82b8908e3ab96a4d6b2f33259829"
                },
                "forgedAt": "2019-07-29T20:22:56Z"
              },
              {
                "slotInEpoch": 9,
                "number": 20579,
                "hash": "af3a74f0fb8a651923ee4dec365925460eed9527a0c4ecae2087e005f0d5a007",
                "slotLeader": {
                  "hash": "44e51b81adce8430a63c985594cc82b8908e3ab96a4d6b2f33259829"
                },
                "forgedAt": "2019-07-29T20:23:16Z"
              }
            ],
            "startedAt": "2019-07-29T20:20:16Z",
            "transactionsCount": "305",
            "blocks_aggregate": {
              "aggregate": {
                "sum": {
                  "size": "14882059",
                  "fees": "54105620"
                }
              }
            },
            "output": "152336265877919"
          }
        ]
      }
    }
    ```
    </details>

=== "cardano-rosetta"

    !!! warning
        Not available in Rosetta!

#### [/api/epochs/{epoch}/{slot}](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_epochSlots)

Get the slot information in an epoch.

=== "cardano-rest"

    ```console
    $ https://explorer.cardano-testnet.iohkdev.io/api/epochs/1/8
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "Right": [
        {
          "cbeEpoch": 1,
          "cbeSlot": 8,
          "cbeBlkHeight": 20578,
          "cbeBlkHash": "0edb3f4a743744b907a99c51fe75ef233e1fae585c758dcdd5d32c979b52ab67",
          "cbeTimeIssued": 1564431776,
          "cbeTxNum": 0,
          "cbeTotalSent": {
            "getCoin": "0"
          },
          "cbeSize": 667,
          "cbeBlockLead": "44e51b81adce8430a63c985594cc82b8908e3ab96a4d6b2f33259829",
          "cbeFees": {
            "getCoin": "0"
          }
        }
      ]
    }
    ```
    </details>

=== "cardano-graphql"

    **query.graphql**
    ```graphql
    query getSlotInEpoch($epoch: Int!, $slot: Int!) {
      epochs(where: { number: { _eq: $epoch } }) {
        number # cbeEpoch
        blocks(where: { slotInEpoch: { _eq: $slot } }) {
          slotInEpoch # cbeSlot
          number # cbeBlkHeight
          hash # cbeBlkHash
          slotLeader {
            hash # cbeBlockLead
          }
          forgedAt # cbeTimeIssued
        }
        startedAt
        transactionsCount # cbeTxNum
        blocks_aggregate(where: { slotInEpoch: { _eq: $slot } }) {
          aggregate {
            sum {
              size # cbeSize
              fees # cbeFees
            }
          }
        }
        output # cbeTotalSent
      }
    }
    ```

    **variables.json**
    ```json
    {
      "epoch": 1,
      "slot": 8
    }
    ```

    ```console
    $ curl 'https://cardano-graphql-testnet.daedalus-operations.com/' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: https://cardano-graphql-testnet.daedalus-operations.com' --data-binary '{"query":"query getSlotInEpoch($epoch: Int!, $slot: Int!) {\n  epochs(where: { number: { _eq: $epoch } }) {\n    number # cbeEpoch\n    blocks(where: { slotInEpoch: { _eq: $slot } }) {\n      slotInEpoch # cbeSlot\n      number # cbeBlkHeight\n      hash # cbeBlkHash\n      slotLeader {\n        hash # cbeBlockLead\n      }\n      forgedAt # cbeTimeIssued\n    }\n    startedAt\n    transactionsCount # cbeTxNum\n    blocks_aggregate(where: { slotInEpoch: { _eq: $slot } }) {\n      aggregate {\n        sum {\n          size # cbeSize\n          fees # cbeFees\n        }\n      }\n    }\n    output # cbeTotalSent\n  }\n}","variables":{"epoch":1,"slot":8}}' --compressed
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "data": {
        "epochs": [
          {
            "number": 1,
            "blocks": [
              {
                "slotInEpoch": 8,
                "number": 20578,
                "hash": "0edb3f4a743744b907a99c51fe75ef233e1fae585c758dcdd5d32c979b52ab67",
                "slotLeader": {
                  "hash": "44e51b81adce8430a63c985594cc82b8908e3ab96a4d6b2f33259829"
                },
                "forgedAt": "2019-07-29T20:22:56Z"
              }
            ],
            "startedAt": "2019-07-29T20:20:16Z",
            "transactionsCount": "305",
            "blocks_aggregate": {
              "aggregate": {
                "sum": {
                  "size": "667",
                  "fees": "0"
                }
              }
            },
            "output": "152336265877919"
          }
        ]
      }
    }
    ```
    </details>

=== "cardano-rosetta"

    !!! warning
        Not available in Rosetta!

## Genesis

#### [/api/genesis/summary](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_genesisSummary)

Get information about the genesis block.

=== "cardano-rest"

    ```console
    $ https://explorer.cardano-testnet.iohkdev.io/api/genesis/summary
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "Right": {
        "cgsNumTotal": 207,
        "cgsNumRedeemed": 10,
        "cgsNumNotRedeemed": 197,
        "cgsRedeemedAmountTotal": {
          "getCoin": "38060000000000003"
        },
        "cgsNonRedeemedAmountTotal": {
          "getCoin": "3939999999999900"
        }
      }
    }
    ```
    </details>

=== "cardano-graphql"

    !!! warning
        This currently is only a partial mapping from REST.

    **query.graphql**
    ```graphql
    query getGenesisSummary {
      genesis {
        byron {
          avvmDistr
          nonAvvmBalances
        }
      }
    }
    ```

    ```console
    $ curl 'https://cardano-graphql-testnet.daedalus-operations.com/' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: https://cardano-graphql-testnet.daedalus-operations.com' --data-binary '{"query":"    query getGenesisSummary {\n      genesis {\n        byron {\n          avvmDistr\n          nonAvvmBalances\n        }\n      }\n    }\n"}' --compressed
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "data": {
        "genesis": {
          "byron": {
            "avvmDistr": {
              "CWJf8Kl8Gp-DhcWKuhNRUU9P0_CVI2LmpR1MIMxVgGQ=": "20000000000000",
              "h48-GEVDKf_0_vGKzmGOuAOhhIm2uc0OEDSNwFayV28=": "20000000000000",
              "PO1Kz9bpAowfWD8u9Ial2OkmxDiw6bK_ICDPvuHshJM=": "20000000000000",
              "mqJXwreGLRzV9a--egcVvKN4hzIcNUULsXqcPWe3YXI=": "20000000000000",
              "ENoYC3dNAtKL-lvjCTZDVhQYmfyWVtI0GNbz4QKqVdY=": "20000000000000",
              "o0O4s8YkitBZPeZLVyjn8pjtpBoncr-H9mbtAJS_KfE=": "20000000000000",
              "1XEVfDyaheIAeQICkHlwmvEuY9A7E50hA1v_E_QB3Fg=": "20000000000000",
              "OKVfmKrrzY0-10uxl9IxlYA6CFWwOU1dN-NyUI0bobU=": "20000000000000",
              "LYOSBM00cdDToqHepveoat2SN6vdPntA0nSFXRch83Q=": "20000000000000",
              "3Z-Z3rLCxLt0C3KgagBq3wOXrfm68_zh2st5Bi6Covs=": "20000000000000",
              "VTx9H6wpJNMC-H-pThklJ9uCllwqXaU0WXHcVTEFAIU=": "20000000000000",
              "beyF5mz8icvrBvM-mvQzLfbnHCmxOOg8Z7kJs7nySZs=": "20000000000000",
              "rs_VPO7SNO2YlSY2N881xHFeBnW_Sn2o4uSfuGpq9cc=": "20000000000000",
              "5RZPTI9FoSvLmiXjKYtUdkrqoMg99tIw8k4enSB1qlQ=": "20000000000000",
              "QRBLXNJdJCVDjbwPvQmg_liOcYIWfvoKf7Ns5w_RDvU=": "20000000000000",
              "YG9mVGb_MAvTSdFgOUV8JbRBxnj3sPELZxQNVup_X18=": "20000000000000",
              "7dEmv0hdv1a7imviD3q3p9pFBFMC77Byx9oinyGwdIQ=": "20000000000000",
              "H_Qs3m89FGw8QxVTUGuPjdbOPQyP8vzcD8I37BkRAXY=": "20000000000000",
              "pa6NZ1j_bs8kezg23cUQiba3UyLxLiGDQfDXMQmuPSE=": "20000000000000",
              "Qi5VCXOUdP-U-Pd--boAii_-gwMpB0IRfibzxWEN53s=": "20000000000000",
              "cLJrI380JOISi9A14PYTRmClcxPNQS3_GIZdqcXC1JQ=": "20000000000000",
              "BVY8wsqEPXPQ7DPTvnvat7GRiwxFPHC4jY1x3ZvY1PE=": "20000000000000",
              "FBlBD9ykcwuFmoogsVjRCUag9xpkwCAgbYDNazT1oY4=": "20000000000000",
              "KHwUZRYdwRs5UYIhOO9ycjucY8WvTzpkZgZ4PSbDDPI=": "20000000000000",
              "ED2RmO7Wfad2p4gxyzhr4gqlhavksgHEg1acZiKMQF4=": "20000000000000",
              "BOjqPWCmGewTKqKekH0HSgpnOEYT8g0qV4t4t6Nj7-c=": "20000000000000",
              "b-8mgbBV-r9bqufItyyPp2WLitNhBjaMAajl3GfteeE=": "20000000000000",
              "JW_kuEdd0TkJEUA35YUbf_K9C4OlpZd83iTUqrD0q0g=": "20000000000000",
              "IF7PMOFaXdwztrj1j_yx_YBafdZpb3pc5EF6y822KgI=": "20000000000000",
              "r9HNGwms9l0cMBuT8CznPoadXKbzLPceo4-vDydXUwE=": "20000000000000",
              "rM0RFpsVm738CCDdzBhrEz8Q0CLIqiBKMl5rtdFlWmA=": "20000000000000",
              "jFyHtgHVcd70V1SZ7O55mo8yvYw5KsijV6fMQEWkJaE=": "20000000000000",
              "qWmtDqvA3KZyySJLRPFPO2TiOZh3Dpv1FCJTdEilJvc=": "20000000000000",
              "5YYkqAleNp8VXGYiD7WsXvWZQwybkqAUR9xMf5lLvzs=": "20000000000000",
              "bi2YeYdi2W2r7IDYToq_N0RR1k3z4GYsRo2xddi2Y7I=": "20000000000000",
              "vENWfzLRm1wwHYWTUIOznVCXC3ISPt8J6n3gYFrwfpg=": "20000000000000",
              "G71fGc7_2IoExvY8VctjxMCd7lJsTWgvWzg__lF4qcY=": "20000000000000",
              "pFzX1lvX3LPolGCv8TQXfWAZupMZEjVlMWmT7nUV70I=": "20000000000000",
              "pE7umM4kIaTYqKOviWvgTb34xky-kpbN7VvSzhOT8wo=": "20000000000000",
              "P6J7kBAlCPD4wxAnzuzlqTBWMyqI1zVkqVWM5kKoCwg=": "20000000000000",
              "CmwjLeSIEKfLzd5ks16QoGCtYNc--wiGsgWWM4OKsco=": "20000000000000",
              "-RTrh8sxu9mOYYpcfmyod8-v1Z0nqxMunwK-_NFDzYQ=": "20000000000000",
              "EnoSKFBfSJ_l7RXMglXTSolDt6VeNRMay4soxkUmk0E=": "20000000000000",
              "VzxlTAQWmOH7ALIXviuXH2pjjYmtW77r9ypeEZlg4mc=": "20000000000000",
              "93QRpWUE-Yqthy85Y8poB_WeWdG8A_9nnq4HJiMfJQc=": "20000000000000",
              "H7YIN-FF0kUawzVnhQpCoMLuOJkLzYAtZ2xof7vhtPs=": "20000000000000",
              "a9_Q5Pzte2G7wXujwVBaAzLcMki6_UjKrxvWzayJ3gY=": "20000000000000",
              "O4h-7y-2Izq3ojxailZAbMd0VCq78-kIMv8gIcaA6cA=": "20000000000000",
              "mlzqpS0hE6s7apMGRQP4Cx5Fc380yt9gQX7XVcVrmQQ=": "20000000000000",
              "aK-WCeAwKHQE9H02zvRLRdoMPIsZWiOfKkbA6yTyMxs=": "20000000000000",
              "b3fq65eebunM4fM3AQubawjF6Mt9v9jyEXF5f3hewbo=": "20000000000000",
              "ZbZ7v6OcrjZ3vqPFVzaHOK2A5UzRYy-wm0C-ngebU_s=": "20000000000000",
              "zXza46kY7Gs5cJEoN2TUwAaXth5W7uUKQfNiaPyWDSc=": "20000000000000",
              "5q5RzTcpFFUne5AKkua3DJO1IFQEOGyb2jQvV4DmDT8=": "20000000000000",
              "5CzeQFC__uUi8TRPuWVgsUeJauYR3i1f3rvD0CrC34o=": "20000000000000",
              "KOp96-E17RXmCf0_vEOcecpTmY8W0wpbpBwFuPwFbKM=": "20000000000000",
              "EFK3F4mO823aCcko3QmFJ3Klm7glGs8a0f_f6WVIwdg=": "20000000000000",
              "e4TOcy9Qp5BQ8tXkEXaWpuHRmAVcJRfPEV3sVCOKZsQ=": "20000000000000",
              "w-2bM_wksghmtHp4ZB2ZOQ-V9Dw1ZivS7RwxgY-DsB4=": "20000000000000",
              "9pKGBzQJLoqY6vcOM_OqHRgq9KIdO3ovCBp1mBEFKek=": "20000000000000",
              "TMFEEMjP7q64-Liae7CEG0ELKtEC2e2vDuCpMItyfzU=": "20000000000000",
              "nrRREQUC1zKTpQRRNDzO-NiQh6DJahitvTk0SWLkc8g=": "20000000000000",
              "9xnVxPVNI9fdN5zGa5Fa3HcIwof5T-2lMbdLh3_Nbpk=": "20000000000000",
              "NqOaYkD2B5yTFQ1dHMY7X2LmV0Q9tZI6KYR1-dFW_z8=": "20000000000000",
              "zhXX6D5r4CDjlLLQiC87LZZL2zWUIYaXhxbcgq_Ww3w=": "20000000000000",
              "CXMg_ROxPjiJAyxUpBlUepLDhcdhMffR89izVr9vcRU=": "20000000000000",
              "vfTTtqpmg_3jQ7zWV3XhNfmtbtn32Z0fcfNc6_Cx-4E=": "20000000000000",
              "3UAwyThFcR1vKSBpktCSkJg3NpMQhL1z4l46NHpfJkY=": "20000000000000",
              "K4m8Vu1qtFRavgx0jrctVErZf6VbKDfBnigjQef6k-o=": "20000000000000",
              "rmyqI_SuwRsbR4rG1Uk6bUlSJRvo004w5SeejGQERT4=": "20000000000000",
              "oo8sxwl7TO2JP-QfW3_aQbE9zZCLBogFPnwEMPUBuYs=": "20000000000000",
              "lif3znin9EWfZBoYZ9Ta1c69eINSJNmaqkKJVpFuF2U=": "20000000000000",
              "kMAvVvgk0sEf3kHXyfb8gQ6H4gWaFBDSUwms4IFs2jo=": "20000000000000",
              "8CMex0Km9bk2J1r4FaSD_FSwlGRgh1e8C1-7fCGUAJE=": "20000000000000",
              "pUliJY_tq41pTdo5VJISdWbGGBnL_82pupm4AjZF-y0=": "20000000000000",
              "E7dg9_nI8tY9CXli9OyIHtx0FUsq0QZKLBVx9Cr8Daw=": "20000000000000",
              "cpO_GBP5qVwOCGxws6oGvgZszu7jy_LwbKZj_f2pg8o=": "20000000000000",
              "n8MZ16U_jB4Vg4BPZHGorDzVO7dC9qOfAdYhAluKD4Y=": "20000000000000",
              "hqzkwiRusxDSgY-MyqmCyTC0VxELSFdJKJVpzBGOdQM=": "20000000000000",
              "QCiQStlI-PWCbwclsM8ZvfrmP49kql7lAwJjgzZ_OvI=": "20000000000000",
              "7prDyNFRXierLX1UE26h-TSO0fmfC2lFHQoelogU3hg=": "20000000000000",
              "32mL1n7cF_xLIjWAGTC6vISGcafcJe0EgXaxaQNtrzQ=": "20000000000000",
              "H94Fk3T5fiEXPd1eGYfpwPP4_y9FVQWsi2bhIAf9hFE=": "20000000000000",
              "YLQqBDTjfVrQJcqCzgn0js4ScjJpbh3_dGRmg_wQ9WM=": "20000000000000",
              "Ctfg-00LO1JetbfqwOOIwrm56xdZSzzZRccGW52eCps=": "20000000000000",
              "GCkRs5Iqi5jyzRhF-Z5B4-EzgCRAb55pJK8a3kmrbwU=": "20000000000000",
              "975KNlfAi1B-u2Q0X0qBZpuZNLCUNnKnX-jvBzah3pE=": "20000000000000",
              "SrxXeNRxF0accD3dsKoj8ymSQeJoUVs78Llsy-4ZIO0=": "20000000000000",
              "m5zF99P2vG3cqGrG17VvRti7d2XRi3fuUHfec-jM6tQ=": "20000000000000",
              "A-w4r_kJIZpI8TqDAY-F44cR0lhZtnB25UhT0NXM6Zc=": "20000000000000",
              "7mglsAKSgUEkAyA6C5Ni-v_1xoGNPCN_cj7ctQZGZqg=": "20000000000000",
              "x3DrzZ_Yp8df2EGsGlwNAnclV2Tv3lcdqnI3Lk_0bF4=": "20000000000000",
              "GWoauU1tVb37fjs6mPrxXiBoy9HarPqyGI8zj1mc7cw=": "20000000000000",
              "op9p-7Xy9fBmzrjLbMD1jEuW1QVXTOsXSIwgaFN3sDk=": "20000000000000",
              "KC7yE_m_JSiWGVP9cpcfYTLF77taAPTgveRKEiIPg1A=": "20000000000000",
              "6V6sxoH3dMLw8vWcH0NQF2SZNPDzmtULTX4vxkeCQd4=": "20000000000000",
              "y0DLZDhvfU-M5MvdhoyxEFp811PWFfrAdIfDVhYCMzE=": "20000000000000",
              "wCHhA7PS7wdfEuMpzrOJfdGyF2uIChR2LnnAcQE3hHI=": "20000000000000",
              "s4iYnscFXdEK56b7o4ZvKpgN0YoKchpR-U9MZEqbGb8=": "20000000000000",
              "WkMPzKKtocKcbc7_fsFND1oln6gAfWJspg9REi75pMw=": "20000000000000"
            },
            "nonAvvmBalances": {
              "2cWKMJemoBajGgvgVVziaKmUFa4LwJnAHffmuaSJBMDqethwJVQsyBsTSfFhp5jFpkVQM": "5428571428571429",
              "37btjrVyb4KEg6anTcJ9E4EAvYtNV9xXL6LNpA15YLhgvm9zJ1D2jwme574HikZ36rKdTwaUmpEicCoL1bDw4CtH5PNcFnTRGQNaFd5ai6Wvo6CZsi": "19999999999999",
              "37btjrVyb4KCRtni6YrG77RLPosnDqtEYoAD5xLdKYkWgnLqGa8yuXDUQd3psHrfxqaRcvNTsAW4ngUe6bzstbzSUJtwoaKbYaL8zjFAJJsZkQ42ti": "19999999999999",
              "37btjrVyb4KGDMix4Uj5opvbMDgjZYUjeARAqTEFEbgLUH3qyju9gkBpcm2fVWgkcNgK3xFsQgWm1w8zxqvm9P6xJj9mHqLeMJPwDMUKUGPcDyUaDS": "19999999999999",
              "37btjrVyb4KEkSeCVx985rXc38DCud2AW4LdasNmyoPLbtDGcDCyYVdf8BzxvDnzPehv4kyVBkzThjVEkSpGTv8PGQs4yRUgiCaKa7PTtBY4ohNGqR": "19999999999999",
              "37btjrVyb4KFGS7upvgJHtmp7y7EFB67utzaHf7PM8y8U4tNkpmARNwiD7seN4NSAceHmj64KLGgh9qn1BpYF49NyWxocBHn1N533qBUYfhQar9ceu": "19999999999999",
              "37btjrVyb4KCfir7GrvC6Y5kBNjeakZNd5po62AzQQ85SGkBB4QfXibC4fSNK5YvNeVgmPc8WbEeSUHRjoiqhJ4HDtinK2deBHSdCH6Cw8k2u92rdh": "19999999999999",
              "37btjrVyb4KGAExHTQjLUHJBksSXGTomjgNsw8a4KepCgQYk4gxacKb84vGpPSv9Pjt3gdgMjA1nB67Pq3XyJpTDk8kLcXpJawCe6SCJf5jUowvAz8": "19999999999999",
              "37btjrVyb4KCE1qeEoUh9b8CpcZcJ794Di14AxAELGoppJNVdB79nnuKcgRut566MdDkxTqravFaDSD9iwAvDByUHi59xocCY3ButEjmCQeLTLZXQ7": "19999999999999",
              "37btjrVyb4KGGSGD8KgQD6qUBaSjxy5JRtsmMSHEGGAZqA29ULGwci8TcM16vBhywuBw54izQtpAqXeyUnbjh56hCgoqGZp9tHTMLLkEgLzwxVCZ4N": "19999999999999",
              "37btjrVyb4KG5ZZfwwiQuhAGWiNJ2FhXP3oAuiq75qknCz4CZWNMVY4B9BmiHRHnWfhUbkLHUqfabCYASUk2V1qGuDw97x1gdf871aFY7Lpz3N1NvT": "19999999999999",
              "37btjrVyb4KFtDHT2vDtMvQbLBgfH5hnpyVTTqqpPsieykukuxrDShHNccAEEj7M87UuV2GJ5pPA7YJ4JPjSokA99XaDgLmeaAumhZPHMwzg2Laspr": "19999999999999",
              "37btjrVyb4KDHFyvvKb29RD53ebt6N8kpbL41J4VxWpiFC4FnxxybP33M9tBbdqfMXvSvyTQpv4dULXf5B838kEWXSJ24bpHtFgcbRkiHQwqWFQ5du": "19999999999999",
              "37btjrVyb4KFh7jhHCtWxW942ceq7Xhxay8FZ7GkEBezGyFm3wJcVBGy1YYJDZ4Z7GbrFZmHLSe47zFs8Rjxk8rveoRpo1s43HXrMrhd4ijim4jJVP": "19999999999999",
              "37btjrVyb4KFhYgC9Lr4Se7C1gL39d5WBVADyUyQZz2BfG4BZxczyW827JRQR5enyWaoj6NnA5NyKsheV6Eb7WvQtbN8D6116HTknHhEb5jh1yUU6Z": "19999999999999",
              "37btjrVyb4KDLtM8HUJsBwergjZUj4DcMfkFmbV4bXUFGJk815o9nowX9ndPPVAeSNjAFYqJeFwTiMa9Ka8LqBnqFZgPpacyx9LrQLoXVMjvvLB7DK": "19999999999999",
              "37btjrVyb4KDec7E64byKc4XjmmCRDaTGQYgHJTPDijZVr7NwZSP8g7ienzTLx5Z1quaQRhJqqAyV8Z2QdkzXvjTTRiVDCqps78uGp3uuth4wEJKP9": "19999999999999",
              "37btjrVyb4KF5R1LEsaQgjWFWXwbgJ51naDEaCRG23KiAN3UtGzaT5PvUANtFBgjmcCtPLMBYMTGL4S8px4HyQMLAyF4fakYoFAJC3PkxCWMUatGWD": "19999999999999",
              "37btjrVyb4KFB5Tmw1wsLmuv17Q6y8i6HGpVxbW8k4bevmob3DcdbH6jzrAtUrBpKgfTGgPMpLAbJcpaByGGJErkXQWFwrNMW35S79hxFvAN2GTXVQ": "19999999999999",
              "37btjrVyb4KDEX2XToMQoi1No3YdREgZWzrf1xQPbfhbZTZnprFwDsRMiBxqUrA7p4BwjxXHDyqAccPwyX8iWWquz2CrLazJMR4s8AMz2US1D1ffJL": "19999999999999",
              "37btjrVyb4KFTKCoqtZBdbh7LtJ9mRR1nbkX7ggP6a7AwvkSDxUN6U5GJWfuRXnL3a5x5e16uQwyjC6PoPVQ7VLdJXr8Kd3eFknLu6NDf2ey4AaJo2": "19999999999999",
              "37btjrVyb4KCHCpiGd1J2GtVjP4KxEWP7RE6K6yxHzE97cbDgFD63fUFygbni8jKw1N37nGsT43KBvBn5w9ee8sVegr6Tg8fAr52mUkhdvTZYJV52T": "19999999999999",
              "37btjrVyb4KGLRpX3uQfSeLovpMTcWfVZSM5RCufYvy2tyCMwrLXyHKCM9VqQh8dCQA6WcTrViaxqpvBSeKreHFL4CftfJU1z7CjHAze236NLesbL8": "19999999999999",
              "37btjrVyb4KEdwV1MS3Pjek1HjLN2CSq3SJZGFBbZctkGLz569i9RWN15bAvCcZ8R5dgEi8iYjpmMVKioufoGv3issZQvVtzPz38pWHBViyRK2how5": "19999999999999",
              "37btjrVyb4KD1x4cqHfGxrvBubZ8pSM8Jmw15UiHpy77eMsqpMewGND2GdvAwTBZhf4KA4uypBJnuUPbPYFovpRVJ92BUaMBHfQnAD3i15DAzD8EvL": "19999999999999",
              "2cWKMJemoBait15xg1M73WAvWafoieg2GrcykbRk6J1QC2jMUXn7LpXf4mk5RUeu8qYeG": "5428571428571429",
              "37btjrVyb4KC3HyNR82Bj2Sr9o6CF9o3J5hBNycGb9JwrHggTYUHfivi87akkYDv8ayepMkM4mNvxTKvoVdMHFkMnZZgrk5qobwPKM8idSnYYmvTRU": "19999999999999",
              "37btjrVyb4KCmCLYttFEWLNQc1MRbV1NyhhssRioZ5CgkqHgYUTT1pPSr2hrfevSe8bSwLiPsLnaCbsxJQc5SWgWYEJDPWuUA1s4AotQxERNbT9ReA": "19999999999999",
              "2cWKMJemoBahLnFCQ8wrTuZ3sMyiCeEkUZDYLNucPiVTJr8UU3BgADsKtqosDYNFXzeiw": "5428571428571429",
              "37btjrVyb4KDHDPBVenqrh8tUTVNYX5ZGwjd4r3svqdnwWicGnMZZV1E7nBJVQsDY69co936H9onHpmA3PYSabYH4ibbULphL1CitDgArH9KknBARc": "19999999999999",
              "37btjrVyb4KD9Z53z9qD7gTWMHr22e8jDcpCHgJFQaGQsvnNkycRehAaxLAnufNRjhLzQ57XVGJnR6mcsk6MorapLpADT77tyTaX9xfUSZyTA32ZAy": "19999999999999",
              "37btjrVyb4KCsozLcUUHR8GyVG7erY6j9zehKTADn3e5xpRJtu1YgfJzSmAyERBHUXa5LGWY2aR2KqcssnRjwugh1bGjxc6U6ZrePJnALYTw2TR3yh": "19999999999999",
              "37btjrVyb4KEDBSAmNtUBy6pfXesvTvtrDZQsSYcyo7SUwjLkhoSaPDCsNqMmoGbqzFQyEe9DNwK59BMudtdkzFPBpbgiEWx5SZr6vVMbpe86qsQJV": "19999999999999",
              "37btjrVyb4KFf5NQ1DuNoAP4phRomqdEUmtFb6sWcDHkizGj56dwn54LfKrfWa6Er5sxDXYrzpWwS56PKmKaBjJtn1JqN67K3CihFXXospn8B2TDz2": "19999999999999",
              "37btjrVyb4KBPHxFJqCekpPztGnLgbVsUA46Q8Lj2LKbFJL5Nqk5LgP2u28eBJAxkkU2r118ARdXW7fXLPQgctwK22L4N3zc6XeoDqkadGmTd4s8a1": "19999999999999",
              "37btjrVyb4KBYe4RSCngNCgVMAeMJkRQqoJs8t6t9e9BHcNvvT6awv4CruMWH2FyiudxcGfZHmjghDvqk39iFrmCt4XE2XDuYzyo97BxwS6MngfeWp": "19999999999999",
              "37btjrVyb4KF3MxxJBeJqCPFgHyUsSDkrDqoctSSVi9h7F4Wj9zFKcPVuVju76KYhdp7nJhy44512Wjhw7WH4sed3MMSh1HYnKUfjZXGkoZXMajaye": "19999999999999",
              "37btjrVyb4KDsi9fc3RfExWLumjkp2YrcMjZpew19Z92kZnjPy5Xa84KY2WZw6xmjJA7AXFJCBWtrF9RFw1BjCewEqK77CVYj2s7bk9aAA7yyZARRz": "19999999999999",
              "2cWKMJemoBaj34AMeqLspGBgX1PVc7z6VkALK3rtVd8iFgCtMUenNoHhVRnjeGfYVQJM1": "5428571428571429",
              "37btjrVyb4KDpppzxzoaPgnstPejNxGaSZ3Vh22Qd2DWGrwfLJ2tizs33Y5Yjya1U6TXPzVX2PT5g1PXMy4jR4aWZRGZqYJk4Uw9p1h6BhEa189eiN": "19999999999999",
              "37btjrVyb4KEXaPVoMuKpnVEKKLMFuGSvYL7YMAD529yxeK8Y1zqnbh5FQ8GMYpJwARugWmzaXdJ1gopgsxziC4e5wgjf3zkp7RH41KTJ73xLyfrFP": "19999999999999",
              "37btjrVyb4KBrhG1yaBVY3X1ZoTCjUH7gbiA4qSFsMGgtLUBgwHiMZPiAJ3kQrRiboPV3s7eYXZD9fnd27qRb1cCEMc4oU57aPn2cYcEdAEJHrsyLB": "19999999999999",
              "37btjrVyb4KEC4vC63KNqRBD7RX1KBwWDdPDE6oXGxP8x5aKrbVTALaq4XBdak8F47Kt9VcsQvVsKZfAit8vBtZpG2mc6VKUXCFv8pTYWwQMnABckB": "19999999999999",
              "37btjrVyb4KFPbetkmdvqD8nLRFsUVL4HsVUYmgaZhAmBXcr78M3XoZkptjuszd2T1FNr1fGZApkZFZXikGtyhCc7jH5JYD1q8csTNSWQn4Us3nzX9": "19999999999999",
              "37btjrVyb4KDfwNJcNMYsyEDVHWrGrNAPBwF9FGEnm5j2XFs4BeGdiSPqPtqCjWCvcRYBfY5EoDjRBhjsTrr2HjB1jw8XZ9Hy8wd9gz4KkCbMugSgf": "19999999999999",
              "37btjrVyb4KEgvmzjLT7R9xHg1vNob6vCf999UFuG4qfTpHGZufdhbkUogSFJXtQXnCcJDHJ6xuZt92H6VgxGdhSLQ9gmWZy4zCJEVu8Nj8NashVQY": "19999999999999",
              "37btjrVyb4KAtY8hCobTmAB36dzosSo644ZrzATKQhP1AsnM6BAVfTWwMX5BGXhigxLm5hk4beodymyjivxrH7ZY6BZjMu3AtafB5guvagxEZM7vJq": "19999999999999",
              "37btjrVyb4KCRMJDqQ3iK4M19XhWGWpFbCoxjgeDB7ZqUhgW7jSYLEk5oVL4okVPVx5rXCgoK2ND9kAWnNU5QncJp1qvuCngRdJaLrvFwp4boE56VR": "19999999999999",
              "37btjrVyb4KBTRHUnz17FQNFzHR8PpoGGwuNQZauAUxmvTb1o7Ragv9Zvyiv6Cb3rnrmYY1PGtVFTmom3TGg4mK5XpkRyf7PnnCG5EQM69i7MViLpU": "19999999999999",
              "2cWKMJemoBam1WPtZrz3Fi4EMUDao74k9Xn4fhyVYLqK4o1VRzoxPFU91QBRToLbVyrjX": "5428571428571429",
              "37btjrVyb4KBUH8nDpwtt3sSc6rJ7AkYjmnqvt1tJ4J8ZKCuqPrEuEioJJZXeD9aohveoB9zhRWru6oM5zyBcgMtkA26HLtTDKsSVwzoqugfftbiPu": "19999999999999",
              "37btjrVyb4KCgWqZ8sJW46mQGdZS9wKW2TEQesyCoRScUxvisbdvEkfsxYLR49i6wE6uP1BBgPX9eg8cxKHPuNyKpStwf5UVmRCXD2ahotjamotMwV": "19999999999999",
              "37btjrVyb4KCR9vZcXetWv4QdP4jPSH4msGeXs8DUBUMVYJHgD62etfv2jiD7gmLbLezCAiGTQu9JvrHd9Wfu74wguKgkX1vCUQkzcWsn4rVWsKCyt": "19999999999999",
              "37btjrVyb4KFV3TiBqtLDN3oHQr5NrA8ouAAqasFU4ZuB9W13xgcgWsSy5fUtbNL4imCruQz19hjzBzykxGxCAarrviCUBh3sxWbvvTTHdvpyWGgXc": "19999999999999",
              "37btjrVyb4KGM5rFFreGtZAs4PFB2Drb37uXRHebh8rCeVWFkW8De8XAbYqvfQrAqVthfJp9Qy2YzbzNhWSiUGY3D7yJkRkChyMveKCWT8qUTNEu6e": "19999999999999",
              "2cWKMJemoBahEJS9xuB3R1ofSgtG621enmfpxfx9Unpo2K26wJPioaA4tizZrNMACNoQb": "5428571428571429",
              "37btjrVyb4KFGV8HUDv7S5E8CSBV3pQLgGFt5HXa2jb9ofbAo3gTxcaQ4So84mHsNk9mhAybq6miH2VZU3iz7cqCd74gPMyn3zdUsrF1u2rib7HSXq": "19999999999999",
              "37btjrVyb4KCF4JTyyC27XuFmcrn9Nxj1DmM4G5BKnf8F8F8BSpvn3PnsLRNH2RZJoajmg4yqHnwMXpUnbb7sFSuthjXv6YUenX8EWsApQUzAm77Dm": "19999999999999",
              "37btjrVyb4KD5zMmtnD3jWpX3TSJnZJ8jMzCFQHYa3HcHNXxdAnK5A88SiWncRpJQxesMDrYgzPHk7SnFNag5teaFELV6hE9opnJpJzMGpVicDDRX4": "19999999999999",
              "37btjrVyb4KDTABtj2RScLVCLVyFhxcURYUZNVta8CghbH5Edz32XSP79NHc28QTkKLMNUBupRnJXs4zcZt8C2fiPFGZfgSBMqMGMidWc2zo9piRb4": "19999999999999",
              "37btjrVyb4KCGn7x5G6obn9NPNoTuv25LrBcqK9wHCm3XbrhxqycSQrbPfsDxgDp2M8pqTjCk8cVEG2fRxWrTjfG4q71MtyMo6nt8WG11kJzdQQSNL": "19999999999999",
              "37btjrVyb4KEhnv3cCqP8jzRBbwE5v6ymPkBjTexHCcCgYJarjHHaxipJvz4aaXc5Xmp5KXxnC3SoE1oqR8sdGHobyfsAqJy7DwejZWpkkoYD7LJsS": "19999999999999",
              "37btjrVyb4KDvKgSbCTx1gwwZFGe5DZaXyGwTYGGnJNCQ9C61XP4n1pKFQtNbYEowGeRoKHCGUvuU8Ebz2vQwN7YhcJ9bSb5oNpAoCe8UxX5KK5C3e": "19999999999999",
              "37btjrVyb4KB6yr5YozXGqSKemHyZfsgiRQFX3VdJBKx7waoSaScNWc2dNvhNp6HSnXMxUwDtBvicXDWdpoJ7cKLWAwqEYki5azdt1qDP4sHXh8XhJ": "19999999999999",
              "37btjrVyb4KCqRLQRj8svdZGGLQDZGdXztzeC15Vvt6uZWg23QAdfL1dMc52dpc8jqKquWNj6xjyLnLciVnRxzEq1kiq54ssmc6h7V5xK1cqqKJWBT": "19999999999999",
              "37btjrVyb4KEW3PG2LAJNwmok2H6i149HetvT6fYrPsqPGpUkucNkA4b5TQmv896EF44UmcCAXDycfxFB7GcVefBCgk9cZffVUdX3kri3i5TEqSjKm": "19999999999999",
              "37btjrVyb4KDdgCo1a7URfpQVFoTJEUcn4LqWpAtCeLaW7NeGMJtecsahTJM7886BjLcnhU2CboLSUojCPcab3WNTmXFDrRMHMHdmWefCAyYA7QaaL": "19999999999999",
              "37btjrVyb4KG5vwKTPpCSQ14a7Cv4TESosSVoFVRHsw1vHj7Tpb7N4j1U5dtFcY7L3MWsH9BJqQjU9NxwaHpbHCJAhmsLi2mC2BE8k8Rj7zcjiiVhR": "19999999999999",
              "37btjrVyb4KAr171Hd3fu65bbtKxqwktHGwY9kNanPYGXQcFKA8d9Hp1RgLrxzU1AdJCJbJDByTnHdDmtA7pm985tKK8hr5JdHPXFfSvkkYZn3kb9G": "19999999999999",
              "37btjrVyb4KEZWqFxGYhFuLE23i92B8BiLRwwFUdnjmM24KHCNuWik5dc6MJqz7GxupgKGK5zzbYSXJyA8DQVDszyFmJuoxgzPn2GSnfBoREZ82ZdF": "19999999999999",
              "37btjrVyb4KDB6sZamEoJWYBLoDWucRdDtRXCuQvLCoVoHPNjrywJKDz8PQg65ZtLvYvREwAK9oLzGGb6UcdAf9zwQcFaKRRHhLzCZxwTsRDVyPkSy": "19999999999999",
              "37btjrVyb4KDJc8Af5dfJY2jcFbtkofFL6qXBxWnk2kHzCE63qAQR9Ynnk1XkfUrcnBrN7EEyvxmUDEFdNFfZHzXKhmkSQht3b6Y5rHHmuFYYoKdZz": "19999999999999",
              "37btjrVyb4KDjPpuxBuJM1Ma5NGBqriSEAcozZMqYkWEkoJ3GgR8MAy3Zeb8q5BvtsWGEpSFQ69znPuaX4kVCcnEiEMDp91A6EL8A5YM12cpYYogLm": "19999999999999",
              "37btjrVyb4KDmCmpc6FutA3PbQmWxcsbzZsFEdMKhHxrJVtGkmaiWd18dKmiaRA6o4Y2sAjDwjFozKzNzQg3dp8CXVSPpWgDLAXaozoRyPanW7UM8B": "19999999999999",
              "37btjrVyb4KCLfSBFhCBdrb72YBNJkKcDCqpdxf3k7iwJTj7M3txxiq3fcam7Nyi8sLcJNSfgnUrh8C7RKEMN5wWpku7HLdZqZVuPRjeihhgXmEpCe": "19999999999999",
              "37btjrVyb4KF3ZQhHTvVH3L7jNYoZ3XWK6SWpqZKiu9AGz6qNtxoxhAmmJpenFMA6fedYDT3Lt7cihgc1q4pE2GJXvPuknAkjvESmPxhhzkBzuiRis": "19999999999999",
              "37btjrVyb4KF9xNLdS47pRBLB8e8bQLuvrBiGJbnHPNCbKU4wrMerJztEtYJdHayvaoUEmJ1vc3aiq9Z3UgP83Y1b4rpiyrGeYjzQhhDgB6DW8sWJm": "19999999999999",
              "37btjrVyb4KC4LdZLvrexUgAntpySomDAPVwdMEnz1cP9pZxsxZqVYzM6zPPWAhc7byfwsLdgW8GEMTuTUagYFAKEmYgaDmYxK7cHxtWJG3hiMHxVU": "19999999999999",
              "37btjrVyb4KFfXPG5GDEc4tLyVUSepKD9GGXZcSuP1xtLLRQnQr4wSXawT62bSJsiPzS25kdADKh94V3iDksm9nq5fhV4jixCnpNjsn7k2hSkwrAUa": "19999999999999",
              "37btjrVyb4KC1L9MsZ8htPomWp4FV4hULeBVT6jf3GGqmDcw3k9tPn6pnfqGTWowQDvqVr7BqtQ5rcQ7gc5z41qh9vyBV7Ds83bRsndDbTAwkEUJ21": "19999999999999",
              "37btjrVyb4KBtLs3NwnoLkyVx9ZxSoQ7mQx2ZvzanG7PGWWdMA9ksXqZxakLxdj9MAPKw7eQoZJWHxJJCsd7MeWj7ujfXtPsthGhcURT3Hste4Kr4n": "19999999999999",
              "37btjrVyb4KCaYYFDdnbHEdBzpDPcL5iH98AGF8J5avuweDHwKChDq9mBvLKVJS6F4YTuAVDigPjMnAcuYUJ2UUbvDPePFZBhLhBrFUKeauoKC8X5z": "19999999999999",
              "37btjrVyb4KFfnmiGpxNSGQVMfmFpAFrEAEhZwGPQDutSHnZqQXPhcXxDNcbdoKiztyQHpTA3jSmVowZCxcaJMS1k1wu8U6nXTzejgh7wYZjycamU6": "19999999999999",
              "37btjrVyb4KCTVE8b2UitH791rYrkSrHG9u449h6JHKotuPWRsdVZQfP1jXrs4ygSxAnG1rM5mGFM6cmUqA44e9fenjbVC1QFYyn3R4CaptVZypKgW": "19999999999999",
              "37btjrVyb4KDaKs8A6CU8Lrzxpr3WNM1kDdd4CPe66TqSP2ehHexrAuZ3ykMmhkaUZEHUEiq78ELQx5vpSFGHXFKbyGgrWa8rokqamCV8bSKiqsqVt": "19999999999999",
              "37btjrVyb4KCdPziHrv1QgXL5zMy3KYxr7zPqoRd96iz3LNrkWbobRmPswTpRKgCQEkZcEnipiNJ5UoULAc33mbR44MchdHT5vLNYT9sPxwzpgNUWj": "19999999999999",
              "37btjrVyb4KGAZJWCpicgRkb9ijP3Jnv6y9EYvnpMqkPdBZ2d6fdnCa9C97HUmfHLWF846AKjViPvnY7MbSM8mTM3VDx5RazBFxA7C7mZ9CyM48AW9": "19999999999999",
              "37btjrVyb4KFfgfRyETGNXNXm6gAxNpTQSxr6bM7T6yZE1ibiZBovxG52PioVmLRnPYxs4wYddAfgTH4mbmtLFnwuZYSBh2eNsdLdc6vWb4AdJNNym": "19999999999999",
              "37btjrVyb4KBsV28Wce4x44WsTVoXa5s4zaBKALWXxMQ2MtfVgB8LJJW34QFpvjrxKmheLtpcLyVqaeu36oB6ZcgQPppFN4oqhdueoKBEpn8dfQUVF": "19999999999999",
              "37btjrVyb4KBEKzyCGqao8GmErVER19oBH3L8xVNCSZs8tCVd53iV5FXZAZ7bNCT67bKoasGeiYZxEoDzGBsKxb4uJxStU1e9wkaPo6BxUEErZG7LD": "19999999999999",
              "37btjrVyb4KCi8WRzrkFiE1AVYbSiXBzMRTuTLv8DAchfg4tPaDgHRuDN6m4dq4VkA17pkWwCoa2NmvNb5sGeU1ZkjcqFuYrWPK2C5a3TLBfx64uLs": "19999999999999",
              "37btjrVyb4KDz8QGqk9LJ9kSsSd1zKgqfTuiTKbL92b4aDSXmRPyrFZp3VPEwhMyEmwCiSkpd7KQztirmU6CGwiphhiHoXbbZjkbfiHN5Mq1y7fmJM": "19999999999999",
              "37btjrVyb4KEcCa8yc1d8Zrne1hRtedHQQGPbkRJcvHak2ygcCndPfSqWwb2L59ERxhtqsMJLdS1fPQPs2vcJcQCvB52tCm4rGCDwUmRG51PUzjSVh": "19999999999999",
              "37btjrVyb4KEhvykuoQZKWdNKFgTrugRUSV66yr8GCXREuaX2PfVQJeuXS4h1bNP8SeUxHAG2J6RfK35YnsZj5qkWQCWV7tVYMVokU5bw9y8CcmqPy": "19999999999999",
              "37btjrVyb4KDsFS7rbQjZQGX6Fz53u76NF2hF9iqRhfbx8ePmjJKCsv5rZV1hhtP6DHdKwLAf5zkVH9FE51xYuCvJzNppSn1tgExVQJpuTwKmyTekT": "19999999999999",
              "37btjrVyb4KFtfVixJcJdD27YZfXfRM5diZFj4TEazkCAhF8KyztTbe67zQpBc1RTjruHNvefdJ9Jtr7u5rebR71tGrGtKioSSGKy2hMKd6GUVFkEg": "19999999999999",
              "2cWKMJemoBaiXc4dYCHfDyvy3LcqPebJ8zfRJsZZoHDqik2SzK9Be73YQ5W9u5jEiMPXa": "5428571428571429",
              "37btjrVyb4KFNpxUYvzqFKsRfRYJKHjEuFfgy6rpoGq29dcMrNhbKTvqNr719U282rp9PcnFonAENkUdv2nE36wZmyNkj8JVQJL1TLu25SKjGFNzVs": "19999999999999",
              "37btjrVyb4KFYnqsKFfMwj4S3PcCxwxutoiUubHazLfw4wJc5bfrQgpNEpRnGCS2UUfzvMWRRV2vy4wzPKE7tshLS2YEW219R6QfAenXzktMoXCmuU": "19999999999999",
              "37btjrVyb4KEbo96SLroMfZB11rEvztPswgdqC5ESkHXc4EaFdAhsDQ24vJK23XsjTJzpPS3ZcDWiHiFVtmp5wkJrcnRcMe74s88eTy8YLvLL7EBRj": "19999999999999",
              "37btjrVyb4KBcyGa22cYKz3Xo9UnB2kzZg4nqVX863JCAvUd54ehdg94DWwnBasCv6sUdbKdh9t4tf48oaXokeoms1HDNuegsmRjVntHBX3Z2hnrV7": "19999999999999",
              "37btjrVyb4KDHmdZ8ewhythkmGaUzLCwME5pGtWR7nPhE9nCjLMcKstQFyKq1vTSagA2BXtiopfGwtLq2e1jhUVKsw1Z8Me6XTmLm5C9MzRYCZCd3n": "19999999999999",
              "37btjrVyb4KBGJtifVyePJjSHTno1XNBeTcqmSqEhyUznts9KGQTsvCd9Hq6zM2w29njvmJYCtWmNkUXvayfqyv7epN1awWkKK1WUFQKJsjtevSKz7": "19999999999999",
              "37btjrVyb4KEwqVLa1yaRPdDUctf525DovPsxoXZfqNp26eXQnmTSFwY3Q3rgPsjRfTKHKtjFpxPy6XYvjzscccsZYfXSaL9MmgrKAaeQgQhCtoXih": "19999999999999",
              "37btjrVyb4KDxRyqtP9nWyEd7sfzdB8Xgh2egCATJAVtvxM2LhKLp1ALCE714vMCsbQZ5SwvVgiAvmieJTkae865ycwU39JN4pgt27pqEuB8uvi947": "19999999999999",
              "37btjrVyb4KDA9F68PUv9efaoQHTvacu98Dk6Zx3784ADx4SDnwMfDt3uRfJwqBELeVuis5UEqsf9u4zAD9YC82s6YNmQu43avWDqrQq9Z4hHkEVrL": "19999999999999",
              "37btjrVyb4KCjschbSccsYGDJo1rVBjdVrpH27iRtc5h1q4XRqpQJTma1NA9t9t8PrTsJjFE7WzNCczJHQR1RGXW1jDNEiEqNa6xctAZ4ZBtXKHVtp": "19999999999999"
            }
          }
        }
      }
    }
    ```
    </details>

=== "cardano-rosetta"

    !!! warning
        Not available in Rosetta!

#### [/api/genesis/address/pages/total](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_genesisPagesTotal)

!!! info
    Neither GraphQL, nor Rosetta are paged, but instead allow to set a limit and an offset to queries.

#### [/api/genesis/address](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_genesisAddressInfo)

=== "cardano-rest"

    ```console
    $ https://explorer.cardano-testnet.iohkdev.io/api/genesis/address?page=1
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "Right": [
        {
          "cgaiCardanoAddress": "2cWKMJemoBai9rXSd5wmGzzYktjNffQ6QaHcMUCtPQgzvjJbDtbLYRYzn4PN4wZTthMue",
          "cgaiGenesisAmount": {
            "getCoin": "20000000000000"
          },
          "cgaiIsRedeemed": false
        },
        {
          "cgaiCardanoAddress": "2cWKMJemoBahmj4r84YFn4K7jkJU8FJdicDp1S8JMM49PikC3FqheqEAVwrVgGvB3oN9v",
          "cgaiGenesisAmount": {
            "getCoin": "20000000000000"
          },
          "cgaiIsRedeemed": false
        },
        {
          "cgaiCardanoAddress": "2cWKMJemoBajYangUhwh3erpxp4rwUXowLmWppr9Ki6SnGLZsWc1RQptQcVqsHS8p9U1U",
          "cgaiGenesisAmount": {
            "getCoin": "20000000000000"
          },
          "cgaiIsRedeemed": false
        },
        {
          "cgaiCardanoAddress": "2cWKMJemoBajRGmZJQE4J4o64AMpA1ZeX2x1FQL6tQj2Bk9qv3cNjazq3rGoXir5Qcrpi",
          "cgaiGenesisAmount": {
            "getCoin": "20000000000000"
          },
          "cgaiIsRedeemed": false
        },
        {
          "cgaiCardanoAddress": "2cWKMJemoBakAyWD5Qe7aTftJYWUcJntvnWQiKiQ8MhCEaZY4DD3KStXr73SgCiDqYvhG",
          "cgaiGenesisAmount": {
            "getCoin": "20000000000000"
          },
          "cgaiIsRedeemed": false
        },
        {
          "cgaiCardanoAddress": "2cWKMJemoBamAw2b4CPMpJTVt2Jog1b3q6DmbLGjXFMus7ZfizYWBMjYePLMmGcrmK1PD",
          "cgaiGenesisAmount": {
            "getCoin": "20000000000000"
          },
          "cgaiIsRedeemed": false
        },
        {
          "cgaiCardanoAddress": "2cWKMJemoBajtE9twqWKPUQLtEU8aDDpYiCPFTS9oc8TfD1YNgViGwXUcS7L9MTGcHjo4",
          "cgaiGenesisAmount": {
            "getCoin": "20000000000000"
          },
          "cgaiIsRedeemed": false
        },
        {
          "cgaiCardanoAddress": "2cWKMJemoBaksfAxvNzmtzsJ5VeZqUw85FRfCJAwt8tGR5PCoDCxysZTf615JSqZBACRU",
          "cgaiGenesisAmount": {
            "getCoin": "20000000000000"
          },
          "cgaiIsRedeemed": false
        },
        {
          "cgaiCardanoAddress": "2cWKMJemoBaiEHjmRy9GfsgkjWRsaoJuFsk9pCZdKDopmpwhv2Lg83FPDg73AMmdmoBrh",
          "cgaiGenesisAmount": {
            "getCoin": "20000000000000"
          },
          "cgaiIsRedeemed": false
        },
        {
          "cgaiCardanoAddress": "2cWKMJemoBaiQKewfpuaQbeqjLeRmYdai9Qt7eXUZPbv4VwK594cSLuGK1ErtZoPgwLuN",
          "cgaiGenesisAmount": {
            "getCoin": "20000000000000"
          },
          "cgaiIsRedeemed": false
        }
      ]
    }
    ```
    </details>

=== "cardano-graphql"

    !!! warning
        This currently is only a partial mapping from REST.

    **query.graphql**
    ```graphql
    query getGenesisSummary {
      genesis {
        byron {
          avvmDistr
          nonAvvmBalances
        }
      }
    }
    ```

    ```console
    $ curl 'https://cardano-graphql-testnet.daedalus-operations.com/' -H 'Accept-Encoding: gzip, deflate, br' -H 'Content-Type: application/json' -H 'Accept: application/json' -H 'Connection: keep-alive' -H 'DNT: 1' -H 'Origin: https://cardano-graphql-testnet.daedalus-operations.com' --data-binary '{"query":"    query getGenesisSummary {\n      genesis {\n        byron {\n          avvmDistr\n          nonAvvmBalances\n        }\n      }\n    }\n"}' --compressed
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "data": {
        "genesis": {
          "byron": {
            "avvmDistr": {
              "CWJf8Kl8Gp-DhcWKuhNRUU9P0_CVI2LmpR1MIMxVgGQ=": "20000000000000",
              "h48-GEVDKf_0_vGKzmGOuAOhhIm2uc0OEDSNwFayV28=": "20000000000000",
              "PO1Kz9bpAowfWD8u9Ial2OkmxDiw6bK_ICDPvuHshJM=": "20000000000000",
              "mqJXwreGLRzV9a--egcVvKN4hzIcNUULsXqcPWe3YXI=": "20000000000000",
              "ENoYC3dNAtKL-lvjCTZDVhQYmfyWVtI0GNbz4QKqVdY=": "20000000000000",
              "o0O4s8YkitBZPeZLVyjn8pjtpBoncr-H9mbtAJS_KfE=": "20000000000000",
              "1XEVfDyaheIAeQICkHlwmvEuY9A7E50hA1v_E_QB3Fg=": "20000000000000",
              "OKVfmKrrzY0-10uxl9IxlYA6CFWwOU1dN-NyUI0bobU=": "20000000000000",
              "LYOSBM00cdDToqHepveoat2SN6vdPntA0nSFXRch83Q=": "20000000000000",
              "3Z-Z3rLCxLt0C3KgagBq3wOXrfm68_zh2st5Bi6Covs=": "20000000000000",
              "VTx9H6wpJNMC-H-pThklJ9uCllwqXaU0WXHcVTEFAIU=": "20000000000000",
              "beyF5mz8icvrBvM-mvQzLfbnHCmxOOg8Z7kJs7nySZs=": "20000000000000",
              "rs_VPO7SNO2YlSY2N881xHFeBnW_Sn2o4uSfuGpq9cc=": "20000000000000",
              "5RZPTI9FoSvLmiXjKYtUdkrqoMg99tIw8k4enSB1qlQ=": "20000000000000",
              "QRBLXNJdJCVDjbwPvQmg_liOcYIWfvoKf7Ns5w_RDvU=": "20000000000000",
              "YG9mVGb_MAvTSdFgOUV8JbRBxnj3sPELZxQNVup_X18=": "20000000000000",
              "7dEmv0hdv1a7imviD3q3p9pFBFMC77Byx9oinyGwdIQ=": "20000000000000",
              "H_Qs3m89FGw8QxVTUGuPjdbOPQyP8vzcD8I37BkRAXY=": "20000000000000",
              "pa6NZ1j_bs8kezg23cUQiba3UyLxLiGDQfDXMQmuPSE=": "20000000000000",
              "Qi5VCXOUdP-U-Pd--boAii_-gwMpB0IRfibzxWEN53s=": "20000000000000",
              "cLJrI380JOISi9A14PYTRmClcxPNQS3_GIZdqcXC1JQ=": "20000000000000",
              "BVY8wsqEPXPQ7DPTvnvat7GRiwxFPHC4jY1x3ZvY1PE=": "20000000000000",
              "FBlBD9ykcwuFmoogsVjRCUag9xpkwCAgbYDNazT1oY4=": "20000000000000",
              "KHwUZRYdwRs5UYIhOO9ycjucY8WvTzpkZgZ4PSbDDPI=": "20000000000000",
              "ED2RmO7Wfad2p4gxyzhr4gqlhavksgHEg1acZiKMQF4=": "20000000000000",
              "BOjqPWCmGewTKqKekH0HSgpnOEYT8g0qV4t4t6Nj7-c=": "20000000000000",
              "b-8mgbBV-r9bqufItyyPp2WLitNhBjaMAajl3GfteeE=": "20000000000000",
              "JW_kuEdd0TkJEUA35YUbf_K9C4OlpZd83iTUqrD0q0g=": "20000000000000",
              "IF7PMOFaXdwztrj1j_yx_YBafdZpb3pc5EF6y822KgI=": "20000000000000",
              "r9HNGwms9l0cMBuT8CznPoadXKbzLPceo4-vDydXUwE=": "20000000000000",
              "rM0RFpsVm738CCDdzBhrEz8Q0CLIqiBKMl5rtdFlWmA=": "20000000000000",
              "jFyHtgHVcd70V1SZ7O55mo8yvYw5KsijV6fMQEWkJaE=": "20000000000000",
              "qWmtDqvA3KZyySJLRPFPO2TiOZh3Dpv1FCJTdEilJvc=": "20000000000000",
              "5YYkqAleNp8VXGYiD7WsXvWZQwybkqAUR9xMf5lLvzs=": "20000000000000",
              "bi2YeYdi2W2r7IDYToq_N0RR1k3z4GYsRo2xddi2Y7I=": "20000000000000",
              "vENWfzLRm1wwHYWTUIOznVCXC3ISPt8J6n3gYFrwfpg=": "20000000000000",
              "G71fGc7_2IoExvY8VctjxMCd7lJsTWgvWzg__lF4qcY=": "20000000000000",
              "pFzX1lvX3LPolGCv8TQXfWAZupMZEjVlMWmT7nUV70I=": "20000000000000",
              "pE7umM4kIaTYqKOviWvgTb34xky-kpbN7VvSzhOT8wo=": "20000000000000",
              "P6J7kBAlCPD4wxAnzuzlqTBWMyqI1zVkqVWM5kKoCwg=": "20000000000000",
              "CmwjLeSIEKfLzd5ks16QoGCtYNc--wiGsgWWM4OKsco=": "20000000000000",
              "-RTrh8sxu9mOYYpcfmyod8-v1Z0nqxMunwK-_NFDzYQ=": "20000000000000",
              "EnoSKFBfSJ_l7RXMglXTSolDt6VeNRMay4soxkUmk0E=": "20000000000000",
              "VzxlTAQWmOH7ALIXviuXH2pjjYmtW77r9ypeEZlg4mc=": "20000000000000",
              "93QRpWUE-Yqthy85Y8poB_WeWdG8A_9nnq4HJiMfJQc=": "20000000000000",
              "H7YIN-FF0kUawzVnhQpCoMLuOJkLzYAtZ2xof7vhtPs=": "20000000000000",
              "a9_Q5Pzte2G7wXujwVBaAzLcMki6_UjKrxvWzayJ3gY=": "20000000000000",
              "O4h-7y-2Izq3ojxailZAbMd0VCq78-kIMv8gIcaA6cA=": "20000000000000",
              "mlzqpS0hE6s7apMGRQP4Cx5Fc380yt9gQX7XVcVrmQQ=": "20000000000000",
              "aK-WCeAwKHQE9H02zvRLRdoMPIsZWiOfKkbA6yTyMxs=": "20000000000000",
              "b3fq65eebunM4fM3AQubawjF6Mt9v9jyEXF5f3hewbo=": "20000000000000",
              "ZbZ7v6OcrjZ3vqPFVzaHOK2A5UzRYy-wm0C-ngebU_s=": "20000000000000",
              "zXza46kY7Gs5cJEoN2TUwAaXth5W7uUKQfNiaPyWDSc=": "20000000000000",
              "5q5RzTcpFFUne5AKkua3DJO1IFQEOGyb2jQvV4DmDT8=": "20000000000000",
              "5CzeQFC__uUi8TRPuWVgsUeJauYR3i1f3rvD0CrC34o=": "20000000000000",
              "KOp96-E17RXmCf0_vEOcecpTmY8W0wpbpBwFuPwFbKM=": "20000000000000",
              "EFK3F4mO823aCcko3QmFJ3Klm7glGs8a0f_f6WVIwdg=": "20000000000000",
              "e4TOcy9Qp5BQ8tXkEXaWpuHRmAVcJRfPEV3sVCOKZsQ=": "20000000000000",
              "w-2bM_wksghmtHp4ZB2ZOQ-V9Dw1ZivS7RwxgY-DsB4=": "20000000000000",
              "9pKGBzQJLoqY6vcOM_OqHRgq9KIdO3ovCBp1mBEFKek=": "20000000000000",
              "TMFEEMjP7q64-Liae7CEG0ELKtEC2e2vDuCpMItyfzU=": "20000000000000",
              "nrRREQUC1zKTpQRRNDzO-NiQh6DJahitvTk0SWLkc8g=": "20000000000000",
              "9xnVxPVNI9fdN5zGa5Fa3HcIwof5T-2lMbdLh3_Nbpk=": "20000000000000",
              "NqOaYkD2B5yTFQ1dHMY7X2LmV0Q9tZI6KYR1-dFW_z8=": "20000000000000",
              "zhXX6D5r4CDjlLLQiC87LZZL2zWUIYaXhxbcgq_Ww3w=": "20000000000000",
              "CXMg_ROxPjiJAyxUpBlUepLDhcdhMffR89izVr9vcRU=": "20000000000000",
              "vfTTtqpmg_3jQ7zWV3XhNfmtbtn32Z0fcfNc6_Cx-4E=": "20000000000000",
              "3UAwyThFcR1vKSBpktCSkJg3NpMQhL1z4l46NHpfJkY=": "20000000000000",
              "K4m8Vu1qtFRavgx0jrctVErZf6VbKDfBnigjQef6k-o=": "20000000000000",
              "rmyqI_SuwRsbR4rG1Uk6bUlSJRvo004w5SeejGQERT4=": "20000000000000",
              "oo8sxwl7TO2JP-QfW3_aQbE9zZCLBogFPnwEMPUBuYs=": "20000000000000",
              "lif3znin9EWfZBoYZ9Ta1c69eINSJNmaqkKJVpFuF2U=": "20000000000000",
              "kMAvVvgk0sEf3kHXyfb8gQ6H4gWaFBDSUwms4IFs2jo=": "20000000000000",
              "8CMex0Km9bk2J1r4FaSD_FSwlGRgh1e8C1-7fCGUAJE=": "20000000000000",
              "pUliJY_tq41pTdo5VJISdWbGGBnL_82pupm4AjZF-y0=": "20000000000000",
              "E7dg9_nI8tY9CXli9OyIHtx0FUsq0QZKLBVx9Cr8Daw=": "20000000000000",
              "cpO_GBP5qVwOCGxws6oGvgZszu7jy_LwbKZj_f2pg8o=": "20000000000000",
              "n8MZ16U_jB4Vg4BPZHGorDzVO7dC9qOfAdYhAluKD4Y=": "20000000000000",
              "hqzkwiRusxDSgY-MyqmCyTC0VxELSFdJKJVpzBGOdQM=": "20000000000000",
              "QCiQStlI-PWCbwclsM8ZvfrmP49kql7lAwJjgzZ_OvI=": "20000000000000",
              "7prDyNFRXierLX1UE26h-TSO0fmfC2lFHQoelogU3hg=": "20000000000000",
              "32mL1n7cF_xLIjWAGTC6vISGcafcJe0EgXaxaQNtrzQ=": "20000000000000",
              "H94Fk3T5fiEXPd1eGYfpwPP4_y9FVQWsi2bhIAf9hFE=": "20000000000000",
              "YLQqBDTjfVrQJcqCzgn0js4ScjJpbh3_dGRmg_wQ9WM=": "20000000000000",
              "Ctfg-00LO1JetbfqwOOIwrm56xdZSzzZRccGW52eCps=": "20000000000000",
              "GCkRs5Iqi5jyzRhF-Z5B4-EzgCRAb55pJK8a3kmrbwU=": "20000000000000",
              "975KNlfAi1B-u2Q0X0qBZpuZNLCUNnKnX-jvBzah3pE=": "20000000000000",
              "SrxXeNRxF0accD3dsKoj8ymSQeJoUVs78Llsy-4ZIO0=": "20000000000000",
              "m5zF99P2vG3cqGrG17VvRti7d2XRi3fuUHfec-jM6tQ=": "20000000000000",
              "A-w4r_kJIZpI8TqDAY-F44cR0lhZtnB25UhT0NXM6Zc=": "20000000000000",
              "7mglsAKSgUEkAyA6C5Ni-v_1xoGNPCN_cj7ctQZGZqg=": "20000000000000",
              "x3DrzZ_Yp8df2EGsGlwNAnclV2Tv3lcdqnI3Lk_0bF4=": "20000000000000",
              "GWoauU1tVb37fjs6mPrxXiBoy9HarPqyGI8zj1mc7cw=": "20000000000000",
              "op9p-7Xy9fBmzrjLbMD1jEuW1QVXTOsXSIwgaFN3sDk=": "20000000000000",
              "KC7yE_m_JSiWGVP9cpcfYTLF77taAPTgveRKEiIPg1A=": "20000000000000",
              "6V6sxoH3dMLw8vWcH0NQF2SZNPDzmtULTX4vxkeCQd4=": "20000000000000",
              "y0DLZDhvfU-M5MvdhoyxEFp811PWFfrAdIfDVhYCMzE=": "20000000000000",
              "wCHhA7PS7wdfEuMpzrOJfdGyF2uIChR2LnnAcQE3hHI=": "20000000000000",
              "s4iYnscFXdEK56b7o4ZvKpgN0YoKchpR-U9MZEqbGb8=": "20000000000000",
              "WkMPzKKtocKcbc7_fsFND1oln6gAfWJspg9REi75pMw=": "20000000000000"
            },
            "nonAvvmBalances": {
              "2cWKMJemoBajGgvgVVziaKmUFa4LwJnAHffmuaSJBMDqethwJVQsyBsTSfFhp5jFpkVQM": "5428571428571429",
              "37btjrVyb4KEg6anTcJ9E4EAvYtNV9xXL6LNpA15YLhgvm9zJ1D2jwme574HikZ36rKdTwaUmpEicCoL1bDw4CtH5PNcFnTRGQNaFd5ai6Wvo6CZsi": "19999999999999",
              "37btjrVyb4KCRtni6YrG77RLPosnDqtEYoAD5xLdKYkWgnLqGa8yuXDUQd3psHrfxqaRcvNTsAW4ngUe6bzstbzSUJtwoaKbYaL8zjFAJJsZkQ42ti": "19999999999999",
              "37btjrVyb4KGDMix4Uj5opvbMDgjZYUjeARAqTEFEbgLUH3qyju9gkBpcm2fVWgkcNgK3xFsQgWm1w8zxqvm9P6xJj9mHqLeMJPwDMUKUGPcDyUaDS": "19999999999999",
              "37btjrVyb4KEkSeCVx985rXc38DCud2AW4LdasNmyoPLbtDGcDCyYVdf8BzxvDnzPehv4kyVBkzThjVEkSpGTv8PGQs4yRUgiCaKa7PTtBY4ohNGqR": "19999999999999",
              "37btjrVyb4KFGS7upvgJHtmp7y7EFB67utzaHf7PM8y8U4tNkpmARNwiD7seN4NSAceHmj64KLGgh9qn1BpYF49NyWxocBHn1N533qBUYfhQar9ceu": "19999999999999",
              "37btjrVyb4KCfir7GrvC6Y5kBNjeakZNd5po62AzQQ85SGkBB4QfXibC4fSNK5YvNeVgmPc8WbEeSUHRjoiqhJ4HDtinK2deBHSdCH6Cw8k2u92rdh": "19999999999999",
              "37btjrVyb4KGAExHTQjLUHJBksSXGTomjgNsw8a4KepCgQYk4gxacKb84vGpPSv9Pjt3gdgMjA1nB67Pq3XyJpTDk8kLcXpJawCe6SCJf5jUowvAz8": "19999999999999",
              "37btjrVyb4KCE1qeEoUh9b8CpcZcJ794Di14AxAELGoppJNVdB79nnuKcgRut566MdDkxTqravFaDSD9iwAvDByUHi59xocCY3ButEjmCQeLTLZXQ7": "19999999999999",
              "37btjrVyb4KGGSGD8KgQD6qUBaSjxy5JRtsmMSHEGGAZqA29ULGwci8TcM16vBhywuBw54izQtpAqXeyUnbjh56hCgoqGZp9tHTMLLkEgLzwxVCZ4N": "19999999999999",
              "37btjrVyb4KG5ZZfwwiQuhAGWiNJ2FhXP3oAuiq75qknCz4CZWNMVY4B9BmiHRHnWfhUbkLHUqfabCYASUk2V1qGuDw97x1gdf871aFY7Lpz3N1NvT": "19999999999999",
              "37btjrVyb4KFtDHT2vDtMvQbLBgfH5hnpyVTTqqpPsieykukuxrDShHNccAEEj7M87UuV2GJ5pPA7YJ4JPjSokA99XaDgLmeaAumhZPHMwzg2Laspr": "19999999999999",
              "37btjrVyb4KDHFyvvKb29RD53ebt6N8kpbL41J4VxWpiFC4FnxxybP33M9tBbdqfMXvSvyTQpv4dULXf5B838kEWXSJ24bpHtFgcbRkiHQwqWFQ5du": "19999999999999",
              "37btjrVyb4KFh7jhHCtWxW942ceq7Xhxay8FZ7GkEBezGyFm3wJcVBGy1YYJDZ4Z7GbrFZmHLSe47zFs8Rjxk8rveoRpo1s43HXrMrhd4ijim4jJVP": "19999999999999",
              "37btjrVyb4KFhYgC9Lr4Se7C1gL39d5WBVADyUyQZz2BfG4BZxczyW827JRQR5enyWaoj6NnA5NyKsheV6Eb7WvQtbN8D6116HTknHhEb5jh1yUU6Z": "19999999999999",
              "37btjrVyb4KDLtM8HUJsBwergjZUj4DcMfkFmbV4bXUFGJk815o9nowX9ndPPVAeSNjAFYqJeFwTiMa9Ka8LqBnqFZgPpacyx9LrQLoXVMjvvLB7DK": "19999999999999",
              "37btjrVyb4KDec7E64byKc4XjmmCRDaTGQYgHJTPDijZVr7NwZSP8g7ienzTLx5Z1quaQRhJqqAyV8Z2QdkzXvjTTRiVDCqps78uGp3uuth4wEJKP9": "19999999999999",
              "37btjrVyb4KF5R1LEsaQgjWFWXwbgJ51naDEaCRG23KiAN3UtGzaT5PvUANtFBgjmcCtPLMBYMTGL4S8px4HyQMLAyF4fakYoFAJC3PkxCWMUatGWD": "19999999999999",
              "37btjrVyb4KFB5Tmw1wsLmuv17Q6y8i6HGpVxbW8k4bevmob3DcdbH6jzrAtUrBpKgfTGgPMpLAbJcpaByGGJErkXQWFwrNMW35S79hxFvAN2GTXVQ": "19999999999999",
              "37btjrVyb4KDEX2XToMQoi1No3YdREgZWzrf1xQPbfhbZTZnprFwDsRMiBxqUrA7p4BwjxXHDyqAccPwyX8iWWquz2CrLazJMR4s8AMz2US1D1ffJL": "19999999999999",
              "37btjrVyb4KFTKCoqtZBdbh7LtJ9mRR1nbkX7ggP6a7AwvkSDxUN6U5GJWfuRXnL3a5x5e16uQwyjC6PoPVQ7VLdJXr8Kd3eFknLu6NDf2ey4AaJo2": "19999999999999",
              "37btjrVyb4KCHCpiGd1J2GtVjP4KxEWP7RE6K6yxHzE97cbDgFD63fUFygbni8jKw1N37nGsT43KBvBn5w9ee8sVegr6Tg8fAr52mUkhdvTZYJV52T": "19999999999999",
              "37btjrVyb4KGLRpX3uQfSeLovpMTcWfVZSM5RCufYvy2tyCMwrLXyHKCM9VqQh8dCQA6WcTrViaxqpvBSeKreHFL4CftfJU1z7CjHAze236NLesbL8": "19999999999999",
              "37btjrVyb4KEdwV1MS3Pjek1HjLN2CSq3SJZGFBbZctkGLz569i9RWN15bAvCcZ8R5dgEi8iYjpmMVKioufoGv3issZQvVtzPz38pWHBViyRK2how5": "19999999999999",
              "37btjrVyb4KD1x4cqHfGxrvBubZ8pSM8Jmw15UiHpy77eMsqpMewGND2GdvAwTBZhf4KA4uypBJnuUPbPYFovpRVJ92BUaMBHfQnAD3i15DAzD8EvL": "19999999999999",
              "2cWKMJemoBait15xg1M73WAvWafoieg2GrcykbRk6J1QC2jMUXn7LpXf4mk5RUeu8qYeG": "5428571428571429",
              "37btjrVyb4KC3HyNR82Bj2Sr9o6CF9o3J5hBNycGb9JwrHggTYUHfivi87akkYDv8ayepMkM4mNvxTKvoVdMHFkMnZZgrk5qobwPKM8idSnYYmvTRU": "19999999999999",
              "37btjrVyb4KCmCLYttFEWLNQc1MRbV1NyhhssRioZ5CgkqHgYUTT1pPSr2hrfevSe8bSwLiPsLnaCbsxJQc5SWgWYEJDPWuUA1s4AotQxERNbT9ReA": "19999999999999",
              "2cWKMJemoBahLnFCQ8wrTuZ3sMyiCeEkUZDYLNucPiVTJr8UU3BgADsKtqosDYNFXzeiw": "5428571428571429",
              "37btjrVyb4KDHDPBVenqrh8tUTVNYX5ZGwjd4r3svqdnwWicGnMZZV1E7nBJVQsDY69co936H9onHpmA3PYSabYH4ibbULphL1CitDgArH9KknBARc": "19999999999999",
              "37btjrVyb4KD9Z53z9qD7gTWMHr22e8jDcpCHgJFQaGQsvnNkycRehAaxLAnufNRjhLzQ57XVGJnR6mcsk6MorapLpADT77tyTaX9xfUSZyTA32ZAy": "19999999999999",
              "37btjrVyb4KCsozLcUUHR8GyVG7erY6j9zehKTADn3e5xpRJtu1YgfJzSmAyERBHUXa5LGWY2aR2KqcssnRjwugh1bGjxc6U6ZrePJnALYTw2TR3yh": "19999999999999",
              "37btjrVyb4KEDBSAmNtUBy6pfXesvTvtrDZQsSYcyo7SUwjLkhoSaPDCsNqMmoGbqzFQyEe9DNwK59BMudtdkzFPBpbgiEWx5SZr6vVMbpe86qsQJV": "19999999999999",
              "37btjrVyb4KFf5NQ1DuNoAP4phRomqdEUmtFb6sWcDHkizGj56dwn54LfKrfWa6Er5sxDXYrzpWwS56PKmKaBjJtn1JqN67K3CihFXXospn8B2TDz2": "19999999999999",
              "37btjrVyb4KBPHxFJqCekpPztGnLgbVsUA46Q8Lj2LKbFJL5Nqk5LgP2u28eBJAxkkU2r118ARdXW7fXLPQgctwK22L4N3zc6XeoDqkadGmTd4s8a1": "19999999999999",
              "37btjrVyb4KBYe4RSCngNCgVMAeMJkRQqoJs8t6t9e9BHcNvvT6awv4CruMWH2FyiudxcGfZHmjghDvqk39iFrmCt4XE2XDuYzyo97BxwS6MngfeWp": "19999999999999",
              "37btjrVyb4KF3MxxJBeJqCPFgHyUsSDkrDqoctSSVi9h7F4Wj9zFKcPVuVju76KYhdp7nJhy44512Wjhw7WH4sed3MMSh1HYnKUfjZXGkoZXMajaye": "19999999999999",
              "37btjrVyb4KDsi9fc3RfExWLumjkp2YrcMjZpew19Z92kZnjPy5Xa84KY2WZw6xmjJA7AXFJCBWtrF9RFw1BjCewEqK77CVYj2s7bk9aAA7yyZARRz": "19999999999999",
              "2cWKMJemoBaj34AMeqLspGBgX1PVc7z6VkALK3rtVd8iFgCtMUenNoHhVRnjeGfYVQJM1": "5428571428571429",
              "37btjrVyb4KDpppzxzoaPgnstPejNxGaSZ3Vh22Qd2DWGrwfLJ2tizs33Y5Yjya1U6TXPzVX2PT5g1PXMy4jR4aWZRGZqYJk4Uw9p1h6BhEa189eiN": "19999999999999",
              "37btjrVyb4KEXaPVoMuKpnVEKKLMFuGSvYL7YMAD529yxeK8Y1zqnbh5FQ8GMYpJwARugWmzaXdJ1gopgsxziC4e5wgjf3zkp7RH41KTJ73xLyfrFP": "19999999999999",
              "37btjrVyb4KBrhG1yaBVY3X1ZoTCjUH7gbiA4qSFsMGgtLUBgwHiMZPiAJ3kQrRiboPV3s7eYXZD9fnd27qRb1cCEMc4oU57aPn2cYcEdAEJHrsyLB": "19999999999999",
              "37btjrVyb4KEC4vC63KNqRBD7RX1KBwWDdPDE6oXGxP8x5aKrbVTALaq4XBdak8F47Kt9VcsQvVsKZfAit8vBtZpG2mc6VKUXCFv8pTYWwQMnABckB": "19999999999999",
              "37btjrVyb4KFPbetkmdvqD8nLRFsUVL4HsVUYmgaZhAmBXcr78M3XoZkptjuszd2T1FNr1fGZApkZFZXikGtyhCc7jH5JYD1q8csTNSWQn4Us3nzX9": "19999999999999",
              "37btjrVyb4KDfwNJcNMYsyEDVHWrGrNAPBwF9FGEnm5j2XFs4BeGdiSPqPtqCjWCvcRYBfY5EoDjRBhjsTrr2HjB1jw8XZ9Hy8wd9gz4KkCbMugSgf": "19999999999999",
              "37btjrVyb4KEgvmzjLT7R9xHg1vNob6vCf999UFuG4qfTpHGZufdhbkUogSFJXtQXnCcJDHJ6xuZt92H6VgxGdhSLQ9gmWZy4zCJEVu8Nj8NashVQY": "19999999999999",
              "37btjrVyb4KAtY8hCobTmAB36dzosSo644ZrzATKQhP1AsnM6BAVfTWwMX5BGXhigxLm5hk4beodymyjivxrH7ZY6BZjMu3AtafB5guvagxEZM7vJq": "19999999999999",
              "37btjrVyb4KCRMJDqQ3iK4M19XhWGWpFbCoxjgeDB7ZqUhgW7jSYLEk5oVL4okVPVx5rXCgoK2ND9kAWnNU5QncJp1qvuCngRdJaLrvFwp4boE56VR": "19999999999999",
              "37btjrVyb4KBTRHUnz17FQNFzHR8PpoGGwuNQZauAUxmvTb1o7Ragv9Zvyiv6Cb3rnrmYY1PGtVFTmom3TGg4mK5XpkRyf7PnnCG5EQM69i7MViLpU": "19999999999999",
              "2cWKMJemoBam1WPtZrz3Fi4EMUDao74k9Xn4fhyVYLqK4o1VRzoxPFU91QBRToLbVyrjX": "5428571428571429",
              "37btjrVyb4KBUH8nDpwtt3sSc6rJ7AkYjmnqvt1tJ4J8ZKCuqPrEuEioJJZXeD9aohveoB9zhRWru6oM5zyBcgMtkA26HLtTDKsSVwzoqugfftbiPu": "19999999999999",
              "37btjrVyb4KCgWqZ8sJW46mQGdZS9wKW2TEQesyCoRScUxvisbdvEkfsxYLR49i6wE6uP1BBgPX9eg8cxKHPuNyKpStwf5UVmRCXD2ahotjamotMwV": "19999999999999",
              "37btjrVyb4KCR9vZcXetWv4QdP4jPSH4msGeXs8DUBUMVYJHgD62etfv2jiD7gmLbLezCAiGTQu9JvrHd9Wfu74wguKgkX1vCUQkzcWsn4rVWsKCyt": "19999999999999",
              "37btjrVyb4KFV3TiBqtLDN3oHQr5NrA8ouAAqasFU4ZuB9W13xgcgWsSy5fUtbNL4imCruQz19hjzBzykxGxCAarrviCUBh3sxWbvvTTHdvpyWGgXc": "19999999999999",
              "37btjrVyb4KGM5rFFreGtZAs4PFB2Drb37uXRHebh8rCeVWFkW8De8XAbYqvfQrAqVthfJp9Qy2YzbzNhWSiUGY3D7yJkRkChyMveKCWT8qUTNEu6e": "19999999999999",
              "2cWKMJemoBahEJS9xuB3R1ofSgtG621enmfpxfx9Unpo2K26wJPioaA4tizZrNMACNoQb": "5428571428571429",
              "37btjrVyb4KFGV8HUDv7S5E8CSBV3pQLgGFt5HXa2jb9ofbAo3gTxcaQ4So84mHsNk9mhAybq6miH2VZU3iz7cqCd74gPMyn3zdUsrF1u2rib7HSXq": "19999999999999",
              "37btjrVyb4KCF4JTyyC27XuFmcrn9Nxj1DmM4G5BKnf8F8F8BSpvn3PnsLRNH2RZJoajmg4yqHnwMXpUnbb7sFSuthjXv6YUenX8EWsApQUzAm77Dm": "19999999999999",
              "37btjrVyb4KD5zMmtnD3jWpX3TSJnZJ8jMzCFQHYa3HcHNXxdAnK5A88SiWncRpJQxesMDrYgzPHk7SnFNag5teaFELV6hE9opnJpJzMGpVicDDRX4": "19999999999999",
              "37btjrVyb4KDTABtj2RScLVCLVyFhxcURYUZNVta8CghbH5Edz32XSP79NHc28QTkKLMNUBupRnJXs4zcZt8C2fiPFGZfgSBMqMGMidWc2zo9piRb4": "19999999999999",
              "37btjrVyb4KCGn7x5G6obn9NPNoTuv25LrBcqK9wHCm3XbrhxqycSQrbPfsDxgDp2M8pqTjCk8cVEG2fRxWrTjfG4q71MtyMo6nt8WG11kJzdQQSNL": "19999999999999",
              "37btjrVyb4KEhnv3cCqP8jzRBbwE5v6ymPkBjTexHCcCgYJarjHHaxipJvz4aaXc5Xmp5KXxnC3SoE1oqR8sdGHobyfsAqJy7DwejZWpkkoYD7LJsS": "19999999999999",
              "37btjrVyb4KDvKgSbCTx1gwwZFGe5DZaXyGwTYGGnJNCQ9C61XP4n1pKFQtNbYEowGeRoKHCGUvuU8Ebz2vQwN7YhcJ9bSb5oNpAoCe8UxX5KK5C3e": "19999999999999",
              "37btjrVyb4KB6yr5YozXGqSKemHyZfsgiRQFX3VdJBKx7waoSaScNWc2dNvhNp6HSnXMxUwDtBvicXDWdpoJ7cKLWAwqEYki5azdt1qDP4sHXh8XhJ": "19999999999999",
              "37btjrVyb4KCqRLQRj8svdZGGLQDZGdXztzeC15Vvt6uZWg23QAdfL1dMc52dpc8jqKquWNj6xjyLnLciVnRxzEq1kiq54ssmc6h7V5xK1cqqKJWBT": "19999999999999",
              "37btjrVyb4KEW3PG2LAJNwmok2H6i149HetvT6fYrPsqPGpUkucNkA4b5TQmv896EF44UmcCAXDycfxFB7GcVefBCgk9cZffVUdX3kri3i5TEqSjKm": "19999999999999",
              "37btjrVyb4KDdgCo1a7URfpQVFoTJEUcn4LqWpAtCeLaW7NeGMJtecsahTJM7886BjLcnhU2CboLSUojCPcab3WNTmXFDrRMHMHdmWefCAyYA7QaaL": "19999999999999",
              "37btjrVyb4KG5vwKTPpCSQ14a7Cv4TESosSVoFVRHsw1vHj7Tpb7N4j1U5dtFcY7L3MWsH9BJqQjU9NxwaHpbHCJAhmsLi2mC2BE8k8Rj7zcjiiVhR": "19999999999999",
              "37btjrVyb4KAr171Hd3fu65bbtKxqwktHGwY9kNanPYGXQcFKA8d9Hp1RgLrxzU1AdJCJbJDByTnHdDmtA7pm985tKK8hr5JdHPXFfSvkkYZn3kb9G": "19999999999999",
              "37btjrVyb4KEZWqFxGYhFuLE23i92B8BiLRwwFUdnjmM24KHCNuWik5dc6MJqz7GxupgKGK5zzbYSXJyA8DQVDszyFmJuoxgzPn2GSnfBoREZ82ZdF": "19999999999999",
              "37btjrVyb4KDB6sZamEoJWYBLoDWucRdDtRXCuQvLCoVoHPNjrywJKDz8PQg65ZtLvYvREwAK9oLzGGb6UcdAf9zwQcFaKRRHhLzCZxwTsRDVyPkSy": "19999999999999",
              "37btjrVyb4KDJc8Af5dfJY2jcFbtkofFL6qXBxWnk2kHzCE63qAQR9Ynnk1XkfUrcnBrN7EEyvxmUDEFdNFfZHzXKhmkSQht3b6Y5rHHmuFYYoKdZz": "19999999999999",
              "37btjrVyb4KDjPpuxBuJM1Ma5NGBqriSEAcozZMqYkWEkoJ3GgR8MAy3Zeb8q5BvtsWGEpSFQ69znPuaX4kVCcnEiEMDp91A6EL8A5YM12cpYYogLm": "19999999999999",
              "37btjrVyb4KDmCmpc6FutA3PbQmWxcsbzZsFEdMKhHxrJVtGkmaiWd18dKmiaRA6o4Y2sAjDwjFozKzNzQg3dp8CXVSPpWgDLAXaozoRyPanW7UM8B": "19999999999999",
              "37btjrVyb4KCLfSBFhCBdrb72YBNJkKcDCqpdxf3k7iwJTj7M3txxiq3fcam7Nyi8sLcJNSfgnUrh8C7RKEMN5wWpku7HLdZqZVuPRjeihhgXmEpCe": "19999999999999",
              "37btjrVyb4KF3ZQhHTvVH3L7jNYoZ3XWK6SWpqZKiu9AGz6qNtxoxhAmmJpenFMA6fedYDT3Lt7cihgc1q4pE2GJXvPuknAkjvESmPxhhzkBzuiRis": "19999999999999",
              "37btjrVyb4KF9xNLdS47pRBLB8e8bQLuvrBiGJbnHPNCbKU4wrMerJztEtYJdHayvaoUEmJ1vc3aiq9Z3UgP83Y1b4rpiyrGeYjzQhhDgB6DW8sWJm": "19999999999999",
              "37btjrVyb4KC4LdZLvrexUgAntpySomDAPVwdMEnz1cP9pZxsxZqVYzM6zPPWAhc7byfwsLdgW8GEMTuTUagYFAKEmYgaDmYxK7cHxtWJG3hiMHxVU": "19999999999999",
              "37btjrVyb4KFfXPG5GDEc4tLyVUSepKD9GGXZcSuP1xtLLRQnQr4wSXawT62bSJsiPzS25kdADKh94V3iDksm9nq5fhV4jixCnpNjsn7k2hSkwrAUa": "19999999999999",
              "37btjrVyb4KC1L9MsZ8htPomWp4FV4hULeBVT6jf3GGqmDcw3k9tPn6pnfqGTWowQDvqVr7BqtQ5rcQ7gc5z41qh9vyBV7Ds83bRsndDbTAwkEUJ21": "19999999999999",
              "37btjrVyb4KBtLs3NwnoLkyVx9ZxSoQ7mQx2ZvzanG7PGWWdMA9ksXqZxakLxdj9MAPKw7eQoZJWHxJJCsd7MeWj7ujfXtPsthGhcURT3Hste4Kr4n": "19999999999999",
              "37btjrVyb4KCaYYFDdnbHEdBzpDPcL5iH98AGF8J5avuweDHwKChDq9mBvLKVJS6F4YTuAVDigPjMnAcuYUJ2UUbvDPePFZBhLhBrFUKeauoKC8X5z": "19999999999999",
              "37btjrVyb4KFfnmiGpxNSGQVMfmFpAFrEAEhZwGPQDutSHnZqQXPhcXxDNcbdoKiztyQHpTA3jSmVowZCxcaJMS1k1wu8U6nXTzejgh7wYZjycamU6": "19999999999999",
              "37btjrVyb4KCTVE8b2UitH791rYrkSrHG9u449h6JHKotuPWRsdVZQfP1jXrs4ygSxAnG1rM5mGFM6cmUqA44e9fenjbVC1QFYyn3R4CaptVZypKgW": "19999999999999",
              "37btjrVyb4KDaKs8A6CU8Lrzxpr3WNM1kDdd4CPe66TqSP2ehHexrAuZ3ykMmhkaUZEHUEiq78ELQx5vpSFGHXFKbyGgrWa8rokqamCV8bSKiqsqVt": "19999999999999",
              "37btjrVyb4KCdPziHrv1QgXL5zMy3KYxr7zPqoRd96iz3LNrkWbobRmPswTpRKgCQEkZcEnipiNJ5UoULAc33mbR44MchdHT5vLNYT9sPxwzpgNUWj": "19999999999999",
              "37btjrVyb4KGAZJWCpicgRkb9ijP3Jnv6y9EYvnpMqkPdBZ2d6fdnCa9C97HUmfHLWF846AKjViPvnY7MbSM8mTM3VDx5RazBFxA7C7mZ9CyM48AW9": "19999999999999",
              "37btjrVyb4KFfgfRyETGNXNXm6gAxNpTQSxr6bM7T6yZE1ibiZBovxG52PioVmLRnPYxs4wYddAfgTH4mbmtLFnwuZYSBh2eNsdLdc6vWb4AdJNNym": "19999999999999",
              "37btjrVyb4KBsV28Wce4x44WsTVoXa5s4zaBKALWXxMQ2MtfVgB8LJJW34QFpvjrxKmheLtpcLyVqaeu36oB6ZcgQPppFN4oqhdueoKBEpn8dfQUVF": "19999999999999",
              "37btjrVyb4KBEKzyCGqao8GmErVER19oBH3L8xVNCSZs8tCVd53iV5FXZAZ7bNCT67bKoasGeiYZxEoDzGBsKxb4uJxStU1e9wkaPo6BxUEErZG7LD": "19999999999999",
              "37btjrVyb4KCi8WRzrkFiE1AVYbSiXBzMRTuTLv8DAchfg4tPaDgHRuDN6m4dq4VkA17pkWwCoa2NmvNb5sGeU1ZkjcqFuYrWPK2C5a3TLBfx64uLs": "19999999999999",
              "37btjrVyb4KDz8QGqk9LJ9kSsSd1zKgqfTuiTKbL92b4aDSXmRPyrFZp3VPEwhMyEmwCiSkpd7KQztirmU6CGwiphhiHoXbbZjkbfiHN5Mq1y7fmJM": "19999999999999",
              "37btjrVyb4KEcCa8yc1d8Zrne1hRtedHQQGPbkRJcvHak2ygcCndPfSqWwb2L59ERxhtqsMJLdS1fPQPs2vcJcQCvB52tCm4rGCDwUmRG51PUzjSVh": "19999999999999",
              "37btjrVyb4KEhvykuoQZKWdNKFgTrugRUSV66yr8GCXREuaX2PfVQJeuXS4h1bNP8SeUxHAG2J6RfK35YnsZj5qkWQCWV7tVYMVokU5bw9y8CcmqPy": "19999999999999",
              "37btjrVyb4KDsFS7rbQjZQGX6Fz53u76NF2hF9iqRhfbx8ePmjJKCsv5rZV1hhtP6DHdKwLAf5zkVH9FE51xYuCvJzNppSn1tgExVQJpuTwKmyTekT": "19999999999999",
              "37btjrVyb4KFtfVixJcJdD27YZfXfRM5diZFj4TEazkCAhF8KyztTbe67zQpBc1RTjruHNvefdJ9Jtr7u5rebR71tGrGtKioSSGKy2hMKd6GUVFkEg": "19999999999999",
              "2cWKMJemoBaiXc4dYCHfDyvy3LcqPebJ8zfRJsZZoHDqik2SzK9Be73YQ5W9u5jEiMPXa": "5428571428571429",
              "37btjrVyb4KFNpxUYvzqFKsRfRYJKHjEuFfgy6rpoGq29dcMrNhbKTvqNr719U282rp9PcnFonAENkUdv2nE36wZmyNkj8JVQJL1TLu25SKjGFNzVs": "19999999999999",
              "37btjrVyb4KFYnqsKFfMwj4S3PcCxwxutoiUubHazLfw4wJc5bfrQgpNEpRnGCS2UUfzvMWRRV2vy4wzPKE7tshLS2YEW219R6QfAenXzktMoXCmuU": "19999999999999",
              "37btjrVyb4KEbo96SLroMfZB11rEvztPswgdqC5ESkHXc4EaFdAhsDQ24vJK23XsjTJzpPS3ZcDWiHiFVtmp5wkJrcnRcMe74s88eTy8YLvLL7EBRj": "19999999999999",
              "37btjrVyb4KBcyGa22cYKz3Xo9UnB2kzZg4nqVX863JCAvUd54ehdg94DWwnBasCv6sUdbKdh9t4tf48oaXokeoms1HDNuegsmRjVntHBX3Z2hnrV7": "19999999999999",
              "37btjrVyb4KDHmdZ8ewhythkmGaUzLCwME5pGtWR7nPhE9nCjLMcKstQFyKq1vTSagA2BXtiopfGwtLq2e1jhUVKsw1Z8Me6XTmLm5C9MzRYCZCd3n": "19999999999999",
              "37btjrVyb4KBGJtifVyePJjSHTno1XNBeTcqmSqEhyUznts9KGQTsvCd9Hq6zM2w29njvmJYCtWmNkUXvayfqyv7epN1awWkKK1WUFQKJsjtevSKz7": "19999999999999",
              "37btjrVyb4KEwqVLa1yaRPdDUctf525DovPsxoXZfqNp26eXQnmTSFwY3Q3rgPsjRfTKHKtjFpxPy6XYvjzscccsZYfXSaL9MmgrKAaeQgQhCtoXih": "19999999999999",
              "37btjrVyb4KDxRyqtP9nWyEd7sfzdB8Xgh2egCATJAVtvxM2LhKLp1ALCE714vMCsbQZ5SwvVgiAvmieJTkae865ycwU39JN4pgt27pqEuB8uvi947": "19999999999999",
              "37btjrVyb4KDA9F68PUv9efaoQHTvacu98Dk6Zx3784ADx4SDnwMfDt3uRfJwqBELeVuis5UEqsf9u4zAD9YC82s6YNmQu43avWDqrQq9Z4hHkEVrL": "19999999999999",
              "37btjrVyb4KCjschbSccsYGDJo1rVBjdVrpH27iRtc5h1q4XRqpQJTma1NA9t9t8PrTsJjFE7WzNCczJHQR1RGXW1jDNEiEqNa6xctAZ4ZBtXKHVtp": "19999999999999"
            }
          }
        }
      }
    }
    ```
    </details>

=== "cardano-rosetta"

    !!! warning
        Not available in Rosetta!

#### [/api/supply/ada](https://input-output-hk.github.io/cardano-rest/explorer-api/#operation/_totalAda)

Get the total ADA supply in the blockchain.

=== "cardano-rest"

    ```console
    $ https://explorer.cardano-testnet.iohkdev.io/api/supply/ada
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    {
      "Right": 42001932863.01618
    }
    ```
    </details>

=== "cardano-graphql"

    **query.graphql**
    ```graphql
    query getAdaSupply {
      ada {
        supply {
          total # corresponds to REST output
                # total = max - reserves
          # additional interesting output
          circulating
          max
        }
      }
    }
    ```

    ```console
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    ```
    </details>

=== "cardano-rosetta"

    !!! warning
        Not available in Rosetta!

