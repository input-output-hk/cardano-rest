"""
Schemas taken from https://input-output-hk.github.io/cardano-rest/explorer-api/ - 25/07/2020
"""

from jsonschema import exceptions, validate
import json
import requests
import pytest

API_HOST = "https://explorer.cardano.org/api/"
NETWORK = "mainnet"


@pytest.mark.checkschema
class TestHttpBridge:
    def test_network_utxos_address(self):
        """
        /api/{network}/utxos/{address}
        """
        test_address = "Ae2tdPwUPEZLzyDSkCj5APms4VBpDqdhuAvZio5EktwdDjYs2JX7gMdyHP1"
        url = "{network}/utxos/{address}".format(network=NETWORK, address=test_address)
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "object",
                "properties": {
                    "address": {
                    "type": "string"
                    },
                    "txid": {
                    "type": "string"
                    },
                    "index": {
                    "type": "number"
                    },
                    "coin": {
                    "type": "object"
                    }
                },
                "required": [
                    "address",
                    "txid",
                    "index",
                    "coin"
                ]
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")


@pytest.mark.checkschema
class TestBlocks:
    def test_blocks_pages(self):
        """
        /api/blocks/pages
        """
        url = "blocks/pages"
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "array",
                "items": [
                    {
                    "type": "number"
                    },
                    {
                    "type": "array",
                    "items": [
                        {
                        "type": "object",
                        "properties": {
                            "cbeEpoch": {
                            "type": "number"
                            },
                            "cbeSlot": {
                            "type": "number"
                            },
                            "cbeBlkHeight": {
                            "type": "number"
                            },
                            "cbeBlkHash": {
                            "type": "string"
                            },
                            "cbeTimeIssued": {
                            "type": "number"
                            },
                            "cbeTxNum": {
                            "type": "number"
                            },
                            "cbeTotalSent": {
                            "type": "object",
                            "properties": {
                                "getCoin": {
                                "type": "number"
                                }
                            }
                            },
                            "cbeSize": {
                            "type": "number"
                            },
                            "cbeBlockLead": {
                            "type": "string"
                            },
                            "cbeFees": {
                            "type": "object",
                            "properties": {
                                "getCoin": {
                                "type": "number"
                                }
                            }
                            }
                        }
                        }
                    ]
                    }
                ]
                }
            }
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")

    def test_blocks_pages_total(self):
        """
        /api/blocks/pages/total
        """
        url = "blocks/pages/total"
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "number"
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")

    def test_blocks_summary_blockhash(self):
        """
        /api/blocks/summary/{blockHash}
        """
        test_blockhash = "3c89f7d9ff6c06468e32fd916d153b033264f780e11fca7750cb85f56d4f31d0"
        url = "blocks/summary/{blockHash}".format(blockHash=test_blockhash)
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "object",
                "properties": {
                    "cbsEntry": {
                    "type": "object",
                    "properties": {
                        "cbeEpoch": {
                        "type": "number"
                        },
                        "cbeSlot": {
                        "type": "number"
                        },
                        "cbeBlkHeight": {
                        "type": "number"
                        },
                        "cbeBlkHash": {
                        "type": "string"
                        },
                        "cbeTimeIssued": {
                        "type": "number"
                        },
                        "cbeTxNum": {
                        "type": "number"
                        },
                        "cbeTotalSent": {
                        "type": "object",
                        "properties": {
                            "getCoin": {
                            "type": "number"
                            }
                        },
                        "required": [
                            "getCoin"
                        ]
                        },
                        "cbeSize": {
                        "type": "number"
                        },
                        "cbeBlockLead": {
                        "type": "string"
                        },
                        "cbeFees": {
                        "type": "object",
                        "properties": {
                            "getCoin": {
                            "type": "number"
                            }
                        },
                        "required": [
                            "getCoin"
                        ]
                        }
                    },
                    "required": [
                        "cbeEpoch",
                        "cbeSlot",
                        "cbeBlkHeight",
                        "cbeBlkHash",
                        "cbeTimeIssued",
                        "cbeTxNum",
                        "cbeTotalSent",
                        "cbeSize",
                        "cbeBlockLead",
                        "cbeFees"
                    ]
                    },
                    "cbsPrevHash": {
                    "type": "string"
                    },
                    "cbsNextHash": {
                    "type": "string"
                    },
                    "cbsMerkleRoot": {
                    "type": "string"
                    }
                },
                "required": [
                    "cbsEntry",
                    "cbsPrevHash",
                    "cbsNextHash",
                    "cbsMerkleRoot"
                ]
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")

    def list_blocks_txs_blockhash(self):
        """
        /api/blocks/txs/{blockHash}
        """
        test_blockhash = "3c89f7d9ff6c06468e32fd916d153b033264f780e11fca7750cb85f56d4f31d0"
        url = "blocks/txs/{blockHash}".format(blockHash=test_blockhash)
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "array",
                "items": [
                    {
                    "type": "object",
                    "properties": {
                        "ctbId": {
                        "type": "string"
                        },
                        "ctbTimeIssued": {
                        "type": "number"
                        },
                        "ctbInputs": {
                        "type": "array",
                        "items": [
                            {
                            "type": "object",
                            "properties": {
                                "ctaAddress": {
                                "type": "string"
                                },
                                "ctaAmount": {
                                "type": "object",
                                "properties": {
                                    "getCoin": {
                                    "type": "number"
                                    }
                                },
                                "required": [
                                    "getCoin"
                                ]
                                },
                                "ctaTxHash": {
                                "type": "string"
                                },
                                "ctaTxIndex": {
                                "type": "number"
                                }
                            },
                            "required": [
                                "ctaAddress",
                                "ctaAmount",
                                "ctaTxHash",
                                "ctaTxIndex"
                            ]
                            }
                        ]
                        },
                        "ctbOutputs": {
                        "type": "array",
                        "items": [
                            {
                            "type": "object",
                            "properties": {
                                "ctaAddress": {
                                "type": "string"
                                },
                                "ctaAmount": {
                                "type": "object",
                                "properties": {
                                    "getCoin": {
                                    "type": "number"
                                    }
                                },
                                "required": [
                                    "getCoin"
                                ]
                                },
                                "ctaTxHash": {
                                "type": "string"
                                },
                                "ctaTxIndex": {
                                "type": "number"
                                }
                            },
                            "required": [
                                "ctaAddress",
                                "ctaAmount",
                                "ctaTxHash",
                                "ctaTxIndex"
                            ]
                            }
                        ]
                        },
                        "ctbInputSum": {
                        "type": "object",
                        "properties": {
                            "getCoin": {
                            "type": "number"
                            }
                        },
                        "required": [
                            "getCoin"
                        ]
                        },
                        "ctbOutputSum": {
                        "type": "object",
                        "properties": {
                            "getCoin": {
                            "type": "number"
                            }
                        },
                        "required": [
                            "getCoin"
                        ]
                        },
                        "ctbFees": {
                        "type": "object",
                        "properties": {
                            "getCoin": {
                            "type": "number"
                            }
                        },
                        "required": [
                            "getCoin"
                        ]
                        }
                    },
                    "required": [
                        "ctbId",
                        "ctbTimeIssued",
                        "ctbInputs",
                        "ctbOutputs",
                        "ctbInputSum",
                        "ctbOutputSum",
                        "ctbFees"
                    ]
                    }
                ]
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")


@pytest.mark.checkschema
class TestTransactions:
    def test_txs_last(self):
        """
         /api/txs/last
        """
        url = "txs/last"
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "object",
                "properties": {
                    "cteId": {
                    "type": "string"
                    },
                    "cteTimeIssued": {
                    "type": "number"
                    },
                    "cteAmount": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    }
                },
                "required": [
                    "cteId",
                    "cteTimeIssued",
                    "cteAmount"
                ]
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")

    def test_txs_summary_txId(self):
        """
        /api/txs/summary/{txId}
        """
        test_tx_id = "3c89f7d9ff6c06468e32fd916d153b033264f780e11fca7750cb85f56d4f31d0"
        url = "txs/summary/{txId}".format(txId=test_tx_id)
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "object",
                "properties": {
                    "ctsId": {
                    "type": "string"
                    },
                    "ctsTxTimeIssued": {
                    "type": "number"
                    },
                    "ctsBlockTimeIssued": {
                    "type": "number"
                    },
                    "ctsBlockHeight": {
                    "type": "number"
                    },
                    "ctsBlockEpoch": {
                    "type": "number"
                    },
                    "ctsBlockSlot": {
                    "type": "number"
                    },
                    "ctsBlockHash": {
                    "type": "string"
                    },
                    "ctsRelayedBy": {
                    "type": "string"
                    },
                    "ctsTotalInput": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    },
                    "ctsTotalOutput": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    },
                    "ctsFees": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    },
                    "ctsInputs": {
                    "type": "array",
                    "items": [
                        {
                        "type": "object",
                        "properties": {
                            "ctaAddress": {
                            "type": "string"
                            },
                            "ctaAmount": {
                            "type": "object",
                            "properties": {
                                "getCoin": {
                                "type": "number"
                                }
                            },
                            "required": [
                                "getCoin"
                            ]
                            },
                            "ctaTxHash": {
                            "type": "string"
                            },
                            "ctaTxIndex": {
                            "type": "number"
                            }
                        },
                        "required": [
                            "ctaAddress",
                            "ctaAmount",
                            "ctaTxHash",
                            "ctaTxIndex"
                        ]
                        }
                    ]
                    },
                    "ctsOutputs": {
                    "type": "array",
                    "items": [
                        {
                        "type": "object",
                        "properties": {
                            "ctaAddress": {
                            "type": "string"
                            },
                            "ctaAmount": {
                            "type": "object",
                            "properties": {
                                "getCoin": {
                                "type": "number"
                                }
                            },
                            "required": [
                                "getCoin"
                            ]
                            },
                            "ctaTxHash": {
                            "type": "string"
                            },
                            "ctaTxIndex": {
                            "type": "number"
                            }
                        },
                        "required": [
                            "ctaAddress",
                            "ctaAmount",
                            "ctaTxHash",
                            "ctaTxIndex"
                        ]
                        }
                    ]
                    }
                },
                "required": [
                    "ctsId",
                    "ctsTxTimeIssued",
                    "ctsBlockTimeIssued",
                    "ctsBlockHeight",
                    "ctsBlockEpoch",
                    "ctsBlockSlot",
                    "ctsBlockHash",
                    "ctsRelayedBy",
                    "ctsTotalInput",
                    "ctsTotalOutput",
                    "ctsFees",
                    "ctsInputs",
                    "ctsOutputs"
                ]
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")

    def test_stats_txs(self):
        """
         /api/stats/txs
        """
        url = "stats/txs"
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "array",
                "items": [
                    {
                    "type": "number"
                    },
                    {
                    "type": "array",
                    "items": [
                        {
                        "type": "array",
                        "items": [
                            {
                            "type": "string"
                            },
                            {
                            "type": "number"
                            }
                        ]
                        },
                        {
                        "type": "array",
                        "items": [
                            {
                            "type": "string"
                            },
                            {
                            "type": "number"
                            }
                        ]
                        }
                    ]
                    }
                ]
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")


@pytest.mark.checkschema
class TestAddresses:
    def test_addresses_summary_address(self):
        """
         /api/addresses/summary/{address}
        """
        test_address = "Ae2tdPwUPEZK72eZZqulakkhaUfTCcoaGepvQP718aYBczw5uZmp47h1k14"
        url = "addresses/summary/{address}".format(address=test_address)
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "object",
                "properties": {
                    "caAddress": {
                    "type": "string"
                    },
                    "caType": {
                    "type": "string"
                    },
                    "caChainTip": {
                    "type": "object",
                    "properties": {
                        "ctBlockNo": {
                        "type": "number"
                        },
                        "ctSlotNo": {
                        "type": "number"
                        },
                        "ctBlockHash": {
                        "type": "string"
                        }
                    },
                    "required": [
                        "ctBlockNo",
                        "ctSlotNo",
                        "ctBlockHash"
                    ]
                    },
                    "caTxNum": {
                    "type": "number"
                    },
                    "caBalance": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    },
                    "caTotalInput": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    },
                    "caTotalOutput": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    },
                    "caTotalFee": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    },
                    "caTxList": {
                    "type": "array",
                    "items": [
                        {
                        "type": "object",
                        "properties": {
                            "ctbId": {
                            "type": "string"
                            },
                            "ctbTimeIssued": {
                            "type": "number"
                            },
                            "ctbInputs": {
                            "type": "array",
                            "items": [
                                {
                                "type": "object",
                                "properties": {
                                    "ctaAddress": {
                                    "type": "string"
                                    },
                                    "ctaAmount": {
                                    "type": "object",
                                    "properties": {
                                        "getCoin": {
                                        "type": "number"
                                        }
                                    },
                                    "required": [
                                        "getCoin"
                                    ]
                                    },
                                    "ctaTxHash": {
                                    "type": "string"
                                    },
                                    "ctaTxIndex": {
                                    "type": "number"
                                    }
                                },
                                "required": [
                                    "ctaAddress",
                                    "ctaAmount",
                                    "ctaTxHash",
                                    "ctaTxIndex"
                                ]
                                }
                            ]
                            },
                            "ctbOutputs": {
                            "type": "array",
                            "items": [
                                {
                                "type": "object",
                                "properties": {
                                    "ctaAddress": {
                                    "type": "string"
                                    },
                                    "ctaAmount": {
                                    "type": "object",
                                    "properties": {
                                        "getCoin": {
                                        "type": "number"
                                        }
                                    },
                                    "required": [
                                        "getCoin"
                                    ]
                                    },
                                    "ctaTxHash": {
                                    "type": "string"
                                    },
                                    "ctaTxIndex": {
                                    "type": "number"
                                    }
                                },
                                "required": [
                                    "ctaAddress",
                                    "ctaAmount",
                                    "ctaTxHash",
                                    "ctaTxIndex"
                                ]
                                }
                            ]
                            },
                            "ctbInputSum": {
                            "type": "object",
                            "properties": {
                                "getCoin": {
                                "type": "number"
                                }
                            },
                            "required": [
                                "getCoin"
                            ]
                            },
                            "ctbOutputSum": {
                            "type": "object",
                            "properties": {
                                "getCoin": {
                                "type": "number"
                                }
                            },
                            "required": [
                                "getCoin"
                            ]
                            },
                            "ctbFees": {
                            "type": "object",
                            "properties": {
                                "getCoin": {
                                "type": "number"
                                }
                            },
                            "required": [
                                "getCoin"
                            ]
                            }
                        },
                        "required": [
                            "ctbId",
                            "ctbTimeIssued",
                            "ctbInputs",
                            "ctbOutputs",
                            "ctbInputSum",
                            "ctbOutputSum",
                            "ctbFees"
                        ]
                        }
                    ]
                    }
                },
                "required": [
                    "caAddress",
                    "caType",
                    "caChainTip",
                    "caTxNum",
                    "caBalance",
                    "caTotalInput",
                    "caTotalOutput",
                    "caTotalFee",
                    "caTxList"
                ]
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")

    def test_block_blockhash_address_address(self):
        """
        /api/block/{blockHash}/address/{address}
        """
        test_blockhash = "be5e7e719bee601fed0ea09c76e37b02d019ae2b94e669909e20d583bcda3eb8"
        test_address = "Ae2tdPwUPEZ2TjmbcZpyMLvBZ3BTrnnGzXWaaV2F4oig96dkYGsv96sDJ4Q"
        url = "block/{blockHash}/address/{address}".format(blockHash=test_blockhash, address=test_address)
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "object",
                "properties": {
                    "caAddress": {
                    "type": "string"
                    },
                    "caType": {
                    "type": "string"
                    },
                    "caChainTip": {
                    "type": "object",
                    "properties": {
                        "ctBlockNo": {
                        "type": "number"
                        },
                        "ctSlotNo": {
                        "type": "number"
                        },
                        "ctBlockHash": {
                        "type": "string"
                        }
                    },
                    "required": [
                        "ctBlockNo",
                        "ctSlotNo",
                        "ctBlockHash"
                    ]
                    },
                    "caTxNum": {
                    "type": "number"
                    },
                    "caBalance": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    },
                    "caTotalInput": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    },
                    "caTotalOutput": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    },
                    "caTotalFee": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    },
                    "caTxList": {
                    "type": "array",
                    "items": [
                        {
                        "type": "object",
                        "properties": {
                            "ctbId": {
                            "type": "string"
                            },
                            "ctbTimeIssued": {
                            "type": "number"
                            },
                            "ctbInputs": {
                            "type": "array",
                            "items": [
                                {
                                "type": "object",
                                "properties": {
                                    "ctaAddress": {
                                    "type": "string"
                                    },
                                    "ctaAmount": {
                                    "type": "object",
                                    "properties": {
                                        "getCoin": {
                                        "type": "number"
                                        }
                                    },
                                    "required": [
                                        "getCoin"
                                    ]
                                    },
                                    "ctaTxHash": {
                                    "type": "string"
                                    },
                                    "ctaTxIndex": {
                                    "type": "number"
                                    }
                                },
                                "required": [
                                    "ctaAddress",
                                    "ctaAmount",
                                    "ctaTxHash",
                                    "ctaTxIndex"
                                ]
                                }
                            ]
                            },
                            "ctbOutputs": {
                            "type": "array",
                            "items": [
                                {
                                "type": "object",
                                "properties": {
                                    "ctaAddress": {
                                    "type": "string"
                                    },
                                    "ctaAmount": {
                                    "type": "object",
                                    "properties": {
                                        "getCoin": {
                                        "type": "number"
                                        }
                                    },
                                    "required": [
                                        "getCoin"
                                    ]
                                    },
                                    "ctaTxHash": {
                                    "type": "string"
                                    },
                                    "ctaTxIndex": {
                                    "type": "number"
                                    }
                                },
                                "required": [
                                    "ctaAddress",
                                    "ctaAmount",
                                    "ctaTxHash",
                                    "ctaTxIndex"
                                ]
                                }
                            ]
                            },
                            "ctbInputSum": {
                            "type": "object",
                            "properties": {
                                "getCoin": {
                                "type": "number"
                                }
                            },
                            "required": [
                                "getCoin"
                            ]
                            },
                            "ctbOutputSum": {
                            "type": "object",
                            "properties": {
                                "getCoin": {
                                "type": "number"
                                }
                            },
                            "required": [
                                "getCoin"
                            ]
                            },
                            "ctbFees": {
                            "type": "object",
                            "properties": {
                                "getCoin": {
                                "type": "number"
                                }
                            },
                            "required": [
                                "getCoin"
                            ]
                            }
                        },
                        "required": [
                            "ctbId",
                            "ctbTimeIssued",
                            "ctbInputs",
                            "ctbOutputs",
                            "ctbInputSum",
                            "ctbOutputSum",
                            "ctbFees"
                        ]
                        }
                    ]
                    }
                },
                "required": [
                    "caAddress",
                    "caType",
                    "caChainTip",
                    "caTxNum",
                    "caBalance",
                    "caTotalInput",
                    "caTotalOutput",
                    "caTotalFee",
                    "caTxList"
                ]
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")


@pytest.mark.checkschema
class TestEpochs:
    def test_epochs_epoch(self):
        """
        /api/epochs/{epoch}
        """
        test_epoch = "0"
        url = "epochs/{epoch}".format(epoch=test_epoch)
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "array",
                "items": [
                    {
                    "type": "number"
                    },
                    {
                    "type": "array",
                    "items": [
                        {
                        "type": "object",
                        "properties": {
                            "cbeEpoch": {
                            "type": "number"
                            },
                            "cbeSlot": {
                            "type": "number"
                            },
                            "cbeBlkHeight": {
                            "type": "number"
                            },
                            "cbeBlkHash": {
                            "type": "string"
                            },
                            "cbeTimeIssued": {
                            "type": "number"
                            },
                            "cbeTxNum": {
                            "type": "number"
                            },
                            "cbeTotalSent": {
                            "type": "object",
                            "properties": {
                                "getCoin": {
                                "type": "number"
                                }
                            },
                            "required": [
                                "getCoin"
                            ]
                            },
                            "cbeSize": {
                            "type": "number"
                            },
                            "cbeBlockLead": {
                            "type": "string"
                            },
                            "cbeFees": {
                            "type": "object",
                            "properties": {
                                "getCoin": {
                                "type": "number"
                                }
                            },
                            "required": [
                                "getCoin"
                            ]
                            }
                        },
                        "required": [
                            "cbeEpoch",
                            "cbeSlot",
                            "cbeBlkHeight",
                            "cbeBlkHash",
                            "cbeTimeIssued",
                            "cbeTxNum",
                            "cbeTotalSent",
                            "cbeSize",
                            "cbeBlockLead",
                            "cbeFees"
                        ]
                        }
                    ]
                    }
                ]
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")

    def test_epochs_epoch_slot(self):
        """
        /api/epochs/{epoch}/{slot}
        """
        test_epoch = "0"
        test_slot = "0"
        url = "epochs/{epoch}/{slot}".format(epoch=test_epoch, slot=test_slot)
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "array",
                "items": [
                    {
                    "type": "object",
                    "properties": {
                        "cbeEpoch": {
                        "type": "number"
                        },
                        "cbeSlot": {
                        "type": "number"
                        },
                        "cbeBlkHeight": {
                        "type": "number"
                        },
                        "cbeBlkHash": {
                        "type": "string"
                        },
                        "cbeTimeIssued": {
                        "type": "number"
                        },
                        "cbeTxNum": {
                        "type": "number"
                        },
                        "cbeTotalSent": {
                        "type": "object",
                        "properties": {
                            "getCoin": {
                            "type": "number"
                            }
                        },
                        "required": [
                            "getCoin"
                        ]
                        },
                        "cbeSize": {
                        "type": "number"
                        },
                        "cbeBlockLead": {
                        "type": "string"
                        },
                        "cbeFees": {
                        "type": "object",
                        "properties": {
                            "getCoin": {
                            "type": "number"
                            }
                        },
                        "required": [
                            "getCoin"
                        ]
                        }
                    },
                    "required": [
                        "cbeEpoch",
                        "cbeSlot",
                        "cbeBlkHeight",
                        "cbeBlkHash",
                        "cbeTimeIssued",
                        "cbeTxNum",
                        "cbeTotalSent",
                        "cbeSize",
                        "cbeBlockLead",
                        "cbeFees"
                    ]
                    }
                ]
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")


@pytest.mark.checkschema
class TestGenesis:
    def test_genesis_summary(self):
        """
         /api/genesis/summary
        """
        url = "genesis/summary"
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "object",
                "properties": {
                    "cgsNumTotal": {
                    "type": "number"
                    },
                    "cgsNumRedeemed": {
                    "type": "number"
                    },
                    "cgsNumNotRedeemed": {
                    "type": "number"
                    },
                    "cgsRedeemedAmountTotal": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    },
                    "cgsNonRedeemedAmountTotal": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    }
                },
                "required": [
                    "cgsNumTotal",
                    "cgsNumRedeemed",
                    "cgsNumNotRedeemed",
                    "cgsRedeemedAmountTotal",
                    "cgsNonRedeemedAmountTotal"
                ]
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")

    def test_genesis_address_pages_total(self):
        """
         /api/genesis/address/pages/total
        """
        url = "genesis/address/pages/total"
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "number"
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")

    def test_genesis_address(self):
        """
        /api/genesis/address
        """
        url = "genesis/address"
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "object",
                "properties": {
                    "cgaiCardanoAddress": {
                    "type": "string"
                    },
                    "cgaiGenesisAmount": {
                    "type": "object",
                    "properties": {
                        "getCoin": {
                        "type": "number"
                        }
                    },
                    "required": [
                        "getCoin"
                    ]
                    },
                    "cgaiIsRedeemed": {
                    "type": "boolean"
                    }
                },
                "required": [
                    "cgaiCardanoAddress",
                    "cgaiGenesisAmount",
                    "cgaiIsRedeemed"
                ]
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")

    def test_supply_ada(self):
        """
        /api/supply/ada
        """
        url = "supply/ada"
        expected_schema = {
            "type": "object",
            "properties": {
                "Right": {
                "type": "number"
                }
            },
            "required": [
                "Right"
            ]
        }

        response = requests.get(API_HOST + url)
        assert(response.status_code, 200)

        try:
            datum = json.loads(response.content)
            validate(datum, expected_schema)
        except exceptions.ValidationError:
            pytest.fail("well-formed but invalid JSON:")
        except json.decoder.JSONDecodeError:
            pytest.fail("poorly-formed text, not JSON:")
