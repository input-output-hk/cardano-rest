import requests
import pytest

EXISTING_API_HOST = "https://explorer.cardano.org/api/"
NEW_API_HOST = "https://explorer.cardano.org/api/"
NETWORK = "mainnet"


@pytest.mark.regression
class TestHttpBridge:
    def test_network_utxos_address(self):
        """
         /{network}/utxos/{address}
        """
        test_address = "Ae2tdPwUPEZLzyDSkCj5APms4VBpDqdhuAvZio5EktwdDjYs2JX7gMdyHP1"
        url = "{network}/utxos/{address}".format(network=NETWORK, address=test_address)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)


@pytest.mark.regression
class TestBlocks:
    def test_blocks_pages(self):
        """
        /api/blocks/pages
        """
        url = "blocks/pages"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)

    def test_blocks_pages_total(self):
        """
        /api/blocks/pages/total
        """
        url = "blocks/pages/total"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)

    def test_blocks_summary_blockhash(self):
        """
        /api/blocks/summary/{blockHash}
        """
        test_blockhash = "3c89f7d9ff6c06468e32fd916d153b033264f780e11fca7750cb85f56d4f31d0"
        url = "blocks/summary/{blockHash}".format(blockHash=test_blockhash)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)

    def list_blocks_txs_blockhash(self):
        """
        /api/blocks/txs/{blockHash}
        """
        test_blockhash = "3c89f7d9ff6c06468e32fd916d153b033264f780e11fca7750cb85f56d4f31d0"
        url = "blocks/txs/{blockHash}".format(blockHash=test_blockhash)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)


@pytest.mark.regression
class TestTransactions:
    def test_txs_last(self):
        """
         /api/txs/last
        """
        url = "txs/last"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)

    def test_txs_summary_txId(self):
        """
        /api/txs/summary/{txId}
        """
        test_tx_id = "3c89f7d9ff6c06468e32fd916d153b033264f780e11fca7750cb85f56d4f31d0"
        url = "txs/summary/{txId}".format(txId=test_tx_id)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)

    def test_get_summary(self):
        """
        /api/txs/summary/{txId}
        """
        test_tx_id = "Ae2tdPwUPEZLzyDSkCj5APms4VBpDqdhuAvZio5EktwdDjYs2JX7gMdyHP1"
        url = "txs/summary/{txId}".format(txId=test_tx_id)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)

    def test_stats_txs(self):
        """
         /api/stats/txs
        """
        url = "stats/txs"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)


@pytest.mark.regression
class TestAddresses:
    def test_addresses_summary_address(self):
        """
         /api/addresses/summary/{address}
        """
        test_address = "Ae2tdPwUPEZK72eZZqulakkhaUfTCcoaGepvQP718aYBczw5uZmp47h1k14"
        url = "addresses/summary/{address}".format(address=test_address)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)

    def test_block_blockhash_address_address(self):
        """
        /api/block/{blockHash}/address/{address}
        """
        test_blockhash = "be5e7e719bee601fed0ea09c76e37b02d019ae2b94e669909e20d583bcda3eb8"
        test_address = "Ae2tdPwUPEZ2TjmbcZpyMLvBZ3BTrnnGzXWaaV2F4oig96dkYGsv96sDJ4Q"
        url = "block/{blockHash}/address/{address}".format(blockHash=test_blockhash, address=test_address)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)


@pytest.mark.regression
class TestEpochs:
    def test_epochs_epoch(self):
        """
        /api/epochs/{epoch}
        """
        test_epoch = "0"
        url = "epochs/{epoch}".format(epoch=test_epoch)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)

    def test_epochs_epoch_slot(self):
        """
        /api/epochs/{epoch}/{slot}
        """
        test_epoch = "0"
        test_slot = "0"
        url = "epochs/{epoch}/{slot}".format(epoch=test_epoch, slot=test_slot)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)


@pytest.mark.regression
class TestGenesis:
    def test_genesis_summary(self):
        """
         /api/genesis/summary
        """
        url = "genesis/summary"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)

    def test_genesis_address_pages_total(self):
        """
         /api/genesis/address/pages/total
        """
        url = "genesis/address/pages/total"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)

    def test_genesis_address(self):
        """
        /api/genesis/address
        """
        url = "genesis/address"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)

    def test_supply_ada(self):
        """
        /api/supply/ada
        """
        url = "supply/ada"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        assert(existing_api_response.status_code == 200)
        assert(new_api_response.status_code == 200)

        assert(existing_api_response.content == new_api_response.content)
