import requests
import unittest

EXISTING_API_HOST = "https://localhost/"
NEW_API_HOST = "https://localhost/"
NETWORK = "mainnet"


class HttpBridge(unittest.TestCase):
    def test_network_utxos_address(self):
        """
         /{network}/utxos/{address}
        """
        test_address = "Ae2tdPwUPEZK72eZZqulakkhaUfTCcoaGepvQP718aYBczw5uZmp47h1k14"
        url = "/{network}/utxos/{address}".format(network=NETWORK, address=test_address)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)


class Blocks(unittest.TestCase):
    def test_blocks_pages(self):
        """
        /api/blocks/pages
        """
        url = "api/blocks/pages"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)

    def test_blocks_pages_total(self):
        """
        /api/blocks/pages/total
        """
        url = "/api/blocks/pages/total"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)

    def test_blocks_summary_blockhash(self):
        """
        /api/blocks/summary/{blockHash}
        """
        test_blockhash = "3c89f7d9ff6c06468e32fd916d153b033264f780e11fca7750cb85f56d4f31d0"
        url = "api/blocks/summary/{blockHash}".format(blockHash=test_blockhash)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)

    def list_blocks_txs_blockhash(self):
        """
        /api/blocks/txs/{blockHash}
        """
        test_blockhash = "3c89f7d9ff6c06468e32fd916d153b033264f780e11fca7750cb85f56d4f31d0"
        url = "api/blocks/txs/{blockHash}".format(blockHash=test_blockhash)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)


class Transactions(unittest.TestCase):
    def test_txs_last(self):
        """
         /api/txs/last
        """
        url = "api/txs/last"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)

    def test_txs_summary_txId(self):
        """
        /api/txs/summary/{txId}
        """
        test_tx_id = "3c89f7d9ff6c06468e32fd916d153b033264f780e11fca7750cb85f56d4f31d0"
        url = "api/txs/summary/{txId}".format(txId=test_tx_id)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)

    def test_get_summary(self):
        """
        /api/txs/summary/{txId}
        """
        test_tx_id = ""
        url = "api/txs/summary/{txId}".format(txId=test_tx_id)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)

    def test_stats_txs(self):
        """
         /api/stats/txs
        """
        url = "api/stats/txs"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)


class Addresses(unittest.TestCase):
    def test_addresses_summary_address(self):
        """
         /api/addresses/summary/{address}
        """
        test_address = "Ae2tdPwUPEZK72eZZqulakkhaUfTCcoaGepvQP718aYBczw5uZmp47h1k14"
        url = " api/addresses/summary/{address}".format(address=test_address)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)

    def test_block_blockhash_address_address(self):
        """
        /api/block/{blockHash}/address/{address}
        """
        test_blockhash = "Ae2tdPwUPEZK72eZZqulakkhaUfTCcoaGepvQP718aYBczw5uZmp47h1k14"
        test_address = "3c89f7d9ff6c06468e32fd916d153b033264f780e11fca7750cb85f56d4f31d0"
        url = "api/block/{blockHash}/address/{address}".format(blockHash=test_blockhash, address=test_address)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)


class Epochs(unittest.TestCase):
    def test_epochs_epoch(self):
        """
        /api/epochs/{epoch}
        """
        test_epoch = "0"
        url = "api/epochs/{epoch}".format(epoch=test_epoch)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)

    def test_epochs_epoch_slot(self):
        """
        /api/epochs/{epoch}/{slot}
        """
        test_epoch = "0"
        test_slot = "0"
        url = "api/epochs/{epoch}/{slot}".format(epoch=test_epoch, slot=test_slot)

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)


class Genesis(unittest.TestCase):
    def test_genesis_summary(self):
        """
         /api/genesis/summary
        """
        url = "api/genesis/summary"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)

    def test_genesis_address_pages_total(self):
        """
         /api/genesis/address/pages/total
        """
        url = "api/genesis/address/pages/total"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)

    def test_genesis_address(self):
        """
        /api/genesis/address
        """
        url = "api/genesis/address"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)

    def test_supply_ada(self):
        """
        /api/supply/ada
        """
        url = "api/supply/ada"

        existing_api_response = requests.get(EXISTING_API_HOST + url)
        new_api_response = requests.get(NEW_API_HOST + url)

        self.assertEqual(existing_api_response.status_code == 200)
        self.assertEqual(new_api_response.status_code == 200)

        self.assertEqual(existing_api_response.content == new_api_response.content)


if __name__ == '__main__':
    unittest.main()
