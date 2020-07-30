import requests
import pytest

API_HOST = "https://explorer.cardano.org/api/"
NETWORK = "mainnet"


@pytest.mark.data
class TestAddressData:
    def test_addresses_can_decode(self):
        """
         /api/addresses/summary/{address}
        """
        test_address = "addr1q8cs8j8rv43v59ggt9zhjhngltt7qnz47zc6ypysptzd5p8kf0aj8j5dkt80t5232tf2gj28wy3hzxxam6dre965k6eslaw3az"
        url = "addresses/summary/{address}".format(address=test_address)

        api_response = requests.get(API_HOST + url)

        bad_response = '{"Left":"Unable to decode address Ae2tdPwUPEZK72eZZqulakkhaUfTCcoaGepvQP718aYBczw5uZmp47h1k14."}'

        assert(api_response.status_code == 200)
        assert(api_response.content != bad_response)
