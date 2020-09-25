package com.cardano.rest.tests.functional.oracle;

import com.cardano.rest.tests.functional.ApiResponseComparison;
import com.cardano.rest.tests.functional.BaseTest;
import io.qameta.allure.Description;
import org.testng.annotations.Test;

import static org.testng.AssertJUnit.assertEquals;

public class AddressesTest extends BaseTest {

    @Test
    @Description("addresses/summary/{address} has not changed against the oracle")
    public void addressesSummaryAddress_oracleTest() {
        String addressHash = this.dataStore.getAddressHash();
        ApiResponseComparison responses = compareTwoResponses("addresses/summary/" + addressHash);

        assertEquals(responses.Response, responses.OracleResponse);
    }

    @Test
    @Description("block/{blockHash}/address/{address} has not changed against the oracle")
    public void blockBlockhashAddressAddress_oracleTest() {
        String[] testData = this.dataStore.getBlockHashAndAddress().split(" ");
        ApiResponseComparison responses = compareTwoResponses("block/" + testData[0] + "/address/" + testData[1]);

        assertEquals(responses.Response, responses.OracleResponse);
    }
}
