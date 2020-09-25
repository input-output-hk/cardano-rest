package com.cardano.rest.tests.functional.data_validation;

import com.cardano.rest.tests.functional.BaseTest;
import io.qameta.allure.Description;
import org.testng.annotations.Parameters;
import org.testng.annotations.Test;

import static org.hamcrest.Matchers.equalTo;
import static io.restassured.RestAssured.given;
import static org.hamcrest.Matchers.is;

public class AddressesTest extends BaseTest {

    @Test
    @Parameters({"address", "caAddress"})
    @Description("addresses/summary/{address} returns the expected address")
    public void addressesSummaryAddress_returnsCorrectAddress_test(String address, String caAddress) {
        String endpoint = String.format("addresses/summary/%s", address);
        String url = this.host + endpoint;

        given().
                get(url).
        then().
                statusCode(200).
                body("Right.caAddress", equalTo(caAddress));
    }

    @Test
    @Parameters({"address", "caType"})
    @Description("addresses/summary/{address} returns the expected type")
    public void addressesSummaryAddress_returnsCorrectType_test(String address, String caType) {
        String endpoint = String.format("addresses/summary/%s", address);
        String url = this.host + endpoint;

        given().
                get(url).
        then().
                statusCode(200).
                body("Right.caType", equalTo(caType));
    }

    @Test
    @Parameters({"address", "ctbId"})
    @Description("addresses/summary/{address} returns the expected Id")
    public void addressesSummaryAddress_returnsExpectedIdInTxList_test(String address, String ctbId) {
        String endpoint = String.format("addresses/summary/%s", address);
        String url = this.host + endpoint;

        given().
                get(url).
        then().
                statusCode(200).
                body("Right.caTxList[0].ctbId", is(ctbId));
    }

    @Test
    @Parameters({"address", "ctaAmount"})
    @Description("addresses/summary/{address} returns the expected Amount")
    public void addressesSummaryAddress_returnsExpectedAmountInTxList_test(String address, String ctaAmount) {
        String endpoint = String.format("addresses/summary/%s", address);
        String url = this.host + endpoint;

        given().
                get(url).
        then().
                statusCode(200).
                body("Right.caTxList[0].ctbInputs.ctaAmount.getCoin[0]", equalTo(ctaAmount));
    }

    @Test
    @Parameters({"address", "ctaTxHash"})
    @Description("addresses/summary/{address} returns the expected Tx Hash")
    public void addressesSummaryAddress_returnsExpectedTxHashInTxList_test(String address, String ctaTxHash) {
        String endpoint = String.format("addresses/summary/%s", address);
        String url = this.host + endpoint;

        given().
                get(url).
        then().
                statusCode(200).
                body("Right.caTxList[0].ctbInputs.ctaTxHash[0]", equalTo(ctaTxHash));
    }
}
