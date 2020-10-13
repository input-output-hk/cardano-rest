package com.cardano.rest.tests.functional.smoke_tests;

import com.cardano.rest.tests.functional.BaseTest;
import io.qameta.allure.Description;
import io.restassured.http.ContentType;
import org.testng.annotations.Test;

import static io.restassured.RestAssured.given;
import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchemaInClasspath;

public class AddressesTest extends BaseTest {

    @Test
    @Description("addresses/summary/{address} responds")
    public void addressesSummaryAddress_basicResponse_test() {
        String endpoint = String.format("addresses/summary/%s", this.dataStore.getAddressHash());
        String url = this.host + endpoint;

        given().
        when().
            get(url).
        then().
            assertThat().
            statusCode(200).
        and().
            contentType(ContentType.JSON);
    }

    @Test
    @Description("addresses/summary/{address} matches expected JSON schema")
    public void addressesSummaryAddress_validSchema_test() {
        String endpoint = String.format("addresses/summary/%s", this.dataStore.getAddressHash());
        String url = this.host + endpoint;

        given().
            get(url).
        then().
            assertThat().
            body(matchesJsonSchemaInClasspath("schemas/valid-addresses-summary-address-schema.json"));
    }

    @Test
    @Description("block/{blockHash}/address/{address} responds")
    public void blockBlockhashAddressAddress_basicResponse_test() {
        String[] testData = this.dataStore.getBlockHashAndAddress().split(" ");
        String endpoint = String.format("block/%s/address/%s", testData[0], testData[1]);
        String url = this.host + endpoint;

        given().
        when().
            get(url).
        then().
            assertThat().
            statusCode(200).
        and().
            contentType(ContentType.JSON);
    }

    @Test
    @Description("block/{blockHash}/address/{address} matches expected JSON schema")
    public void blockBlockhashAddressAddress_validSchema_test() {
        String[] testData = this.dataStore.getBlockHashAndAddress().split(" ");
        String endpoint = String.format("block/%s/address/%s", testData[0], testData[1]);
        String url = this.host + endpoint;

        given().
            get(url).
        then().
            assertThat().
            body(matchesJsonSchemaInClasspath("schemas/valid-block-blockhash-address-address-schema.json"));
    }
}
