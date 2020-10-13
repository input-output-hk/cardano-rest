package com.cardano.rest.tests.functional.data_intensive;

import com.cardano.rest.tests.functional.BaseTest;
import io.qameta.allure.Description;
import io.restassured.http.ContentType;
import org.testng.annotations.Test;

import java.util.concurrent.TimeUnit;

import static io.restassured.RestAssured.given;
import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchemaInClasspath;

public class AddressesTest extends BaseTest {

    @Test(invocationCount = 100, threadPoolSize = 2)
    @Description("addresses/summary/{address} matches expected JSON schema")
    public void addressesSummaryAddress_validSchema_test() throws InterruptedException {
        String endpoint = String.format("addresses/summary/%s", this.dataStore.getAddressHash());
        String url = this.host + endpoint;

        given().
            get(url).
        then().
            assertThat().
            body(matchesJsonSchemaInClasspath("schemas/valid-addresses-summary-address-schema.json"));

        TimeUnit.SECONDS.sleep(1);
    }

    @Test(invocationCount = 100, threadPoolSize = 2)
    @Description("block/{blockHash}/address/{address} matches expected JSON schema")
    public void blockBlockhashAddressAddress_validSchema_test() throws InterruptedException {
        String[] testData = this.dataStore.getBlockHashAndAddress().split(" ");
        String endpoint = String.format("block/%s/address/%s", testData[0], testData[1]);
        String url = this.host + endpoint;

        given().
            get(url).
        then().
            assertThat().
            body(matchesJsonSchemaInClasspath("schemas/valid-block-blockhash-address-address-schema.json"));

        TimeUnit.SECONDS.sleep(1);
    }
}
