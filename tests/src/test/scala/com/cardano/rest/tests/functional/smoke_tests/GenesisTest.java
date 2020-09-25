package com.cardano.rest.tests.functional.smoke_tests;

import com.cardano.rest.tests.functional.BaseTest;
import io.qameta.allure.Description;
import io.restassured.http.ContentType;
import org.testng.annotations.Test;

import static io.restassured.RestAssured.given;
import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchemaInClasspath;

public class GenesisTest extends BaseTest {

    @Test
    @Description("genesis/summary responds")
    public void genesisSummary_basicResponse_test() {
        String endpoint = "genesis/summary";
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
    @Description("genesis/summary matches expected JSON schema")
    public void genesisSummary_validSchema_test() {
        String endpoint = "genesis/summary";
        String url = this.host + endpoint;

        given().
            get(url).
        then().
            assertThat().
            body(matchesJsonSchemaInClasspath("schemas/valid-genesis-summary-schema.json"));
    }

    @Test
    @Description("genesis/address/pages/total responds")
    public void genesisAddressPagesTotal_basicResponse_test() {
        String endpoint = "genesis/address/pages/total";
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

    @Test(dependsOnMethods = "genesisAddressPagesTotal_basicResponse_test")
    @Description("genesis/address/pages/total matches expected JSON schema")
    public void genesisAddressPagesTotal_validSchema_test() {
        String endpoint = "genesis/address/pages/total";
        String url = this.host + endpoint;

        given().
            get(url).
        then().
            assertThat().
            body(matchesJsonSchemaInClasspath("schemas/valid-genesis-address-pages-total-schema.json"));
    }

    @Test
    @Description("genesis/address responds")
    public void genesisAddress_basicResponse_test() {
        String endpoint = "genesis/address";
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

    @Test(dependsOnMethods = "genesisAddress_basicResponse_test")
    @Description("genesis/address matches expected JSON schema")
    public void genesisAddress_validSchema_test() {
        String endpoint = "genesis/address";
        String url = this.host + endpoint;

        given().
            get(url).
        then().
            assertThat().
            body(matchesJsonSchemaInClasspath("schemas/valid-genesis-address-schema.json"));
    }

    @Test
    @Description("supply/ada responds")
    public void supplyAda_basicResponse_test() {
        String endpoint = "supply/ada";
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

    @Test(dependsOnMethods = "supplyAda_basicResponse_test")
    @Description("supply/ada matches expected JSON schema")
    public void supplyAda_validSchema_test() {
        String endpoint = "supply/ada";
        String url = this.host + endpoint;

        given().
            get(url).
        then().
            assertThat().
            body(matchesJsonSchemaInClasspath("schemas/valid-supply-ada-schema.json"));
    }
}
