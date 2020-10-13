package com.cardano.rest.tests.functional.oracle;

import com.cardano.rest.tests.functional.ApiResponseComparison;
import com.cardano.rest.tests.functional.BaseTest;
import io.qameta.allure.Description;
import io.restassured.http.ContentType;
import org.testng.annotations.Test;

import static io.restassured.RestAssured.given;
import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchemaInClasspath;
import static org.testng.AssertJUnit.assertEquals;

public class GenesisTest extends BaseTest {

    @Test
    @Description("genesis/summary has not changed against the oracle")
    public void genesisSummary_basicResponse_test() {
        ApiResponseComparison responses = compareTwoResponses("genesis/summary");

        assertEquals(responses.Response, responses.OracleResponse);
    }

    @Test
    @Description("genesis/address/pages/total has not changed against the oracle")
    public void genesisAddressPagesTotal_basicResponse_test() {
        ApiResponseComparison responses = compareTwoResponses("genesis/address/pages/total");

        assertEquals(responses.Response, responses.OracleResponse);
    }

    @Test
    @Description("genesis/address has not changed against the oracle")
    public void genesisAddress_basicResponse_test() {
        ApiResponseComparison responses = compareTwoResponses("genesis/address");

        assertEquals(responses.Response, responses.OracleResponse);
    }

    @Test
    @Description("supply/ada has not changed against the oracle")
    public void supplyAda_basicResponse_test() {
        ApiResponseComparison responses = compareTwoResponses("supply/ada");

        assertEquals(responses.Response, responses.OracleResponse);
    }
}
