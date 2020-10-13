package com.cardano.rest.tests.functional.oracle;

import com.cardano.rest.tests.functional.ApiResponseComparison;
import com.cardano.rest.tests.functional.BaseTest;
import io.qameta.allure.Description;
import io.restassured.http.ContentType;
import org.testng.annotations.Test;

import static io.restassured.RestAssured.given;
import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchemaInClasspath;
import static org.testng.AssertJUnit.assertEquals;

public class BlocksTest extends BaseTest {

    @Test
    @Description("blocks/pages has not changed against the oracle")
    public void blocksPages_basicResponse_test() {
        ApiResponseComparison responses = compareTwoResponses("blocks/pages");

        assertEquals(responses.Response, responses.OracleResponse);
    }

    @Test
    @Description("blocks/pages/total has not changed against the oracle")
    public void blocksPagesTotal_basicResponse_test() {
        ApiResponseComparison responses = compareTwoResponses("blocks/pages/total");

        assertEquals(responses.Response, responses.OracleResponse);
    }

    @Test
    @Description("blocks/summary/{blockHash} has not changed against the oracle")
    public void blocksSummaryBlockhash_basicResponse_test() {
        String blockHash = this.dataStore.getBlockHash();
        ApiResponseComparison responses = compareTwoResponses("blocks/summary/" + blockHash);

        assertEquals(responses.Response, responses.OracleResponse);
    }

    @Test
    @Description("blocks/txs/{blockHash} has not changed against the oracle")
    public void blocksTxsBlockhash_basicResponse_test() {
        String blockHash = this.dataStore.getBlockHash();
        ApiResponseComparison responses = compareTwoResponses("blocks/txs/" + blockHash);

        assertEquals(responses.Response, responses.OracleResponse);
    }
}
