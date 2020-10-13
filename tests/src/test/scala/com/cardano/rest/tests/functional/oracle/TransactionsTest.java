package com.cardano.rest.tests.functional.oracle;

import com.cardano.rest.tests.functional.ApiResponseComparison;
import com.cardano.rest.tests.functional.BaseTest;
import io.qameta.allure.Description;
import io.restassured.http.ContentType;
import org.testng.annotations.Test;

import static io.restassured.RestAssured.given;
import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchemaInClasspath;
import static org.testng.AssertJUnit.assertEquals;

public class TransactionsTest extends BaseTest {

    @Test
    @Description("txs/last has not changed against the oracle")
    public void txsLast_basicResponse_test() {
        ApiResponseComparison responses = compareTwoResponses("txs/last");

        assertEquals(responses.Response, responses.OracleResponse);
    }

    @Test
    @Description("txs/summary/{tx} has not changed against the oracle")
    public void txsSummaryTxid_basicResponse_test() {
        String transaction = this.dataStore.getTransactionHash();
        ApiResponseComparison responses = compareTwoResponses("txs/summary/" + transaction);

        assertEquals(responses.Response, responses.OracleResponse);
    }

    @Test
    @Description("stats/txs has not changed against the oracle")
    public void statsTxs_basicResponse_test() {
        ApiResponseComparison responses = compareTwoResponses("stats/txs");

        assertEquals(responses.Response, responses.OracleResponse);
    }
}
