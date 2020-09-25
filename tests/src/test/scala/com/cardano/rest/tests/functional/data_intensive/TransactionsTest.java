package com.cardano.rest.tests.functional.data_intensive;

import com.cardano.rest.tests.functional.BaseTest;
import io.qameta.allure.Description;
import io.restassured.http.ContentType;
import org.testng.annotations.Test;

import java.util.concurrent.TimeUnit;

import static io.restassured.RestAssured.given;
import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchemaInClasspath;

public class TransactionsTest extends BaseTest {

    @Test(invocationCount = 100, threadPoolSize = 2)
    @Description("txs/summary/{tx} matches expected JSON schema")
    public void txsSummaryTxid_validSchema_test() throws InterruptedException {
        String endpoint = String.format("txs/summary/%s", this.dataStore.getTransactionHash());
        String url = this.host + endpoint;

        given().
            get(url).
        then().
            assertThat().
            body(matchesJsonSchemaInClasspath("schemas/valid-txs-summary-txid-schema.json"));

        TimeUnit.SECONDS.sleep(1);
    }
}
