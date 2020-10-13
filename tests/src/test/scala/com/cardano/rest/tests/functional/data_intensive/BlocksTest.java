package com.cardano.rest.tests.functional.data_intensive;

import com.cardano.rest.tests.functional.BaseTest;
import io.qameta.allure.Description;
import io.restassured.http.ContentType;
import org.testng.annotations.Test;

import java.util.concurrent.TimeUnit;

import static io.restassured.RestAssured.given;
import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchemaInClasspath;

public class BlocksTest extends BaseTest {

    @Test(invocationCount = 100, threadPoolSize = 2)
    @Description("blocks/summary/{blockHash} matches expected JSON schema")
    public void blocksSummaryBlockhash_validSchema_test() throws InterruptedException {
        String endpoint = String.format("blocks/summary/%s", this.dataStore.getBlockHash());
        String url = this.host + endpoint;

        given().
            get(url).
        then().
            assertThat().
            body(matchesJsonSchemaInClasspath("schemas/valid-blocks-summary-blockhash-schema.json"));

        TimeUnit.SECONDS.sleep(1);
    }

    @Test(invocationCount = 100, threadPoolSize = 2)
    @Description("blocks/txs/{blockHash} matches expected JSON schema")
    public void blocksTxsBlockhash_validSchema_test() throws InterruptedException {
        String endpoint = String.format("blocks/summary/%s", this.dataStore.getBlockHash());
        String url = this.host + endpoint;

        given().
            get(url).
        then().
            assertThat().
            body(matchesJsonSchemaInClasspath("schemas/valid-blocks-txs-blockhash-schema.json"));

        TimeUnit.SECONDS.sleep(1);
    }
}
