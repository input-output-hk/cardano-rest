package com.cardano.rest.tests.functional.data_intensive;

import com.cardano.rest.tests.functional.BaseTest;
import io.qameta.allure.Description;
import io.restassured.http.ContentType;
import org.testng.annotations.Test;

import java.util.concurrent.TimeUnit;

import static io.restassured.RestAssured.given;
import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchemaInClasspath;

public class EpochsTest extends BaseTest {

    @Test(invocationCount = 100, threadPoolSize = 2)
    @Description("epochs/{epoch} matches expected JSON schema")
    public void epochsEpoch_validSchema_test() throws InterruptedException {
        String endpoint = String.format("epochs/%s", this.dataStore.getEpoch());
        String url = this.host + endpoint;

        given().
            get(url).
        then().
            assertThat().
            body(matchesJsonSchemaInClasspath("schemas/valid-epochs-epoch-schema.json"));

        TimeUnit.SECONDS.sleep(1);
    }

    @Test(invocationCount = 100, threadPoolSize = 2)
    @Description("epochs/{epoch}/{slot} matches expected JSON schema")
    public void epochsEpochSlot_validSchema_test() throws InterruptedException {
        String endpoint = String.format("epochs/%s/%s", this.dataStore.getEpoch(), this.dataStore.getSlot());
        String url = this.host + endpoint;

        given().
            get(url)
        .then()
            .assertThat()
            .body(matchesJsonSchemaInClasspath("schemas/valid-epochs-epoch-slot-schema.json"));

        TimeUnit.SECONDS.sleep(1);
    }
}
