package com.cardano.rest.tests.functional.smoke_tests;

import com.cardano.rest.tests.functional.BaseTest;
import io.qameta.allure.Description;
import io.restassured.http.ContentType;
import org.testng.annotations.Test;

import static io.restassured.RestAssured.given;
import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchemaInClasspath;

public class EpochsTest extends BaseTest {

    @Test
    @Description("epochs/{epoch} responds")
    public void epochsEpoch_basicResponse_test() {
        String endpoint = String.format("epochs/%s", this.dataStore.getEpoch());
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
    @Description("epochs/{epoch} matches expected JSON schema")
    public void epochsEpoch_validSchema_test() {
        String endpoint = String.format("epochs/%s", this.dataStore.getEpoch());
        String url = this.host + endpoint;

        given().
            get(url)
        .then()
            .assertThat()
            .body(matchesJsonSchemaInClasspath("schemas/valid-epochs-epoch-schema.json"));
    }

    @Test
    @Description("epochs/{epoch}/{slot} responds")
    public void epochsEpochSlot_basicResponse_test() {
        String endpoint = String.format("epochs/%s/%s", this.dataStore.getEpoch(), this.dataStore.getSlot());
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
    @Description("epochs/{epoch}/{slot} matches expected JSON schema")
    public void epochsEpochSlot_validSchema_test() {
        String endpoint = String.format("epochs/%s/%s", this.dataStore.getEpoch(), this.dataStore.getSlot());
        String url = this.host + endpoint;

        given().
            get(url)
        .then()
            .assertThat()
            .body(matchesJsonSchemaInClasspath("schemas/valid-epochs-epoch-slot-schema.json"));
    }
}
