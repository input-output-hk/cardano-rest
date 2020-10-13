package com.cardano.rest.tests.functional.oracle;

import com.cardano.rest.tests.functional.ApiResponseComparison;
import com.cardano.rest.tests.functional.BaseTest;
import io.qameta.allure.Description;
import io.restassured.http.ContentType;
import org.testng.annotations.Test;

import static io.restassured.RestAssured.given;
import static io.restassured.module.jsv.JsonSchemaValidator.matchesJsonSchemaInClasspath;
import static org.testng.AssertJUnit.assertEquals;

public class EpochsTest extends BaseTest {

    @Test
    @Description("epochs/{epoch} has not changed against the oracle")
    public void epochsEpoch_basicResponse_test() {
        String epoch = this.dataStore.getEpoch();
        ApiResponseComparison responses = compareTwoResponses("epochs/" + epoch);

        assertEquals(responses.Response, responses.OracleResponse);
    }

    @Test
    @Description("epochs/{epoch}/{slot} has not changed against the oracle")
    public void epochsEpochSlot_basicResponse_test() {
        String epoch = this.dataStore.getEpoch();
        String slot = this.dataStore.getSlot();
        ApiResponseComparison responses = compareTwoResponses("epochs/" + epoch + "/" + slot);

        assertEquals(responses.Response, responses.OracleResponse);
    }
}
