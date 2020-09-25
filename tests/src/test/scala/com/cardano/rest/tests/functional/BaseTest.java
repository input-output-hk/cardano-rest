package com.cardano.rest.tests.functional;

import io.restassured.RestAssured;
import io.restassured.response.Response;
import io.restassured.specification.RequestSpecification;
import org.testng.annotations.BeforeSuite;
import org.testng.annotations.BeforeTest;

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import com.cardano.rest.tests.DataStore;

import static org.testng.AssertJUnit.assertEquals;

public class BaseTest {

    protected String host;
    protected String oracle;
    protected DataStore dataStore;

    @BeforeSuite
    public void setupProperties() {
    }

    @BeforeTest
    public void setupTests() {
        this.dataStore = new DataStore();

        String resourceName = "config.properties"; // could also be a constant
        ClassLoader loader = Thread.currentThread().getContextClassLoader();
        Properties props = new Properties();
        try(InputStream resourceStream = loader.getResourceAsStream(resourceName)) {
            props.load(resourceStream);
        } catch (IOException e) {
            e.printStackTrace();
        }

        this.host = props.getProperty("host");
        this.oracle = props.getProperty("oracle");
    }

    public ApiResponseComparison compareTwoResponses(String endpoint) {
        String host_url = this.host + endpoint;
        String oracle_url = this.oracle + endpoint;

        RequestSpecification httpRequest = RestAssured.given();
        Response response = httpRequest.get(host_url);
        Response oracleResponse = httpRequest.get(oracle_url);

        String responseJson = response.getBody().asString();
        String oracleResponseJson = oracleResponse.getBody().asString();

        return new ApiResponseComparison(responseJson, oracleResponseJson);
    }
}
