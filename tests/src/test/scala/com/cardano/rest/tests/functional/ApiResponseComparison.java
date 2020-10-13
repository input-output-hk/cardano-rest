package com.cardano.rest.tests.functional;


public class ApiResponseComparison {
    public String Response;
    public String OracleResponse;

    public ApiResponseComparison(String response, String oracleResponse) {
        this.Response = response;
        this.OracleResponse = oracleResponse;
    }
}
