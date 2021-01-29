# submit-api

GraphQL doesn't support submission of data, so the following endpoints
are only provided by Rosetta.

## Transactions

#### [/api/submit/tx](https://input-output-hk.github.io/cardano-rest/submit-api/#operation/postTransaction)

Submit an already serialized transaction to the network.

=== "cardano-rest"

    Assuming `data` is a serialized transaction on the file-system.
    ```console
    $ curl -X POST --header "Content-Type: application/cbor" --data-binary @data http://localhost:8101/api/submit/tx
    ```

    <details>
      <summary>see JSON response</summary>
    ```json
    92bcd06b25dfbd89b578d536b4d3b7dd269b7c2aa206ed518012cffe0444d67f
    ```
    </details>

=== "cardano-rosetta"

    This can be done via the [/construction/submit](https://www.rosetta-api.org/docs/ConstructionApi.html#constructionsubmit) endpoint.
    Response, request and data are documented at there.
