# submit-api

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

=== "cardano-graphql"

    ```graphql
    mutation submitTransaction($transaction) {
      submitTransaction(transaction: $transaction) {
          hash
      }
    }
    ```

    Where `$transaction` corresponds to a CBOR-serialized transaction, e.g. as output from `cardano-cli`. 
  
=== "cardano-rosetta"

    This can be done via the [/construction/submit](https://www.rosetta-api.org/docs/ConstructionApi.html#constructionsubmit) endpoint.

=== "cardano-submit-api"

    Follow the OpenAPI specification defined [here](https://github.com/input-output-hk/cardano-node/blob/master/cardano-submit-api/swagger.yaml). For any question, please open a ticket 
    on input-output-hk/cardano-node.
