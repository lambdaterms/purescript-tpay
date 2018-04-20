# purescript-tpay

Implements integration with Tpay's [integration without API](https://docs.tpay.com/#!/Tpay/tpay_no_api).
Contains data structures
 - `Request` which contains all available request parameters which can be sent
 - `Response` which implements transaction notification
 
Data from `Request` should be embedded in form which will `POST` it to `https://secure.tpay.com`.

See example of usage at [tpay example](https://github.com/lambdaterms/purescript-tpay-example).


## Testing

  ```shell
  $ pulp test --main 'Unit.Main'
  ```
  or

  ```shell
  $ pulp test --main 'Integration.Main'
  ```
