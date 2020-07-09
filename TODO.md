TODO for Gerbil-Ethereum
========================

* Tweak the persistence model to make sure that Ethereum (pre)transactions
  can be scheduled in a single DB transaction together with according changes
  in whatever larger DApp needs the Ethereum transaction.

* Add batch contract call and shared-user capability to the batching contract.
  We only need to check the sender if the balance of the call is negative.

* Add support for adversarial posting as an auction, based on deadlines, a gas model,
  a cost/benefit model for losing the auction, etc.

* Better flesh out the chain-watching capabilities.

* Support multiple Ethernet-compatible networks in the same client and/or DB.

* Support Geth and/or Parity extensions to the standard Ethereum JSON RPC interface.

* Support the websocket.

* Add more tests for every bit of functionality.

* Add adversarial tests for resuming computation when transactions are
  interrupted at "wrong" points.

* Add stress testing for interrupting computations at random points.

* Support cockroachdb or some other distributed backend, instead of leveldb
  (and upstream that support in another library and/or gerbil itself).

* Support resuming computation from a different master when one crashes.

* Support JS as a target via GambitJS.

* Add an assembly model of gas costs. Reuse the KEVM model somehow?

* Add better support for algebraic sum types to poo, and use it here.

* Fully support parsing ABI descriptors, including non-elementary types.

* Ensure we have all the features that web3.js does, and more.

* Add example applications.

