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

* Compute costs: since ISTANBUL, the marginal cost of publishing an otherwise unused 32-byte word
  costs 16/byte (minus 12/256 for the occasional 0, so about 15.95) plus 4 for the memory,
  so 510.5 for the data, plus 3 for copying to memory, plus 6 for hashing, so 523.5 total.
  That's now less than the 800 to read from storage (used to be 68/byte vs 200/word).
  And then, if we pack it as 20-bytes, it's even better (we save 12 zeros, or 48 gas,
  plus 12/32 times 13, so 4.875, but lose 6gas per load, plus 21 per store, only 6 if can padafter).
  For deep merkleized data, it's still better to store one word than reveal a tens of nodes,
  one for each level of the tree--but unless the data is short, it might be better to merkleize
  the entry for the node.
