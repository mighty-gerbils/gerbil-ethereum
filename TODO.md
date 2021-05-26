TODO for Gerbil-Ethereum
========================

* Do our own accounting of in-flight transactions.

* Do proper re-noncing of pre-transactions (already partly done by tx-tracker).

* Be able to observe if there are any in-flight transactions
  from other owners of the same key (issue alert if that is detected!)

* Have a persistent alert system to warn users about alerting situations.

* Have a persistent scheduling system to take automated actions based on alerts.

* Tweak the persistence model to make sure that Ethereum (pre)transactions
  can be scheduled in a single DB transaction together with according changes
  in whatever larger DApp needs the Ethereum transaction.

* Add a universal CREATE2 master contract that ultimately allows for better MAST.

* Add shared-user capability to the batching contract?
  We only need to check the sender if the balance of the call is negative.

* Add support for adversarial posting as an auction, based on deadlines, a gas model,
  a cost/benefit model for losing the auction, etc. Front-running as a service!

* Better flesh out the chain-watching capabilities to work well with persistence.

* Support multiple Ethernet-compatible networks in the same client and/or DB.

* Support Geth and/or Parity extensions to the standard Ethereum JSON RPC interface.

* Support the websocket.

* Add more tests for every bit of functionality.

* Add adversarial tests for resuming computation when transactions are
  interrupted at "wrong" points.

* Add stress testing for interrupting computations at random points.

* Support postgresql, cockroachdb or some other distributed backend, instead of leveldb
  (and upstream that support in gerbil, gerbil-persist and/or suitable libraries).

* Support resuming computation from a different master when one crashes.

* Support JS as a target via GambitJS.

* Add an assembly model of gas costs that computes a reliable yet tight upper-bound
  on cost (based on underestimating the zeroes in memory and arithmetic operations
  that depend on data size, e.g. EXP). Reuse the KEVM model somehow?

* Add better support for algebraic sum types to poo, and use it here.

* Fully support parsing ABI descriptors, including non-elementary types.

* Ensure we have all the features that web3.js does, and more.

* Add example applications.

* Base data representation on actually computed costs:
  since ISTANBUL (EIP-2028), the marginal cost of publishing a 32-byte word is 16/byte
  (in average minus 12/256 for the occasional 0, so about 15.95),
  plus 4 for the memory to copy it to (necessary to do anything with it),
  so 510.5 for the data, plus 3 for copying to memory, plus 6 for hashing, so 523.5 total.
  That's now less than the 800 to read from storage (used to be 68/byte vs 200/word).
  And then, if we trim digests like addresses at 20-bytes, it's even better
  (we save 12 zeros, or 48 gas, plus 12/32 times 13, so 4.875, but lose 6gas per load, plus 21 per store,
  though only 6 per store if we can overwrite-after).
  For deep merkleized data, it's still better to store one word than reveal tens of nodes,
  one for each level of the tree--but unless the data is short, it might be better to merkleize
  the entry for the node.

* Reenable or completely remove the nonce-tracker.
  On geth, the node gives us enough information so we can just let its feedback
  drive our model of what nonce we should be using; but on Mantis,
  we must be more conservative in our behavior, and if our same codebase must work everywhere,
  then we better just do away with the nonsense for now.
  But we may want something better back some day.

* Function to compute GAS for a tx and send all at address
