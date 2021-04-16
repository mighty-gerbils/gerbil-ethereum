# Programming model for shared contract with many interactions

TODO:
* Move this to some kind of Racket Scribble documentation.
* A good design does as much as possible at the *Glow* level,
  and only leaves low-level bits to gerbil-ethereum.

## State Model

### State Scope

A shared-contract state is made of many individual interactions,
each storing data in the persistent of the shared contract.

- In the EVM model, each interaction will have its own state `SSTORE`d
  at consecutive addresses in the shared contract's persistent state
  as determined by its 32-byte interaction id.

- In the EUTXO model of Cardano, each interaction will have its own
  EUTXO holding a recognizable NFT (which embodies the 32-byte interaction id)
  to hold its state.

In addition to interaction-scoped state, some state in contract-scoped:
Some contract-global interactions will contain contract-global state
that gets updated at every relevant transaction,
e.g. total number of active token in each of several categories, etc.

Regardless of scope, the holding of a state associated to an interaction
is a bit like a term in the [Join Calculus](https://en.wikipedia.org/wiki/Join-calculus):
there are rewrite rules based on its being present, that will
linearly construct a new term associated to the same interaction.

### Synchronous vs Asynchronous State

Every interaction may hold two kinds kinds of state: synchronous and asynchronous.

Synchronous state is like the regular state of a program, as manipulated
by the interaction's internal code, its server thread/process. It is
modified by the transactions that are part of a sequential program.
This state can be merkleized, and thus large, but may only be modified
synchronously by the participants of the interaction, who will reveal any
merkleized state. At any point, there should be only a small number of people
who can interact with the synchronous state, each for a small number of steps max;
otherwise it's a race condition, which is bad.
Synchronous state typically takes a relatively small fixed size in persistent storage,
whereby any large or variable-size parts are merkleized.

Asynchronous state is like the state modified by an interrupt handler
(or signal handler in Unix parlance, async event handler in Windows):
it may be modified asynchronously by other external interactions,
according to some limited and well-identified set of operations.
This asynchronous state might typically include balances for assets
that can be directly deposited.
This state cannot be merkleized, but it may be variable size and grow,
though it is also expensive so limited in size in practice
(which is also good for security reasons, otherwise, you could grow until
you overwrite other state).
Regular programs can access the asynchronous state as part of a transaction,
which is akin to accessing that state with interrupts turned off or deferred
in our analogy.

Or maybe asynchronous and synchronous can "simply" be for distinct
(but related?) interaction. If using consecutive addresses, be careful of sizes
to avoid clashes.

### Interaction Id

Every interaction has a 32-byte interaction Id.

#### Ids must include enough of the initial state prevent interaction pre-hijacking

We cannot allow non-authorized third parties to modify the synchronous state of an interaction,
or to "guess" what a future interaction id will be and hijack it.

If participants are to be able to discuss an interaction before it was registered on the blockchain,
then all of them must be verified when creating the interaction and/or enough state should be included
(or hashed) into the id to prevent making the id with a different initial state than agreed upon.

If a set of participants may agree to multiple copies of a "same" interaction,
the ID should include the details that will differ and/or some mutually random salt.

#### Ids must include asynchronous state schema

It ought to be very cheap to determine the data schema of the asynchronous state
and the associated set of authorized asynchronous operations based on the interaction-id alone.

- The simplest method, assuming there is a small number of different asynchronous state schemas
  in the given shared contract is to use the first (or last) byte in the interaction-id as a tag.
  The 31 remaining bytes can be used directly for the identifying interaction state
  (e.g. Ethereum address) or a truncated hash of the interaction parameters.

- Another method, more appropriate if there are a lot of different asynchronous state schemas,
  is to include some description of that state at the beginning of the parameters that are
  hashed into the interaction-id, with or without a byte mark as per the above method.

#### Example: Ids for an ERC20 equivalent.

All state is asynchronous, and the last byte identifies the asynchronous state schema.

- The global state would be under ID 0.

- State associated to a particular user's balance would be at id
  `(bytes-append (make-bytes 11) user-address #u8(1))`

- State associated to a user's allowance for another would be at
  `(bytes-replace (digest (Tuple Address Address) (vector from to)) 31 2)`


The global interaction state schema (result of a global transformation of the *Glow* code)
would be something like that (this looks a bit like a GADT, but isn't quite;
it's more like annotating the parts of a record with "id", "synchronous", "asynchronous" tags).
```
data InteractionState =
  | Global -> { totalSupply: TokenAmount }
  | Account(owner: Address) -> { balance: TokenAmount }
  | Approval(from: Address, to: Address) -> { allowance: TokenAmount }
```

#### Example: Ids for an ERC1155 equivalent.

All state is asynchronous, and the last byte identifies the asynchronous state schema.

- The global state would be under ID 0.

- State associated to a particular subtoken would be at id
  `(bytes-append padding subtoken-address #u8(1))`
  --- assuming the subtoken address is less than 31-byte wide, --- or
  `(bytes-replace (digest (Tuple Address Address) (vector from to)) 31 1)`
  if the subtoken-address is 32-byte wide (maybe itself a digest of some larger state).

- State associated to a user owning some token would be:
  `(bytes-replace (digest (Tuple Address TokenId) (vector owner token-id)) 31 2)`

- State associated to a user allowing a transfer to another user would be:
  `(bytes-replace (digest (Tuple Address Address TokenId) (vector sender recipient token-id)) 31 3)`
