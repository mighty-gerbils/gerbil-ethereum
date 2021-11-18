# Gerbil-ethereum

Gerbil-ethereum is a package to interact with the Ethereum network from [Gerbil Scheme](https://cons.io).
It is an alternative to "web3.js" and other interfaces to the Ethereum network;
it is not at all a reimplementation of the Ethereum protocol,
and relies on an existing node such as Geth or Parity to implement the Ethereum protocol.

### Copyright and License

Copyright 2020 Mutual Knowledge Systems, Inc. All rights reserved.
Gerbil-Ethereum is distributed under the Apache License, version 2.0. See the file [LICENSE](LICENSE).

### What does Gerbil Ethereum do?

Gerbil-ethereum provides an interface to an Ethereum node's JSON RPC server, just like web3.js does,
but with Gerbil Scheme as the underlying language platform, rather than node.js or a browser.
Notable modules include but are not limited to the below:

* `tx-tracker.ss` provides an interface to ensure transactions are posted, no matter what.
  A tracked transaction will be retried until it succeeds, even if the process crashes and restarts.
  This is essential for decentralized applications such as atomic swaps or state channels,
  that require you to post a transaction before a deadline under pain of losing your assets.
  Messages are scheduled and persisted before they are issued, so that
  a temporary machine crash or network crash won't cause you to either forget to post or post twice.

* `assembly.ss` provides an assembler for the EVM as an embedded DSL.
  This assembler can be used directly to write contracts using Scheme functions as a macro-assembler,
  or it can be used indirectly as the embedded backend of a compiler.
  Either way, you can achieve more efficient and safer code
  than by using extremely badly designed languages such as Solidity.

* `simple-apps.ss` includes several simple applications written directly in EVM assembly
  to optimize for size and cost, including batching of several transactions into a single one,
  a general-purpose proxy contract that can relay the actions of an individual or group,
  a general logging contract for logging and development purposes, and
  a CREATE2 wrapper through which you can use techniques akin to Bitcoin MAST on EVM networks.

* `meta-create2.ss` includes pre-signed transactions so every EVM network can have the very same address
  for a universal CREATE2 wrapper, making Bitcoin-style MAST a reality across all EVM networks.

* `json-rpc.ss` offers you a typechecked FFI to all the usual Ethereum JSON RPC APIs.
  An API key is needed for networks using an infura RPC node. Get a key from their website
  and configure it as part of your shell's environment variable:
  `export INFURA_API_KEY=0123456789ABCDEF0123456789ABCDEF`
  Or deploy your own node and define your own alternate network.

There are plenty of other support files, and we'll keep working on this library
as we build software on top of it.


### Why use Gerbil Ethereum?

* *Better Language, Simpler Code*: Gerbil Ethereum can achieve functionality similar
  to that of libraries in other languages at a fraction of the complexity,
  because the underlying language is more *expressive*.
  First-class *continuations* and threads enable you to directly express in Scheme
  massively concurrent computations that require jumping through many hoops in other languages,
  such as async functions, monads, trampolines, thread pools, factory factories, etc.
  Scheme's unique ability for *syntactic abstraction* not only makes the code much more succint,
  but also much safer, by protecting the users from errors introduced by developers when they manually
  follow "design patterns", or fail to follow them, especially as they subsequently evolve their code.
  Using *prototype object orientation* for type descriptors,
  we can leverage both ad-hoc and parametric polymorphism to factor our code into
  small pieces of incremental functionality that nicely build on each other,
  rather than large monolithic pieces of code that constantly repeat each other.
  Thus, for instance, in the case of merkle tries, in one fifth the code size,
  we could fit twice the functionality provided by equivalent code in Go, with added optimizations.
  In the end, that means not only much less code to write, but
  even more importantly for cryptocurrency software, much less code to *audit*.

* *Ecosystem Safety*: what makes JavaScript attractive to web developers is its vast ecosystem
  of software packages, notably distributed via the `npm` package manager.
  But to enjoy the benefit of this ecosystem, you'll soon find yourself pulling
  hundreds or thousands of libraries from as many unidentified developers.
  That's millions of lines of code that no one has the hope to ever audit,
  what more in a combination that moves too fast for an audit to remain valid very long.
  This is not hypothetical: in the past few years, several cryptocurrency wallets
  have actually been attacked not just through existing bugs in such libraries,
  but also through *supply chain attacks* wherein bad actors took over
  some remote indirect dependency of a wallet so as to gain access to its users' assets.
  By contrast, Gerbil may have fewer libraries, but it includes everything required to interact
  with Ethereum, except for a few thousand lines of code that can be readily audited,
  written by a small group of well-identified reputable individuals
  who have been working on this or similar software for decades,
  plus a few small well-audited libraries that everyone trusts and use
  (such as Bitcoin's secp256k1, Ethereum's keccak, or Google's leveldb).

* *Safer Programming Model*: with Gerbil Ethereum, we are creating a programming model
  for building decentralized applications that are safe by construction.
  Our current code base does not yet offer builtin protection against all the attacks
  that we are thinking of preventing, yet the programming model it offers is already
  significantly more robust than that offered by platforms such as web3.js
  whose authors seem blissfully oblivious of these attacks.
  For instance, we take persist-before-messaging discipline seriously,
  with persistent activities, proper transactionality, and in the future distributed replication.
  We also build and test our code with [deterministic build tools](https://www.nixos.org/nix/)
  that ensure that if it works for us, it will work exactly the same for everyone.

* *Potential Portability*: the Gerbil is built on top of Gambit, that has backends
  to any platform that matters, and could be made to target any future platform that would.
  In addition to the C platform, it can target JavaScript, Java, PHP, Python, Ruby, and
  various popular microprocessor architectures. While we haven't yet taken advantage of
  this portability with Gerbil ethereum, your odds at building fully audited cryptocurrency software
  that runs on top of any of these platforms, including JavaScript, are higher
  if you use a few hundreds of lines of Gambit Scheme to retarget Gerbil Ethereum
  than if you use web3.js and start auditing millions of lines of JavaScript,
  or some similar situation with another "blub" language.
