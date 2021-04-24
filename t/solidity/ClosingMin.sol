pragma solidity ^0.8.2; // SPDX-License-Identifier: Apache2.0
contract ClosingMin { // Minimal working & readable version of the contract
  address payable Buyer; address payable Seller; bytes32 digest; uint deadline;
  constructor(uint timeoutInBlocks, // (1, 2, 3, 4)
              address payable _Seller, bytes32 _digest) payable {
    Buyer = msg.sender;      Seller = _Seller;       digest = _digest;
    deadline = block.number + timeoutInBlocks;
  }
  event SignaturePublished(uint8 v, bytes32 r, bytes32 s);
  function sign(uint8 v, bytes32 r, bytes32 s) public payable { // (5, 6, 7)
    require(ecrecover(digest, v, r, s) == Seller);
    emit SignaturePublished(v, r, s);
    address payable _Seller = Seller;
    Buyer = payable(0); Seller = payable(0); digest = 0; deadline = 0; // (8)
    selfdestruct(_Seller);
  }
  function timeout() public payable {
    require(block.number > deadline); // (5)
    address payable _Buyer = Buyer;
    Buyer = payable(0); Seller = payable(0); digest = 0; deadline = 0; // (8)
    selfdestruct(_Buyer);
  }
}
/*
  Footnotes for this 23-line correct contract.

  1. We don't need to require(msg.sender == Buyer) because only the Buyer is
    interested in paying and the Seller doesn't care where the money comes from.

  2. We don't need to require(msg.value == price) because the Seller can
    check that off-chain and decline to cooperate if the amount is wrong, thus
    saving GAS for a task that is useless to do on-chain.

  3. We don't need to store in a state variable at which point of the
    interaction's finite state machine the contract is, since there's only
    one single valid state after construction and before destruction.

  4. We don't need to store the timeoutInBlocks, since it's only used once.
    Actually, we could also precompute the deadline block and pass that,
    since we know what the block number will be with a good enough precision;
    if we make it too small, that leaves less timeout for the Seller, who
    may then decline to participate.

  5. We don't need to require(msg.sender == Seller) in sign() or to
    require(msg.sender == Buyer) in timeout() because only the respective
    user is interested in being paid, and would actually welcome someone else
    front-running their transaction and paying the GAS for their benefit.
    That would be different if the party posting the transaction could
    actually inject data into the interaction.

  6. We don't strictly need this (or any) event, but until things like
    VulcanizeDB become widespread and cheap, it is technically easier and
    cheaper for the Buyer to extract the signature via an event than to
    examine every transaction in the block that completed the contract
    to find the one that published (directly or indirectly) the signature.

  7. We also don't check the deadline, because the Seller's interest will
    already prevent him from signing after the deadline (or too close to it)
    by fear that the Buyer, having seen the message with the signature,
    manages to race it with a timeout, thus getting the signature for free.

  8. We reset the persistent memory to get according GAS refund.
    The refund is actually larger than the gas for the transaction,
    which might then be batched with other transactions to use the refund...
    but that supposes the Buyer and Seller are themselves proxy contracts
    capable of using that refund.
 */
