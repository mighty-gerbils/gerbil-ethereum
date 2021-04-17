pragma solidity ^0.8.2; // SPDX-License-Identifier: Apache2.0

// This contract is a stubborn manual compilation of the closing contract,
// with no optimizations, no cleverness, no global reasoning.
// Numbers in parentheses refer to footnotes regarding possible optimizations.

contract Closing {
        // Global variables for every contract
        uint state; // At which point of the interaction are we at? (1)
        uint deadline; // At which block will the next participant timeout?
        uint timeoutInBlocks; // timeout in blocks

        // Interaction parameters for the contract: participants
        address payable Buyer;
        address payable Seller;

        // Regular DApp parameters
        bytes32 digest;
        uint price; // (2)

        constructor(uint _timeoutInBlocks,
                    address payable _Buyer,
                    address payable _Seller,
                    bytes32 _digest, // (2)
                    uint _price) payable {
                timeoutInBlocks = _timeoutInBlocks;
                Buyer = _Buyer;
                Seller = _Seller;
                price = _price; // (2)
                digest = _digest;
                require(msg.sender == Buyer); // (3)
                require(msg.value == price); // (2)
                state = 1; // (1)
                deadline = block.number + timeoutInBlocks;
        }

        event SignaturePublished(uint8 v, bytes32 r, bytes32 s); // (4)

        function sign(uint8 v, bytes32 r, bytes32 s) public payable {
                require(state == 1); // (1)
                require(msg.sender == Seller); // (5)
                require(block.number <= deadline); // (6)
                require(ecrecover(digest, v, r, s) == Seller); // (7)
                emit SignaturePublished(v, r, s); // (4)
                Seller.transfer(price); // (8)
                state = 0; // (1, 9)
                Buyer = payable(0); Seller = payable(0); // (9)
                digest = 0; price = 0; deadline = 0; timeoutInBlocks = 0; // (9)
                selfdestruct(Buyer); // (8)
        }

        function timeout() public payable {
                require(state == 1); // (1)
                require(msg.sender == Buyer); // (5)
                require(block.number > deadline);
                state = 0; // (1, 9)
                Buyer = payable(0); Seller = payable(0); // (9)
                digest = 0; price = 0; deadline = 0; timeoutInBlocks = 0; // (9)
                selfdestruct(Buyer);
        }
}
/*
  Notes for this 47-line correct contract (excluding comments and blanks):

  1. We don't need to store in a state variable at which point of the
    interaction's finite state machine the contract is, since there's only
    one single valid state after construction and before destruction.

  2. We don't need to require(msg.value == price) because the Seller can
    check that off-chain and decline to cooperate if the amount is wrong.
    Thus we can save GAS for a task that is useless to do on-chain, by
    not checking the price; since it isn't used later either, we don't need
    to store it; and since we never use it, we don't even need to pass it
    to the contract to begin with.

  3. Similarly we don't need to require(msg.sender == Buyer) since
    only the Buyer is interested in paying anyway, whereas
    the Seller doesn't care what account the money comes from.

  4. We don't strictly need this event, or any event for that matter:
    a client could carefully evaluate every transaction in every block
    (or just the block in which contract selfdestructed), until it finds
    the one that published (directly or indirectly) the signature.
    VulcanizeDB and its competitors should even make that easy.
    But until they are widespread and cheap, it is technically easier
    for the Buyer to extract the signature via an event.

  5. We don't need to require(msg.sender == Seller) in sign() or to
    require(msg.sender == Buyer) in timeout() because only the respective
    user is interested in being paid, and would actually welcome someone else
    front-running their transaction and paying the GAS for their benefit.
    That would be different if the party posting the transaction could
    actually inject data into the interaction.

  6. We also actually need to check the deadline, because the Seller's interest
    will already prevent him from signing after the deadline (or even just too
    close to it), by fear that the Buyer, having seen the message with the
    signature, would then race and/or outbid his message with a rival timeout
    message, which, if successful, would get him the signature for free.

  7. This is the signature check. If it isn't done and done properly, then then
    Seller (or maybe even a third party) can get the payment without signing.

  8. Considering that there's only one transfer, then a selfdestruct with no
    tokens expected to be left in the contract (we still want to selfdestruct
    for the GAS refund), we can merge the last transfer into the selfdestruct.

  9. We reset all the persistent variables to get an according GAS refund.
    The refund is actually larger than the GAS cost for the transaction,
    so it will be wasted... unless we batch it with other transactions that
    can use the refund... but that supposes that whichever recipient,
    Buyer or Seller, is itself a proxy contract capable of batching.
 */
