pragma solidity ^0.8.2; // SPDX-License-Identifier: Apache2.0
contract ClosingBug { // Can you identify the bugs in this contract?
  address payable Buyer;  address payable Seller;
  bytes32 digest;         uint price;
  constructor(address payable _Buyer, address payable _Seller,
              bytes32 _digest, uint _price) payable {
    Buyer = _Buyer;       Seller = _Seller;
    digest = _digest;     price = _price;
    require(msg.value == price);
  }
  event SignaturePublished(uint8 v, bytes32 r, bytes32 s);
  function sign(uint8 v, bytes32 r, bytes32 s) public payable {
    require(Seller == ecrecover(digest, v, r, s));
    emit SignaturePublished(v, r, s);
    selfdestruct(payable(msg.sender));
  }
}
/* This contract is meant to implement a closing:
   The Buyer will deposit the agreed-upon price as he registers the contract
   on the blockchain, which implicitly marks his agreement to the terms;
   the amount is then released to the Seller upon signature by the Seller.

   What bug(s) can you find in the above 17-line contract, if any?
   ROT13'ed
   - Vs gur Fryyre arire fvtaf, gurer vf ab gvzrbhg zrpunavfz sbe gur Ohlre
     gb trg uvf gbxraf onpx.
   - Vs gur Fryyre choyvfurf uvf fvtangher va n genafnpgvba, nal zvare pna sebag-eha
     uvz naq fpbbc gur cnlzrag ol fhofgvghgvat gurve nqqerff nf zft.fraqre.
*/
