// Succint version of Buyer and Seller clients assuming suitable primitives from our runtime

async function Closing__Buyer (timeoutInBlocks, Buyer, Seller, digest, price) {
  const interaction = await create_interaction(
    {contract: ClosingMin_json,
     notify: Seller,
     parameters: [Buyer, Seller, digest],
     value: price,
     timeoutInBlocks});
  const event = await interaction.recv(onTimeout="timeout");
  const (v, r, s) = decode_event(event, "SignaturePublished(uint8,bytes32,bytes32)");
  assert (check_signature(Seller, digest, v, r, s));
  await interaction.close();
  return {v, r, s};
}

// Expanded version using only web3
async function Closing__Seller (timeoutInBlocks, Buyer, Seller, digest, price) {
  const interaction = await expect_interaction(
    {contract: ClosingMin_contract,
     creator: Buyer,
     parameters: [Buyer, Seller, digest],
     value: price,
     timeoutInBlocks});
  const (v, r, s) = signMessage(Seller, digest);
  interaction.contract.sign(v, r, s);
}
