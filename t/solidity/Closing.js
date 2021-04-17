// Succint version of Buyer and Seller clients assuming suitable primitives from our runtime

if (typeof web3 !== 'undefined') {
    web3 = new Web3(web3.currentProvider);
} else {
    // set the provider you want from Web3.providers
    web3 = new Web3(new Web3.providers.HttpProvider("http://localhost:8545"));
}

// Read the output of `solc --combined-json abi,bin Closing.sol > Closing.json`
const Closing_json = require("Closing.json");
const Closing = Closing_json.contracts["Closing.sol:Closing"];


web3.eth.defaultAccount = web3.eth.accounts[0];

// TODO: a catch block must surround everything after contractInstance creation,
// to ensure the timeout function is called if anything goes wrong.
// TODO: instead of a notification with notify_user, we could, .encodeABI() the .deploy result
// then pass it to the CREATE2 wrapper, that the other will watch. But CREATE2 is expensive
// these days on ETH, thus we must somehow warn the other party...
async function Closing__Buyer (timeoutInBlocks, Buyer, Seller, digest, price) {
    const contract = new web3.eth.Contract(Closing.abi);
    let txHash;
    const contractInstance = await contract
        .deploy({data: Closing.bin, arguments: [timeoutInBlocks, Buyer, Seller, digest, price]})
        .send({from: Buyer}, (err, transactionHash) => {txHash = transactionHash;})
        .on("confirmation");
    const receipt = await web3.eth.getTransactionReceipt(txHash);
    await notify_other_user(Seller, ["Closing__Buyer", 1, receipt,
                                     [timeoutInBlocks, Buyer, Seller, digest, price]]);
    const address = receipt.contractAddress;
    const deadline = receipt.blockNumber + timeoutInBlocks;
    const event = await contractInstance.once("logs", {toBlock: deadline});
    const rv = event.returnValues;
    assert (check_signature(Seller, digest, rv.v, rv.r, rv.s));
    return rv;
}

// Expanded version using only web3
async function Closing__Seller (timeoutInBlocks, Buyer, Seller, digest, price) {
  const interaction = await expect_interaction(
    {contract: ClosingMin_contract,
     creator: Buyer,
     parameters: [Buyer, Seller, digest],
     value: price,
     timeoutInBlocks});
  const (v, r, s) = signMessage(Seller, digest); // requires low-level access to private key
  interaction.contract.sign(v, r, s);
}
