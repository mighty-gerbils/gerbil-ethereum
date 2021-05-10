let netg = require('../network/network.js');
let keys = require('../secret/keys.js');


function initiateWeb3(network){
    return netg.startInstantiateWeb3(network);
}

function initiateWeb3WSS(network){
    return netg.startInstantiateWeb3WSS(network);
}

async function closingMinTimeout(contracAddr, web3, address, privateKey ){
    await netg.closingMinMethod(contracAddr, web3, address, Buffer.from(privateKey, 'hex'))
}

async function deployClosingMinContract(amount, web3,  constructorArg, address, privateKey){
    //deploy("0.003", web3 , args, keys.SellerAddress, Buffer.from(keys.getSellerPrivateKey(), 'hex'));
    return await netg.deploy(amount, web3 , constructorArg, address, Buffer.from(privateKey, 'hex'));
}

async function monitor(web3, web3wss, deadline, contractAddress, address, privateKey) {   
    let startBlock = await netg.getBlockNumber(web3);
    let endBLock = startBlock + parseInt(deadline);
    await netg.iwaitBlockForEvent(web3, web3wss, startBlock, endBLock, contractAddress, address, Buffer.from(privateKey, 'hex'));
}//iwaitBlockForEvent(web3, web3wss, startBlock, endBlock, contractAddress,senderAddress, senderPrivKey)

function order() {
    //item, array is price and quantity
    let order = {
        topic: "Order",
        tesla_V: [0.1, 1],
        bugati_M: [0.1, 1],
        mukN_T_shirt: [0.0001, 10],
        totalCost : 0.201
    }

    return order;
}

function agreement(digest, deadline, buyerTxId, sellerTxId, contractAddress) {
    //item, array is price and quantity
    let agreement = {
        topic: "Agreement",
        totalCost : 0.201,
        digest: digest,
        deadline: deadline,
        contractAddress: contractAddress,
        buyerTxId: buyerTxId,
        sellerTxId: sellerTxId

    }

    return agreement;
}

function expectedOrderCost(sellerTotalCost){
    let totalCost = order().totalCost;
    if(totalCost !== sellerTotalCost) {
        throw new Error("The Order total cost and Seller's total cost should be equal" + totalCost + " " + sellerTotalCost);
    }
}

module.exports = {
    expectedOrderCost,
    agreement,
    order,
    monitor,
    deployClosingMinContract,
    closingMinTimeout,
    initiateWeb3WSS,
    initiateWeb3
}
