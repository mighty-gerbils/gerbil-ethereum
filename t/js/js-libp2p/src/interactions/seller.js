
let netg = require('../network/network.js');
let keys = require('../secret/keys.js');

let globalStoreOrder = {};
let globalStoreOffer = {};

let globalStoreVRS = {};


function storeOrder(items, buyerTxId) {
   globalStoreOrder[buyerTxId] = items;
}

function storeOffer(offer, buyerTxId) {
   globalStoreOffer[buyerTxId] = offer;
}

function storeVRSH(vrsh, buyerTxId) {
   globalStoreVRS[buyerTxId] = vrsh;
}

function getOrder(buyerTxId) {
   return globalStoreOrder[buyerTxId];
}

function getOffer(buyerTxId) {
   return globalStoreOffer[buyerTxId];
}

function getVRSH(buyerTxId) {
   return globalStoreVRS[buyerTxId];
}

function initiateWeb3(network){
    return netg.startInstantiateWeb3(network);
}

async function closingMinGetPaid(buyerTxId, contracAddr, web3, address, privateKey){
    let vrs = getVRSH(buyerTxId);
    netg.closingMinMethod(contracAddr, web3, address, Buffer.from(privateKey, 'hex'), vrs)
}

function digestAndSign(msg, privateKey) {
    return netg.hashAndsign(msg, Buffer.from(privateKey, 'hex'));
}

async function expectedContractBalance(web3, buyerTxId, contractAddress){
    let totalPrice = getOffer(buyerTxId).totalCost;
    if(!(await netg.isBalanced(web3, totalPrice.toString(), contractAddress))) {
        throw new Error('Contract amount should be equal to '+ totalPrice );
    }
}

function offer(items, address, privKey) {
    let buyerTxId = keys.getUUID();
    let SellerTxId = keys.getUUID();
    let total = 0;
    let count = 0;

    storeOrder(items, buyerTxId);
    

    for(const item in items) {
        if(["topic", "totalCost"].includes(item)) continue;
        total = total + (parseFloat(items[item][0]) * parseFloat(items[item][1]));
    }

    if(items.totalCost !== total){
        throw new Error('Contract amount should be equal to '+ total);
    }

    let msg = `${buyerTxId},you are ordering ${count} with totalcost of ${total} at ${new Date()}`;
    let vrsh = digestAndSign(msg, privKey);
    storeVRSH(vrsh, buyerTxId);

    let offerRtn = {
       topic: "Offer",
       buyerTxId:  buyerTxId,
       sellerTxId:  SellerTxId,
       totalCost: total,
       items: items,
       deadline: 20,
       sellerAddress: address,
       digest : vrsh["hash"]

    }

    storeOffer(offerRtn, buyerTxId);
    return offerRtn;
}

  module.exports = {
    offer,
    expectedContractBalance,
    digestAndSign,
    closingMinGetPaid,
    initiateWeb3
  }
