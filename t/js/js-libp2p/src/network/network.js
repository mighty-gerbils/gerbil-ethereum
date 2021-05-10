const Web3 = require('web3');
let source = require("../contracts/contracts.json");
const keys = require('../secret/keys');
const Transaction = require('ethereumjs-tx');
const ethUtils = require("ethereumjs-util");

let contracts = source["contracts"];
const bytecode = contracts["ClosingMin.sol:ClosingMin"].bin;
let abi = JSON.parse(contracts["ClosingMin.sol:ClosingMin"].abi);

/*
   -- Define Provider & Variables --
*/
// Provider
function startInstantiateWeb3(infuraProjectID) {
    var web3 = new Web3(new Web3.providers.HttpProvider(`https://ropsten.infura.io/v3/${infuraProjectID}`));
    return web3;
}

function startInstantiateWeb3WSS(infuraProjectID) {
    var web3 = new Web3(new Web3.providers.WebsocketProvider(`wss://ropsten.infura.io/ws/v3/${infuraProjectID}`));
    return web3;
}

function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
}
 
function sign(msgHash, privKey) {
    if (typeof msgHash === "string" && msgHash.slice(0, 2) === "0x") {
        msgHash = Buffer.alloc(32, msgHash.slice(2), "hex");
    }

    const sig = ethUtils.ecsign(msgHash, privKey);
    return  {
        r: sig.r.toString("hex"),
        s: sig.s.toString("hex"),
        v: sig.v
    }        
}

function hash(msg) {
    if (typeof msg === "string") {
        return ethUtils.keccak(msg);
    }
}

function hashAndsign(msg, privKey){
    let hashed = hash(msg);
    let vrsh = sign(hashed, privKey);

    vrsh["hash"] = hashed.toString('hex');
    return vrsh;
}

async function closingMinMethod(contractAddress, web3, senderAddress, senderPrivKey, vrs={}) {
    let contract = new web3.eth.Contract(abi, contractAddress);
    let data;
    if(vrs.r === undefined) {
        data = contract.methods.timeout().encodeABI();
    } else {
        data = contract.methods.sign(vrs.v, '0x'+vrs.r, '0x'+vrs.s).encodeABI();
    }
   
    let cnt = await web3.eth.getTransactionCount(senderAddress);
    let rawTx = {
        nonce: web3.utils.toHex(cnt),
        data: data,
        gasLimit: web3.utils.toHex(800000),
        gasPrice:  web3.utils.toHex(web3.utils.toWei('20', 'gwei')),
        to: contractAddress,
        from: senderAddress
    };

    const tx = new Transaction(rawTx);
    tx.sign(Buffer.from(senderPrivKey, 'hex'));
    let serializedTx = '0x' + tx.serialize().toString('hex');
    await web3.eth.sendSignedTransaction(serializedTx);
}

async function getBlockNumber(web3){
    return await web3.eth.getBlockNumber();
}

async function iwaitBlockForEvent(web3, web3wss, startBlock, endBlock, contractAddress, senderAddress, senderPrivKey) {
    let contract = new web3wss.eth.Contract(abi, contractAddress);
    while (true) {
        let block = await web3.eth.getBlockNumber();
        if(block >= endBlock) {
            //call timeout
            await closingMinMethod(contractAddress, web3, senderAddress, senderPrivKey)
            break;
        }
        let event = await contract.getPastEvents('SignaturePublished', {fromBlock: startBlock, toBlock: 'latest'});
        if(event.length === 0) {
            await sleep(4000);
            startBlock = block;
            continue;
        } else {
            //confirm event
            console.log(event);
            break;
        }
    }
}

async function isBalanced(web3, expectedBalance, contractAddress) {
    let wei = await web3.eth.getBalance(contractAddress);
    return web3.utils.toWei(expectedBalance, 'ether') === wei;
} 

 /*s
   -- Deploy Contract --
*/
const deploy = async (value, web3 ,initilizationArgument, creator, privateKey) => {
    console.log(`Attempting to deploy from account ${creator}`);

    // Create Contract Instance [5, keys.BuyerAddress, keys.SellerAddress, "0x"+ethUtils.keccak("kool").toString('hex')]
    const closingMin = new web3.eth.Contract(abi);


    // Create Constructor Tx
    const closingMinTx = closingMin.deploy({
        data: bytecode,
        arguments: initilizationArgument
    });

    let rawTx = {
        nonce: web3.utils.toHex(await web3.eth.getTransactionCount(creator)),
        data: closingMinTx.encodeABI(),
        gas: (await closingMinTx.estimateGas()),
        gasPrice:  web3.utils.toHex(web3.utils.toWei('20', 'gwei')),
        chainId: '0x03',
        value: web3.utils.toHex(web3.utils.toWei(value, 'ether'))
    };

    // Sign Transacation and Send
    const tx = new Transaction(rawTx);
    tx.sign(privateKey);
    var serializedTx = '0x' + tx.serialize().toString('hex');


    // Send Tx and Wait for Receipt
    const createReceipt = await web3.eth.sendSignedTransaction(serializedTx);
   
    console.log(
        `Contract deployed at address: ${createReceipt.contractAddress}`
    );
    return createReceipt.contractAddress;
};


module.exports = {
   deploy,
   isBalanced,
   iwaitBlockForEvent,
   getBlockNumber,
   closingMinMethod,
   hashAndsign,
   startInstantiateWeb3WSS,
   startInstantiateWeb3
}