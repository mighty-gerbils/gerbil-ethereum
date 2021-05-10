'use strict'

const path = require('path')
const execa = require('execa')
const pDefer = require('p-defer')
const uint8ArrayToString = require('uint8arrays/to-string')
const sellerInteractions = require('../interactions/seller.js');
const buyerInteractions  = require('../interactions/buyer.js');
const keys = require('../secret/keys.js');

function startProcess(name) {
  return execa('node', [path.join(__dirname, name)], {
    cwd: path.resolve(__dirname),
    all: true
  })
}

function getArgument(parsed) {
  let rtn = [];
  if(!parsed.deadline) {
    throw new Error("Expected Deadline");
  } else {
    rtn.push(parseInt(parsed.deadline));
  }

  rtn.push(keys.BuyerAddress);

  if(!parsed.sellerAddress) {
    throw new Error("Expected Seller's address");
  } else {
    rtn.push(parsed.sellerAddress);
  }

  
  if(!parsed.digest) {
    throw new Error("Expected Digest");
  } else {
    rtn.push("0x"+parsed.digest);
  }

  return rtn;
}

async function closing () {
  const message = 'Start messaging'
  let listenerOutput = ''
  let dialerOutput = ''

  let isListening = false
  let messageSent = false
  const listenerReady = pDefer()
  const dialerReady = pDefer()
  const messageReceived = pDefer()

  // Step 1 process [Seller]
  process.stdout.write('node listener.js\n')
  const listenerProc = startProcess('listener.js')
  listenerProc.all.on('data', async (data) => {
    listenerOutput = uint8ArrayToString(data);
    process.stdout.write(data);

    listenerOutput = uint8ArrayToString(data);
 //   let parsed = JSON.parse(listenerOutput)

    if (!isListening && listenerOutput.includes('Listener ready, listening on')) {
      listenerReady.resolve()
      isListening = true;
    } else if(isListening && listenerOutput.includes('{')) {
      try {
        let parsed = JSON.parse(listenerOutput.substr(1));
        switch(parsed["topic"]){
          case "Order":
            let offerObj = sellerInteractions.offer(parsed, keys.SellerAddress, keys.getSellerPrivateKey());
            let myJSON = JSON.stringify(offerObj)
            listenerProc.stdin.write(myJSON); 
            break;
          case "Agreement":
            let contractAddress = parsed.contractAddress;
            let buyerTxId = parsed.buyerTxId;
            let web3 = sellerInteractions.initiateWeb3(keys.infuraProjectID);
            await sellerInteractions.expectedContractBalance(web3, buyerTxId, contractAddress);
            
            await sellerInteractions.closingMinGetPaid(buyerTxId, contractAddress, web3, keys.SellerAddress, keys.getSellerPrivateKey())
            console.log("Signing progress.....")
            messageReceived.resolve();
        }
      } catch(err) {
        console.log("Seller  " + err)
      }
      //listenerReady.resolve();
      //isListening = false;
    }
  })

  await listenerReady.promise
  process.stdout.write('==================================================================\n')

  // Step 2 process  [Buyer]
  process.stdout.write('node dialer.js\n')
  const dialerProc = startProcess('dialer.js')
  dialerProc.all.on('data', async (data) => {
    dialerOutput = uint8ArrayToString(data)
    process.stdout.write(data)
    

    if (!messageSent && dialerOutput.includes('Type a message and see what happens')) {
      //dialerReady.resolve()
      var obj = buyerInteractions.order();
      var myJSON = JSON.stringify(obj);
      dialerProc.stdin.write(myJSON)
      //dialerProc.stdin.write('\n')
      messageSent = false;
    }

    if (!messageSent && dialerOutput.includes('{')) {
      try {
        let parsed = JSON.parse(dialerOutput.substr(1));
        switch(parsed["topic"]){
          case "Offer":
            let sellerTotalCost = parsed.totalCost;
            buyerInteractions.expectedOrderCost(sellerTotalCost);
            let web3 = buyerInteractions.initiateWeb3(keys.infuraProjectID);
            let constructorArg = getArgument(parsed);
            let contractAddress = await buyerInteractions.deployClosingMinContract(sellerTotalCost.toString(), web3,  constructorArg, keys.BuyerAddress, keys.getBuyerPrivateKey());
            let [deadline, , , digest] = constructorArg;
            let buyerTxId = parsed.buyerTxId;
            let sellerTxId = parsed.sellerTxId;
            let agreementObj = buyerInteractions.agreement(digest, deadline, buyerTxId, sellerTxId, contractAddress)
            var myJSON = JSON.stringify(agreementObj);
            dialerProc.stdin.write(myJSON);
            let web3WSS = buyerInteractions.initiateWeb3WSS(keys.infuraProjectID);
            
             
            buyerInteractions.monitor(web3, web3WSS, deadline, contractAddress, keys.BuyerAddress, keys.getBuyerPrivateKey());
            messageReceived.resolve();
            break;
          default:
            messageReceived.resolve();
        }
      } catch(err) {
        console.log("Buyer  " + err)
        messageReceived.resolve();
      }
      dialerReady.resolve()
      messageSent = true;
    }

  })

  await dialerReady.promise
  process.stdout.write('==================================================================\n')
  await messageReceived.promise
  process.stdout.write('chat message received\n')
 //return
  listenerProc.kill()
  dialerProc.kill()
  await Promise.all([
    listenerProc,
    dialerProc
  ]).catch((err) => {
    if (err.signal !== 'SIGTERM') {
      throw err
    }
  })
}


module.exports = {closing}
