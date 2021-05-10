

const { v4: uuidv4 } = require('uuid');

//Seller
let SellerAddress = "0x4dC831d0Fb4251EDB4ba8A1f4857e61"; //Change to correct value
let SellerPrivatekey = "e0c8600a7f2e982a846eb8d41002dab8fad6204d67e63e963e848"; //Change to correct value


//Buyer
let BuyerAddress = "0x20106136E144e8A4D4920CD0F3"; //Change to correct value
let BuyerPrivatekey = "41b0282bedeeef2a4a3509a23ef3134769411246276da48fa1e"; //Change to correct value

const infuraProjectID = "614ccde97fa84d778b2cf1"; //Change to correct value

function getSellerPrivateKey(){
    return SellerPrivatekey
}

function getBuyerPrivateKey(){
    return BuyerPrivatekey
}


function getUUID() {
    return uuidv4();
}
  
module.exports = {
    infuraProjectID,
    BuyerAddress,
    getBuyerPrivateKey,
    SellerAddress,
    getSellerPrivateKey,
    getUUID
}
