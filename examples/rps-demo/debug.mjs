/** Various functions used while debugging */
import * as common_utils from "./common-utils.mjs";
import * as web3_prelude from "./web3-prelude.mjs";
import * as local_storage from "./local-storage.mjs";
import * as common_runtime from "./common-runtime.mjs";

const {seq, registerGlobals, kLogError} = common_utils;
const {getGame} = common_runtime;

registerGlobals({common_utils, web3_prelude, local_storage, common_runtime})


// From the txHash of the transaction whereby the factory contract created the game contract,
// extract the game creation data.
// txHash => {contract: address, player0: address, player1: address, timeoutInBlocks: integer,
// commitment: bytes32, wagerInWei: BN, escrowInWei: BN, blockNumber: integer}
const getGameCreationData = txHash => (k, kError = kLogError) =>
    errbacK(web3.eth.getTransactionReceipt)(txHash)(
        receipt => {
            const game = decodeGameCreationData(receipt.logs[0].data, receipt.blockNumber, txHash);
            if(receipt.transactionHash === txHash
               && receipt.status === "0x1"
               && receipt.from === game.player0
               && receipt.to === config.contract.address
               && receipt.logs.length === 1) {
                return k(game);
            } else {
                return kError("bad rps game creation data receipt", txHash, receipt, game);
            }},
        kError);

/** Test games */
var gsalt = "0x30f6cb71704ee3321c0bb552120a492ab2406098f5a89b0a765155f4f5dd9124";
var alice = "0x60B8193167c5355779B4d03cA37A0C11807e667f";
var bob = "0xa8c6677434CDb243E9f19Cca387a916487da8C2f";

var play0 = () => srf(player0StartGame(alice, 12, makeCommitment(gsalt, 2), meth(1000), meth(1100)));

var g0tx = "0xbc75c759901430f54694ff2ae4888e9b772c15f6364320ccbcab5cfbc9f870c3";
var g0v0 = () => srf(getGameCreationData(g0tx));
var g0c = "0xd933e31efb452bfcc6f993e578946aafd6aa75d0"
var g0p1 = () => srf(player1ShowHand(g0c, meth(1000), 0));
var g0p2 = () => srf(player1WinByDefault(g0c));
var g0s = () => srf(queryConfirmedState(g1c));

var g1c = "0x21a63393301aa265ea7fbde8a7d43ea96a40f08f";
var g1p1 = () => srf(player1ShowHand(g1c, meth(1000), 0));
var g1p2 = () => srf(player0Reveal(g1c, gsalt, 2));
var g1s = () => srf(queryConfirmedState(g1c));

var wb = () =>
    newBlockHooks["newBlock"] = (from, to) => loggingK("newBlock! from:", from, "to:", to)();
var g2c = g => g.salt && g.hand0 && makeCommitment(g.salt, g.hand0);
var i2c = seq(getGame)(g2c)

// Local Variables:
// mode: JavaScript
// End: