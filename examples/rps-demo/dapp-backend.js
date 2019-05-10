/** web3 client backend for rock-paper-scissors. */
/*
  DONE:

  * All the messaging TO the blockchain, including creating a new game.
  * Follow the factory contract and collect its events.
  * Filter the events to only consider games where the user is either player0 or player1,
    or where player1 is open (0)
  * If the user is player0, try to match with games in our local storage,
    by given details.
  * Now register the game, if not already registered.
  * Have a hook for (re)displaying a contract purely based on its localStorage state,
    and have the frontend get into that hook (will skip the game if already dismissed).
    Call the hook any time a change is detected in a game.

  TODO FOR DEMO:

  * For every registered game that isn't complete,
    have a loop that polls for changes in game confirmed state
    (or, alternatively, also look at unconfirmed changes).
    Let's look at state recomputation, or, alternatively, have the messages register events
    and watch for them.

  TODO LATER MAYBE:

  * If the user is player0 and the txHash is not be already known, do something useful
    (due to race condition with the browser crashing before localStorage was done, or use in another browser)

  * Add speculative information from unconfirmed transactions,
    with clear distinction between what's confirmed,
    and what's unconfirmed by which player.

  * Improve discoverability so users don't have to care too much about being player0 or player1.

  * On frontend, have a list of common playing partners, with short aliases.
*/
'use strict';

const rock = 0;
const paper = 1;
const scissors = 2;
const validateHand = (x) => Number.isInteger(x) && (x == 0 || x == 1 || x == 2);

// SENDING DATA TO THE BLOCKCHAIN

// The contract object, to be fulfilled from config after initialization.
let rpsFactory;

// web3.utils.soliditySha3({t: 'bytes32', value: salt}, {t: 'uint8', value: hand}); // This web3 1.0 function is NOT AVAILABLE IN METAMASK! So we do things manually.
const makeCommitment = (salt, hand) => digestHex(salt + byteToHex(hand));

// (bytes32_0xString, Uint8, address, BN, BN) => ((address) => `a) => `a
const player0StartGame =
      (salt, hand, player1Address, timeoutInBlocks, wagerInWei, escrowInWei) => (k) => {
    const commitment = makeCommitment(salt, hand);
    const totalAmount = wagerInWei.add(escrowInWei);
    return errbacK(rpsFactory.player0_start_game)
    (player1Address, timeoutInBlocks, commitment, wagerInWei, {value: totalAmount})(k);
}

// (address, BN, Uint8) => (() => `a) => `a
const player1ShowHand = (contractAddress, wagerInWei, hand) => (k) => {
    const rps = web3.eth.contract(rpsAbi).at(contractAddress);
    return errbacK(rps.player1_show_hand)(hand, {value: wagerInWei})((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

// (address, Uint8Array(32), Uint8) => (() => `a) => `a
const player0Reveal = (contractAddress, salt, hand) => (k) => {
    const rps = web3.eth.contract(rpsAbi).at(contractAddress);
    return errbacK(rps.player0_reveal)(salt, hand, {})((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

// (address) => (() => `a) => `a
const player0Rescind = (contractAddress) => (k) => {
    const rps = web3.eth.contract(rpsAbi).at(contractAddress);
    return errbacK(rps.player0_rescind)()((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

// (address) => (() => `a) => `a
const player1WinByDefault = (contractAddress) => (k) => {
    const rps = web3.eth.contract(rpsAbi).at(contractAddress);
    return errbacK(rps.player1_win_by_default().send)()((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

const decodeState = (x) => {
    let [state, outcome, timeoutInBlocks, previousBlock, player0, player1, player0Commitment, wagerInWei, escrowInWei, salt, hand0, hand1] = x;
    state = state.toNumber();
    outcome = outcome.toNumber();
    timeoutInBlocks = timeoutInBlocks.toNumber();
    previousBlock = previousBlock.toNumber();
    hand0 = hand0.toNumber();
    hand1 = hand1.toNumber();
    return {state, outcome, timeoutInBlocks, previousBlock, player0, player1, player0Commitment, wagerInWei, escrowInWei, salt, hand0, hand1};};

// (address) => {state: Uint8, outcome: Uint8, timeoutInBlocks: int, previousBlock: int, player0: address, player1: address, player0Commitment: bytes32, wagerInWei: BN, escrowInWei: BN, salt: bytes32, hand0: Uint8, hand1: Uint8} => `a) => `a
const queryState = (contractAddress, blockNumber) => (k) => {
    const rps = web3.eth.contract(rpsAbi).at(contractAddress);
    return errbacK(rps.query_state.call)({}, blockNumber)((x) => k(decodeState(x)))};

// (int => `a) => `a
// TODO: HANDLE ERRORS!
const queryConfirmedState = (contractAddress) => (k) =>
    confirmedBlockNumber((blockNumber) => queryState(contractAddress, blockNumber)(k));

const decodeGameCreationData = (data, blockNumber, txHash) => {
    const x = (i) => data.slice(2+i*64,66+i*64);
    const contract = hexToAddress(x(0));
    const player0 = hexToAddress(x(1));
    const player1 = hexToAddress(x(2));
    const timeoutInBlocks = hexToBigNumber(x(3)).toNumber();
    const commitment = hexTo0x(x(4));
    const wagerInWei = hexToBigNumber(x(5));
    const escrowInWei = hexToBigNumber(x(6));
    return {contract, player0, player1, timeoutInBlocks,
            commitment, wagerInWei, escrowInWei, blockNumber, txHash};}

// TODO: better error handling
// (txHash) => {contract: address, player0: address, player1: address, timeoutInBlocks: integer,
// commitment: bytes32, wagerInWei: BN, escrowInWei: BN, blockNumber: integer}
const getGameCreationData = (txHash) => (k) => {
    errbacK(web3.eth.getTransactionReceipt)(txHash)((receipt) => {
    const result = decodeGameCreationData(receipt.logs[0].data, receipt.blockNumber, txHash);
    if(receipt.transactionHash == txHash
       && receipt.status == "0x1"
       && receipt.from == result.player0
       && receipt.to == config.contract.address
       && receipt.logs.length == 1) {
        return k(result);
    } else {
        console.log("bad rps game creation data receipt", txHash, receipt, result)
        return k(false);
    }})};

// RECEIVING DATA FROM THE BLOCKCHAIN

let nextID;
let activeGames = [];
const gamesByTxHash = {};
const activeGamesByCommitment = {};

const zeroAddress = "0x0000000000000000000000000000000000000000";

const uint32ToHex = (u) => web3.toHex(u + 0x100000000).slice(3);
const idToString = uint32ToHex;

let renderGameHook = (x) => (k) => k();

const getGameID = () => {
    const gameID = nextID;
    nextID = nextID + 1;
    putUserStorage("nextID", nextID);
    return gameID;
}

const registerGameK = (game) => (k) => {
    const id = getGameID();
    putUserStorage(idToString(id), game);
    activeGames.push(id);
    renderGameHook(id, game)(k);
}

const processNewGameK = (event) => (k) => {
    const game = decodeGameCreationData(event.data, event.blockNumber, event.transactionHash);
    if (!(game.player0 == userAddress || game.player1 == userAddress || game.player1 == zeroAddress)) {
        return k();
    }
    let id = gamesByTxHash[event.transactionHash];
    if (id) {
        if (id.contract) { // Known game. Assume blockNumber is also known.
            // TODO: should we ensure it's active, or is activation atomic enough?
            return k();
        } else {
            // TODO: double-check that everything matches, or issue warning?
            updateUserStorage(idToString(id), {blockNumber: game.blockNumber, contract: game.contract});
            return renderGameHook(id, game)(k);
        }
    } else {
        // TODO: handle the case where we're player0 but we crashed between the time the transaction was
        // published and the time we could save the txHash to localStorage.
        // TODO: also handle when we're player0 but that was started on another browser.
        return registerGameK(game)(k);
    }
}

const watchNewGames = (k) =>
    registerConfirmedEventHook(
        "confirmedNewGames",
        config.contract.creationBlock, // TODO: only from 2 timeouts in the past(?)
        {address: config.contract.address},
        processNewGameK)(k);

const processActiveGame = (id) => (k) => {
    const idString = idToString(id);
    const game = getUserStorage(idString);
    if (!game.contract) {
        return k();
    }
    return k(); // TODO     queryConfirmedState(game.contract)(XXX
}

const processActiveGames = (k) => forEachK(processActiveGame)(activeGames)(k);

const watchActiveGames = (k) => {
    newBlockHooks["confirmedActiveGames"] = processActiveGames;
    return processActiveGames(k);
}

const initBackend = (k) => {
    if (config && config.contract) { // Avoid erroring on an unconfigured network
        rpsFactory = web3.eth.contract(rpsFactoryAbi).at(config.contract.address);
        nextID = getUserStorage("nextID", 0);
        for (let id = 0; id < nextID; id++) {
            let game = getUserStorage(idToString(id));
            if (game) {
                if (!game.isComplete) {
                    activeGames.push(id);
                }
                if (game.txHash) {
                    gamesByTxHash[game.txHash] = id;
                }
            }
        }
    }
    return watchNewGames(() => watchActiveGames(k));
}

registerInit(initBackend);

/** Test games */
var gsalt = "0x30f6cb71704ee3321c0bb552120a492ab2406098f5a89b0a765155f4f5dd9124";
var alice = "0x60B8193167c5355779B4d03cA37A0C11807e667f";
var bob = "0xa8c6677434CDb243E9f19Cca387a916487da8C2f";
var meth = web3.toBigNumber(1e15).mul;

var play0 = () => srf(player0StartGame(gsalt, 2, alice, meth(1000), meth(100)));
var c0 = () => srf(computeCommitment(gsalt, 2));

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

var wb = () => {
    newBlockHooks["newBlock"] = (from, to) => (k) => {
        console.log("newBlock! from:", from, "to:", to); return k(); };
    return watchBlockchain(); }
