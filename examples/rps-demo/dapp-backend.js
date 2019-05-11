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
const validateHand = x => Number.isInteger(x) && (x == 0 || x == 1 || x == 2);

// SENDING DATA TO THE BLOCKCHAIN

// The contract object, to be fulfilled from config after initialization.
let rpsFactory;

// web3.utils.soliditySha3({t: 'bytes32', value: salt}, {t: 'uint8', value: hand}); // This web3 1.0 function is NOT AVAILABLE IN METAMASK! So we do things manually.
const makeCommitment = (salt, hand) => digestHex(salt + byteToHex(hand));

// (bytes32_0xString, Uint8, address, BN, BN) => (address => `a) => `a
const player0StartGame =
      (salt, hand, player1, timeoutInBlocks, wagerInWei, escrowInWei) => k => {
    const commitment = makeCommitment(salt, hand);
    const totalAmount = wagerInWei.add(escrowInWei);
    return errbacK(rpsFactory.player0_start_game)
    (player1, timeoutInBlocks, commitment, wagerInWei, {value: totalAmount})(k);
}

const rps = (contractAddress) => web3.eth.contract(rpsAbi).at(contractAddress);

// (address, BN, Uint8) => (() => `a) => `a
const player1ShowHand = (contractAddress, wagerInWei, hand) => (k, kError = kLogError) =>
    errbacK(rps(contractAddress).player1_show_hand)(hand, {value: wagerInWei})(
        txHash => confirmEtherTransaction(txHash)(k), kError);

// (address, Uint8Array(32), Uint8) => (() => `a) => `a
const player0Reveal = (contractAddress, salt, hand) => (k, kError = kLogError) =>
    errbacK(rps(contractAddress).player0_reveal)(salt, hand, {})(
        txHash => confirmEtherTransaction(txHash)(k), kError)

// address => (() => `a) => `a
const player0Rescind = contractAddress => (k, kError = kLogError) =>
    errbacK(rps(contractAddress).player0_rescind)()(
        txHash => confirmEtherTransaction(txHash)(k), kError);

// address => (() => `a) => `a
const player1WinByDefault = contractAddress => (k, kError = kLogError) =>
    errbacK(rps(contractAddress).player1_win_by_default().send)()(
        txHash => confirmEtherTransaction(txHash)(k), kError);

const decodeState = x => {
    let [state, outcome, timeoutInBlocks, previousBlock, player0, player1, player0Commitment, wagerInWei, escrowInWei, salt, hand0, hand1] = x;
    state = state.toNumber();
    outcome = outcome.toNumber();
    timeoutInBlocks = timeoutInBlocks.toNumber();
    previousBlock = previousBlock.toNumber();
    hand0 = hand0.toNumber();
    hand1 = hand1.toNumber();
    return {state, outcome, timeoutInBlocks, previousBlock, player0, player1, player0Commitment, wagerInWei, escrowInWei, salt, hand0, hand1};};

const isEqualState = (x, y) =>
    x.state == y.state &&
    x.outcome == y.outcome &&
    x.timeoutInBlocks == y.timeoutInBlocks &&
    x.previousBlock == y.previousBlock &&
    x.player0 == y.player0 &&
    x.player1 == y.player1 &&
    x.player0Commitment == y.player0Commitment &&
    x.wagerInWei == y.wagerInWei &&
    x.escrowInWei == y.escrowInWei &&
    x.salt == y.salt &&
    x.hand0 == y.hand0 &&
    x.hand1 == y.hand1;

// address => {state: Uint8, outcome: Uint8, timeoutInBlocks: int, previousBlock: int, player0: address, player1: address, player0Commitment: bytes32, wagerInWei: BN, escrowInWei: BN, salt: bytes32, hand0: Uint8, hand1: Uint8} => `a) => `a
const queryState = (contractAddress, blockNumber) => (k, kError = kLogError) => {
    const rps = web3.eth.contract(rpsAbi).at(contractAddress);
    return errbacK(rps.query_state.call)({}, blockNumber)(x => k(decodeState(x)))};

// (int => `a) => `a
const queryConfirmedState = contractAddress => (k, kError = kLogError) =>
    getConfirmedBlockNumber(
        blockNumber => queryState(contractAddress, blockNumber)(k, kError), kError);

const decodeGameCreationData = (data, blockNumber, txHash) => {
    const x = i => data.slice(2+i*64,66+i*64);
    const contract = hexToAddress(x(0));
    const player0 = hexToAddress(x(1));
    const player1 = hexToAddress(x(2));
    const timeoutInBlocks = hexToBigNumber(x(3)).toNumber();
    const player0Commitment = hexTo0x(x(4));
    const wagerInWei = hexToBigNumber(x(5));
    const escrowInWei = hexToBigNumber(x(6));
    return {contract, player0, player1, timeoutInBlocks,
            player0Commitment, wagerInWei, escrowInWei, blockNumber, txHash};}

// txHash => {contract: address, player0: address, player1: address, timeoutInBlocks: integer,
// commitment: bytes32, wagerInWei: BN, escrowInWei: BN, blockNumber: integer}
const getGameCreationData = txHash => (k, kError = kLogError) =>
    errbacK(web3.eth.getTransactionReceipt)(txHash)(
        receipt => {
            const result = decodeGameCreationData(receipt.logs[0].data, receipt.blockNumber, txHash);
            if(receipt.transactionHash == txHash
               && receipt.status == "0x1"
               && receipt.from == result.player0
               && receipt.to == config.contract.address
               && receipt.logs.length == 1) {
                return k(result);
            } else {
                return kError("bad rps game creation data receipt", txHash, receipt, result);
            }},
        kError);

// RECEIVING DATA FROM THE BLOCKCHAIN

let nextID;
let activeGames = {};
const gamesByTxHash = {};
const activeGamesByCommitment = {}; // To search an active game by commitment...

const zeroAddress = "0x0000000000000000000000000000000000000000";

const uint32ToHex = u => web3.toHex(u + 0x100000000).slice(3);
const idToString = uint32ToHex;
const stringToId = x => parseInt(x, 10);

let logGame = id => logging("render:", id, getUserStorage(idToString(id)))();

// Default hook (until replaced by the UI frontend), just log the updated game.
let renderGameHook = logGame

const getGameID = () => {
    const gameID = nextID;
    nextID = nextID + 1;
    putUserStorage("nextID", nextID);
    return gameID;
}

const registerGameK = game => k => {
    const id = getGameID();
    putUserStorage(idToString(id), game);
    activeGames[id] = true;
    if (game.txHash) {
        gamesByTxHash[game.txHash] = id;
    }
    renderGameHook(id);
    return k();
}

const processNewGameK = event => k => {
    const game = decodeGameCreationData(event.data, event.blockNumber, event.transactionHash);
    // logging("newGame")(game);
    if (!(game.player0 == userAddress || game.player1 == userAddress || game.player1 == zeroAddress)) {
        // logging("not for us!")();
        return k();
    }
    let id = gamesByTxHash[event.transactionHash];
    if (id) {
        if (id.contract) { // Known game. Assume blockNumber is also known.
            // logging("old stuff!")();
            return k();
        } else {
            // logging("yay contract!")(game.contract);
            // TODO: triple-check that everything matches, or issue warning?
            updateUserStorage(idToString(id), {blockNumber: game.blockNumber, contract: game.contract});
            renderGameHook(id);
            return k();
        }
    } else {
        // logging("Just register a new game!")();
        // TODO: handle the case where we're player0 but we crashed between the time the transaction was
        // published and the time we could save the txHash to localStorage.
        // TODO: also handle when we're player0 but that was started on another browser.
        return registerGameK(game)(k);
    }
}

const watchNewGames = k =>
    registerConfirmedEventHook(
        "confirmedNewGames",
        // TODO: only track starting from 2 timeout periods in the past(?)
        config.contract.creationBlock,
        {address: config.contract.address},
        processNewGameK)(k);

const addActiveGame = id => activeGames[id] = true;
const removeActiveGame = id => delete activeGames[id];
const activeGamesList = () => Object.keys(activeGames).map(stringToId);

const processActiveGame = id => k => {
    const idString = idToString(id);
    const game = getUserStorage(idString);
    if (game.isComplete) {
        removeActiveGame(id);
        return k();
    }
    if (!game.contract) { // Nothing to watch (yet)
        return k();
    }
    const kError = error => logErrorK(error)(k);
    return queryState(game.contract, "pending")(
        unconfirmedState =>
            game.confirmedState == unconfirmedState ? k() : // No change since last confirmed state
            queryConfirmedState(game.contract)(
                confirmedState => {
                    if (isEqualState(confirmedState, game.confirmedState) &&
                        isEqualState(unconfirmedState, game.unconfirmedState)) {
                        return k();
                    }
                    updateUserStorage(idString, {confirmedState, unconfirmedState});
                    renderGameHook(id);
                    // TODO: handle timeout
                    return k();
                },
                kError),
        kError)}

const processActiveGames = (_firstUnprocessedBlock, _lastUnprocessedBlock) => k =>
      forEachK(processActiveGame)(activeGamesList())(k);

const watchActiveGames = k => {
    newBlockHooks["confirmedActiveGames"] = processActiveGames;
    return processActiveGames(k);
}

const initBackend = k => {
    if (config && config.contract) { // Avoid erroring on an unconfigured network
        rpsFactory = web3.eth.contract(rpsFactoryAbi).at(config.contract.address);
        nextID = getUserStorage("nextID", 0);
        for (let id = 0; id < nextID; id++) {
            let game = getUserStorage(idToString(id));
            if (game) {
                if (game.txHash) {
                    gamesByTxHash[game.txHash] = id;
                }
                if (!game.isComplete) {
                    addActiveGame(id);
                }
                if (!game.isDismissed) {
                    renderGameHook(id);
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
    newBlockHooks["newBlock"] = (from, to) => k => {
        console.log("newBlock! from:", from, "to:", to); return k(); }};
