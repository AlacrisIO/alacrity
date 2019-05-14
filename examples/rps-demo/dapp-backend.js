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
  * For every registered game that isn't complete,
    have a loop that polls for changes in game confirmed state
    (or, alternatively, also look at unconfirmed changes).
    We're polling for recomputed state at every block.
  * Handle timeouts

  TODO FOR DEMO:

  * Use a post-frontend-initialization hook to only start expensive computations
    after the frontend is initialized.

  TODO LATER MAYBE:

  * Instead of polling for contract state all the time, we could have the contract emit
    events at state transitions, and recompute the state from them.
    That would be cheaper to compute on the client and much lighter on the node, though
    it is involves more code to write. It might not cost more gas in the contract,
    since emitting events and requiring users to resubmit relevant state as arguments
    might be cheaper than storing state.

  * If the user is player0 and the txHash is not be already known, do something useful
    (due to race condition with the browser crashing before localStorage was done, or use in another browser)

  * Add speculative information from unconfirmed transactions,
    with clear distinction between what's confirmed,
    and what's unconfirmed by which player.

  * Improve discoverability so users don't have to care too much about being player0 or player1.
*/
'use strict';

const Hand = Object.freeze({Rock: 0, Paper: 1, Scissors: 2});
const handName = hand => ["rock", "paper", "scissors"][hand];
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

// (address, BN, Uint8) => KontE(TxHash)
const player1ShowHand = (contractAddress, wagerInWei, hand) => (k, kError = kLogError) =>
    errbacK(rps(contractAddress).player1_show_hand)(hand, {value: wagerInWei})(k, kError);

// (address, Uint8Array(32), Uint8) => KontE(TxHash)
const player0Reveal = (contractAddress, salt, hand) => (k, kError = kLogError) =>
    errbacK(rps(contractAddress).player0_reveal)(salt, hand, {})(k, kError);

// address => KontE(TxHash)
const player0Rescind = contractAddress => (k, kError = kLogError) =>
    errbacK(rps(contractAddress).player0_rescind)()(k, kError);

// address => KontE(TxHash)
const player1WinByDefault = contractAddress => (k, kError = kLogError) =>
    errbacK(rps(contractAddress).player1_win_by_default().send)()(k, kError);

const decodeState = x => {
    let [state, outcome, timeoutInBlocks, previousBlock, player0, player1, player0Commitment, wagerInWei, escrowInWei, salt, hand0, hand1] = x;
    state = state.toNumber();
    outcome = outcome.toNumber();
    timeoutInBlocks = timeoutInBlocks.toNumber();
    previousBlock = previousBlock.toNumber();
    wagerInWei = toBN(wagerInWei);
    escrowInWei = toBN(escrowInWei);
    hand0 = hand0.toNumber();
    hand1 = hand1.toNumber();
    return {state, outcome, timeoutInBlocks, previousBlock, player0, player1, player0Commitment, wagerInWei, escrowInWei, salt, hand0, hand1};};

const isEqualState = (x, y) =>
    x === y ||
    (typeof x == "object" &&
     typeof y == "object" &&
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
     x.hand1 == y.hand1);

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
const activeGames = {};
const gamesByTxHash = {};
const activeGamesByCommitment = {}; // To search an active game by commitment...
const unconfirmedGames = {};

const zeroAddress = "0x0000000000000000000000000000000000000000";

const uint32ToHex = u => web3.toHex(u + 0x100000000).slice(3);
const idToString = uint32ToHex;
const stringToId = x => parseInt(x, 16);

let logGame = id => logging("render game:", id, getUserStorage(idToString(id)))();

// Default hook (until replaced by the UI frontend), just log the updated game.
let renderGameHook = logGame;

const getGame = id => getUserStorage(idToString(id));
const putGame = (id, game) => putUserStorage(idToString(id), game);
const updateGame = (id, gameUpdate) => updateUserStorage(idToString(id), gameUpdate);

const getGameID = () => {
    const gameID = nextID;
    nextID = nextID + 1;
    putUserStorage("nextID", nextID);
    return gameID;
}

const registerGameK = game => k => {
    const id = getGameID();
    putGame(id, game);
    activeGames[id] = true;
    if (game.txHash) {
        gamesByTxHash[game.txHash] = id;
    }
    renderGameHook(id);
    return k();
}

// const findUnconfirmedGames = game => k => XXX

const processNewGameK = event => k => {
    const game = decodeGameCreationData(event.data, event.blockNumber, event.transactionHash);
    if (!(game.player0 == userAddress || game.player1 == userAddress || game.player1 == zeroAddress)) {
        return k();
    }
    let id = gamesByTxHash[event.transactionHash];
    if (id) {
        if (id.contract) { // Known game. Assume blockNumber is also known.
            return k();
        } else {
            // TODO: triple-check that everything matches, or issue warning?
            updateGame(id, {blockNumber: game.blockNumber, contract: game.contract});
            renderGameHook(id);
            return k();
        }
    } else {
        // TODO: handle the case where we're player0 but we crashed between
        // the time the transaction was published and
        // the time we could save the txHash to localStorage,
        // by keeping the set of interrupted games in unconfirmedGames.
        // TODO: also handle when we're player0 but that was started on another browser,
        // by warning the user that they better reactivate the client with the data
        // before they timeout.
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

const timeoutBlocks = new TinyQueue; // blocks (by number) that include a timeout.
const blockTimeouts = {}; // for each block (by number, stringified), a set of game ids to mind.

const State = Object.freeze({
    Uninitialized: 0,
    WaitingForPlayer1: 1,        // player0 funded wager+escrow and published a commitment
    WaitingForPlayer0Reveal: 2, // player1 showed his hand
    Completed: 3                   // end of game (in the future, have a way to reset the contract to state Uninitialized?)
});

const popEntry = (table, key) => {
    const value = table[key];
    delete table[key];
    return value; }

const queueGame = (id, timeoutBlock) => {
    let queuedBlocks = blockTimeouts[timeoutBlock];
    if (!queuedBlocks) {
        queuedBlocks = {};
        timeoutBlocks.push(timeoutBlock);
    }
    queuedBlocks[idToString(id)] = true;
}

const GameResult = Object.freeze({Draw: 0, YouWin: 1, TheyWin: 2});
const gameResult = (yourHand, theirHand) => (yourHand + 3 - theirHand) % 3;
const player0GameResultSummary = (hand0, hand1, wagerInWei, escrowInWei) => {
    switch(gameResult(hand0, hand1)) {
    case GameResult.Draw: return `have a draw and recover your ${weiToEth(toBN(wagerInWei).add(escrowInWei))} ETH stake.`;
    case GameResult.YouWin: return `win ${weiToEth(wagerInWei)} ETH
and recover your ${weiToEth(wagerInWei.add(escrowInWei))} ETH stake.`;
    case GameResult.TheyWin: return `lose your ${weiToEth(wagerInWei)} ETH wager
but recover your ${weiToEth(escrowInWei)} ETH escrow.`;}}

const player1GameResultSummary = (hand0, hand1, wagerInWei, escrowInWei) => {
    switch(gameResult(hand1, hand9)) {
    case GameResult.Draw: return `have a draw and recover your ${weiToEth(wagerInWei)} ETH stake.`;
    case GameResult.YouWin: return `win ${weiToEth(wagerInWei)} ETH
and recover your ${weiToEth(wagerInWei)} ETH stake.`;
    case GameResult.TheyWin: return `lose your ${weiToEth(wagerInWei)} ETH wager.`;}}


/** Process a game, making all automated responses that do not require user input.
    This is perhaps the heart of the algorithm.
 */
// TODO: are we triggering a renderGame here when something changes, or somewhere else?
const processGame = confirmedBlock => id => k => {
    const game = getGame(id);
    if (!game // No game: It was skipped due to non-atomicity of localStorage, or Garbage-Collected.
        || !game.confirmedState // Game issued, but no confirmed state yet. Wait for confirmation.
        || game.isCompleted) { // Game already completed, nothing to do.
        return k();
    }
    if (game.confirmedState.state == State.Completed) {
        updateGame(id, {isCompleted: true});
        return k();
    }
    if (game.player0 == userAddress &&
        game.confirmedState.state == State.WaitingForPlayer0Reveal) {
        if (!game.revealTxHash) {
            const salt = game.salt;
            const hand0 = game.hand;
            const hand1 = game.confirmedState.hand1;
            const context = `In game ${id}, player1 showed his hand ${handName(hand1)}.
You must show our hand ${handName(hand0)} to
${player0GameResultSummary(hand0, hand1, game.wagerInWei, game.escrowInWei)}.`
            if (game.salt && hand0) {
                loggedAlert(`${context} Please sign the following transaction.`);
                return player0Reveal(game.contract, salt, hand)(
                    txHash => {
                        updateGame(id, {revealTxHash: txHash})
                        // Register txHash for confirmation? Nah, we're just polling for state change!
                        // But if we switch to event-tracking, that's where it would happen.
                        return k();})
            } else {
                loggedAlert(`${context} However, you do not have the salt and hand data in this client.
Be sure to start a client that has this data before the deadline.`); // TODO: print the deadline!
            }
        }
    }
    const timeoutBlock = game.previousBlock + game.timeoutInBlocks;
    if (confirmedBlock < timeoutBlock) {
        // We haven't yet confirmed that future blocks will be > previous + timeout
        // So add the current game to the queue, if it wasn't added yet.
        queueGame(id, timeoutBlock);
        return k();
    }
    if (game.player0 == userAddress &&
        game.confirmedState.state == State.WaitingForPlayer1) {
        const stakeInEth = weiToEth(web3.toBigNumber(game.wagerInWei).add(game.escrowInWei));
        loggedAlert(`Player1 timed out in game ${id},
sending a transaction to recover your stake of ${stakeInEth} ETH`);
        // TODO register the event, don't send twice.
        return player0Rescind(game.contract)(k, flip(logErrorK)(k));
    }
    if (game.player1 == userAddress &&
        game.confirmedState.state == State.WaitingForPlayer0Reveal) {
        const wagerInEth = weiToEth(game.wagerInWei);
        const stakeInEth = weiToEth(web3.toBigNumber(game.wagerInWei).mul(2).add(game.escrowInWei));
        loggedAlert(`Player0 timed out in game ${id},
sending a transaction to recover your ${wagerInEth} ETH wager
and their ${stakeInEth} ETH stake`);
        return WinByDefault(game.contract)(k, flip(logErrorK)(k));
    }
    return k();
}

const handleGameTimeout = id => k =>
    getConfirmedBlockNumber(block => processGame(block)(id)(k));

const handleTimeoutQueueBefore = confirmedBlock => k => {
    if (timeoutBlocks.length == 0 || timeoutBlocks.peek() > confirmedBlock) {
        return k();
    } else {
        const block = timeoutBlocks.pop();
        const gameSet = popEntry(blockTimeouts, block);
        const gameList = Object.keys(gameSet).map(stringToId);
        forEachK(processGame(confirmedBlock))(gameList)(
            () => handleTimeoutQueueBefore(confirmedBlock)(k)); }}

const handleTimeoutQueue = (_oldBlock, currentBlock) => k =>
    handleTimeoutQueueBefore(currentBlock - config.confirmationsWantedInBlocks)(k);

const processActiveGame = id => k => {
    const game = getGame(id);
    logging("processActiveGame", id, game)();
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
                    updateGame(id, {confirmedState, unconfirmedState});
                    renderGameHook(id);
                    return getConfirmedBlockNumber(block => processGame(block)(id)(k));
                },
                kError),
        kError)}

const processActiveGames = (_firstUnprocessedBlock, _lastUnprocessedBlock) => k =>
      forEachK(processActiveGame)(activeGamesList())(k);

const watchActiveGames = k => {
    console.log("watchActiveGames", activeGamesList());
    newBlockHooks["confirmedActiveGames"] = processActiveGames;
    return processActiveGames()(k);
}

const resumeGame = id => k => {
    let game = getGame(id);
    if (!game) {
        return k();
    }
    if (game.txHash) {
        gamesByTxHash[game.txHash] = id;
    } else {
        unconfirmedGames[idToString(id)] = true;
    }
    if (!game.isDismissed) {
        renderGameHook(id);
    }
    if (game.isComplete) {
        return k();
    }
    addActiveGame(id);
    return handleGameTimeout(id)(k);}

const resumeGames = k => {
    nextID = getUserStorage("nextID", 0);
    return forEachK(resumeGame)(range(0, nextID))(k);}

const initBackend = k => {
    if (!config || !config.contract) { // Avoid erroring on an unconfigured network
        return k();
    }
    rpsFactory = web3.eth.contract(rpsFactoryAbi).at(config.contract.address);
    return resumeGames(
        () => watchNewGames(
        () => watchActiveGames(k)))}

registerInit(initBackend);

/** Test games */
var gsalt = "0x30f6cb71704ee3321c0bb552120a492ab2406098f5a89b0a765155f4f5dd9124";
var alice = "0x60B8193167c5355779B4d03cA37A0C11807e667f";
var bob = "0xa8c6677434CDb243E9f19Cca387a916487da8C2f";

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
var g2c = g => g.salt && g.hand && makeCommitment(g.salt, g.hand);
var i2c = seq(getGame)(g2c)
