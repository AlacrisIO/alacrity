/** Common runtime for Alacris DApps when targetting Ethereum using web3 */
import {
    rekeyContainer, intToString, stringToInt, registerInit, keyValuePair, assert,
    handlerK, handlerThenK, errbacK, runHooks, kLogError, kLogResult, hexTo0x, range,
    logging, loggingK, logErrorK, loggedAlert, merge, popEntry, forEachK
} from "./common-utils.mjs";
import {TinyQueue} from "./tinyqueue.mjs";
import {Storage} from "./local-storage.mjs";
import {Web3, web3, networkId, userAddress} from "./web3-prelude.mjs";
import {networkConfig} from "./dapp-config.mjs";

/* TODO LATER:

 * We use web3 for posting transactions, even though it's unreliable.
   In the future, we should post transactions only after persisting locally,
   and carefully play the auction game so as to post transactions without paying too much in fees.
   In other words, we should be using our legilogic_ethereum library,
   as compiled from OCaml to JS using bucklescript.

 * Handle "query returned more than 1000 results" when too many contracts created in interval!!!
 */


export const zeroAddress = "0x0000000000000000000000000000000000000000";
export const optionalAddressOf0x = x => x == zeroAddress ? undefined : x;
export const optionalAddressTo0x = x => x || zeroAddress;
export const optionalAddressMatches = (pattern, address) => !pattern || pattern == address;

/** DApp initialization should instantiate the config configuration.

    contract: data about the contract, an object with the fields address (an address as a String0x),
    creationHash (a digest as a String0x), and creationBlock (a regular integer),
    so you can use the contract, check that it was indeed created from the expected input,
    and watch all the activity on that contract starting from the suitable block.
    If there are multiple contracts, each will have its own configuration variable,
    e.g. factoryContract, oracleContract, historyContract, registryContract, etc.

    timeoutInBlocks: how many blocks after a claim is issued is the deadline for challenges.
    Suggested value for test: 12.
    Suggested value for production: 11520.

    timeoutString: a human-readable string explaining how long the timeout is.
    **Must match timeoutInBlocks**.
    Suggested value for test: "about 3 minutes"
    Suggested value for production: "about 48 hours"

    confirmationsWantedInBlocks: Number of confirmations (in blocks) wanted
    to consider a transaction done.
    Suggested value for test: 1
    Suggested value for production: 100 (although 12 is what metamask uses)

    blockPollingPeriodInSeconds: How often to poll the chain for results.
    Suggested value: 5
*/
export let config;

// Unit conversion
export const toBN = Web3.prototype.toBigNumber;
export const weiPerEth = toBN(1e18);
export const ethToWei = e => toBN(e).mul(weiPerEth).floor();
export const weiToEth = w => toBN(w).div(weiPerEth);
export const meth = x => toBN(1e15).mul(x);

/** Convert a hex string to a BigNumber
    : string => BigNumber */
export const hexToBigNumber = hex => toBN(hexTo0x(hex));
export const uint32ToHex = u => web3.toHex(u + 0x100000000).slice(3);
export const hexToInt = x => parseInt(x, 16);

export const digest = Web3.prototype.sha3;
export const digestHex = x => Web3.prototype.sha3(x, {encoding: "hex"});
export const saltedDigest = toHex => (salt, ...data) => digestHex(salt + toHex(...data));



/** For Apps with a "current user" that may change, making keys relative to a userId.
TODO: decide a good naming policy wrt delete and remove.
Also we need to have a convention for the contents of an interaction:
creation:

TODO: maybe use leveldb via web3.db to get some atomicity?
Although that seems useless unless and until we have them expose more of leveldb.
*/
/** : string */
export let userId;
export const userKey = key => `${userId}.${key}`;
export const userStorage = rekeyContainer(Storage, userKey);
export const updateUserStorage = (key, fields) => userStorage.modify(key, merge(fields));
export const setUserStorageField = (key, field, value) => updateUserStorage(key, keyValuePair(field, value));
export const deleteUserStorageField = (key, field) => {
    const record = userStorage.get(key);
    delete record[field];
    userStorage.set(key, record);
    return record;}


/** Count the number of confirmations for a transaction given by its hash.
    Return -1 if the transaction is yet unconfirmed.
    : String0x => KontE(int) */
export const getConfirmations = txHash => (k = kLogResult, kError = kLogError) =>
    errbacK(web3.eth.getTransaction)(txHash)(
        txInfo => // Get TxInfo
            // When transaction is unconfirmed, its block number is null.
            // In this case we return -1 as number of confirmations
            (txInfo === null || txInfo.blockNumber === null) ? k(-1) :
            errbacK(web3.eth.getBlockNumber)()(
                currentBlock => k(currentBlock - txInfo.blockNumber),
                kError),
        kError)

/** Wait for a transaction to be confirmed
    : String0x => KontE() */
export const confirmTransaction =
      (txHash, confirmations = config.confirmationsWantedInBlocks) => (k = kLogResult) => {
    const kRetry = () =>
          setTimeout((() => confirmTransaction(txHash, confirmations)(k)),
                     config.blockPollingPeriodInSeconds * 1000);
    return getConfirmations(txHash)(
        txConfirmations => (txConfirmations >= confirmations ? k : kRetry)(),
        error => logErrorK(error)(kRetry))}

/** Get the number of the latest confirmed block
   : KontE(int) */
export const getConfirmedBlockNumber = (k = kLogResult, kError = kLogError) =>
    errbacK(web3.eth.getBlockNumber)()(
        currentBlock => k(currentBlock - config.confirmationsWantedInBlocks),
        kError);

// General Purpose Ethereum Blockchain Watcher
/** : int */
export let nextUnprocessedBlock;

/** : StringTable(K(int)) */
export const newBlockHooks = {};

/** Process new blocks.
    After processing, continue with continuation k.
    Invoke k whether or not any new blocks were processed, and
    whether or not the processing was successful.
    : Kont() */
export const processNewBlocks = k =>
    web3.eth.getBlockNumber(
        handlerK(
            currentBlock =>
                (currentBlock >= nextUnprocessedBlock) ?
                runHooks(newBlockHooks)(nextUnprocessedBlock, currentBlock)(() => {
                    nextUnprocessedBlock = currentBlock + 1;
                    userStorage.set("nextUnprocessedBlock", nextUnprocessedBlock);
                    return k();}) :
                k(),
            error => logErrorK(error)(k)))

/** : Kont() */
export const watchBlockchain = () =>
    processNewBlocks(() => setTimeout(watchBlockchain, config.blockPollingPeriodInSeconds * 1000));

export const initWatchBlockchain = k => {
    watchBlockchain();
    return k()}

/** hook to synchronously watch all events of some kind as the chain keeps getting updated */
export const processEvents = (filter, processK) => (fromBlock, toBlock) => k => {
    fromBlock = Math.max(fromBlock,0);
    return (typeof toBlock == "string" || fromBlock <= toBlock) ?
        web3.eth.filter({...filter, fromBlock, toBlock})
        .get(handlerThenK(forEachK(processK), logErrorK)(k)) :
        k();}

/** Register a confirmed event hook.
   NB: *IF* some event has hooks for both confirmed and unconfirmed events, then
   (1) the confirmed event hook must be registered *before*, and
   (2) the name of the confirmed event hook must be lexicographically strictly less
   than the name for the corresponding unconfirmed event hook.
   */
export const registerConfirmedEventHook = (name, fromBlock, filter, processK, confirmations = config.confirmationsWantedInBlocks) => k => {
    newBlockHooks[name] = (firstUnprocessedBlock, lastUnprocessedBlock) => k =>
        processEvents(filter, processK)(
            firstUnprocessedBlock - confirmations, lastUnprocessedBlock - confirmations)(k);
    return processEvents(filter, processK)(fromBlock, nextUnprocessedBlock - 1)(k);}

export const registerUnconfirmedEventHook = (name, filter, processK, confirmations = config.confirmationsWantedInBlocks) => k => {
    const hook = (firstUnprocessedBlock, lastUnprocessedBlock) => k =>
        processEvents(filter, processK)(
            Math.max(firstUnprocessedBlock, lastUnprocessedBlock - confirmations + 1),
             "pending")(k);
    newBlockHooks[name] = hook;
    const fromBlock = nextUnprocessedBlock - 1 - confirmations;
    return hook(filter, processK)(fromBlock, "pending")(k);}

/** Given some function that sends a transaction, see if any gas was specified;
    if not specify it from the output of estimateGas.
    : (...a => KontE(...b)) => (...a => KontE(...b)) */
export const sendTx = fun => (...args) => (k, kError = kLogError) => {
    const txObject = args.pop();
    if (txObject.gas) {
        return errbacK(fun || web3.eth.sendTransaction)(...args, txObject)(k, kError);
    } else {
        return errbacK((fun || web3.eth).estimateGas)(...args, txObject)(
            gas => errbacK(fun || web3.eth.sendTransaction)(...args, {...txObject, gas})(k, kError),
            kError)}}

/** Given some code in 0x form (.bin output from solc), deploy a contract with that code
    and CPS-return its transactionHash
    : String0x => KontE(txHash) */
export const deployContract = code => sendTx(null)({data: code});


// The code in the section below might belong to some library to manage multiple interactions.
// Managing interactions
export let nextId; // runs up from 0, in the table of games as per games.get
export let previousUnconfirmedId; // runs down from -1, in the same table of games as games.get.
export const activeGames = {}; // maps id to true
export const gamesByTxHash = {}; // maps txHash to id.

export const logGame = (id, tag = "Game:") => logging(tag, id, userStorage.get(intToString(id)))();

// Hook for rendering a game, to be replaced later by the UI frontend.
// Default behavior: just log the updated game.
export let renderGame = logGame;

export const games = rekeyContainer(userStorage, intToString);
export const getGame = games.get;
export const updateGame = (id, gameUpdate) => games.modify(id, merge(gameUpdate));
export const deleteGame = games.remove;
export const deleteGameField = (id, field) => games.modify(id, g => {delete g[field]; return g});

export const getUnconfirmedGameId = () => {
    const unconfirmedId = --previousUnconfirmedId;
    userStorage.set("previousUnconfirmedId", previousUnconfirmedId);
    return unconfirmedId;}
export const addUnconfirmedGame = game => {
    const unconfirmedGameId = getUnconfirmedGameId();
    userStorage.set(unconfirmedGameId, game);
    return unconfirmedGameId;}
export const removeUnconfirmedGame = id => {
    if (id === previousUnconfirmedId) {
        previousUnconfirmedId++;
        userStorage.set("previousUnconfirmedId", previousUnconfirmedId);}
    userStorage.remove(id);
    renderGame(id, "Remove Unconfirmed Game:")}
export const confirmGame = (unconfirmedId, data) => {
    const game = merge(data)(games.get(unconfirmedId));
    const id = addGame(game);
    removeUnconfirmedGame(unconfirmedId);
    gamesByTxHash[game.txHash] = id;
    addActiveGame(id);
    renderGame(id, "Confirm Game:");
    return id;}

export const getGameId = () => {
    const gameId = nextId++;
    userStorage.set("nextId", nextId);
    return gameId;}
// : game => id
export const addGame = game => {
    // logging("addGame", game)();
    if (!game) { return; }
    const id = getGameId();
    games.set(id, game);
    activeGames[id] = true;
    if (game.txHash) {
        gamesByTxHash[game.txHash] = id;}
    // Don't display yet: let a later hook do it based on further confirmed state information.
    // Indeed, if the game is irrelevant, we don't even want to have to dismiss it.
    // renderGame(id, "Add Game:");
    return id;}
export const removeGame = id => {
    const g = getGame(id);
    if (g && g.txHash) {
        delete gamesByTxHash[g.txHash];}
    deleteGame(id);
    removeActiveGame(id);
    renderGame(id, "Remove Game:");
    if (id == nextId - 1) {
        nextId = id;
        userStorage.set("nextId", nextId);}}

export const addActiveGame = id => activeGames[id] = true;
export const removeActiveGame = id => delete activeGames[id];
export const activeGamesList = () => Object.keys(activeGames).map(stringToInt);

export const timeoutBlocks = new TinyQueue; // blocks (by number) that include a timeout.
export const blockTimeouts = {}; // for each block (by number, stringified), a set of game ids to mind.

export const queueGame = (id, timeoutBlock) => {
    let queuedBlocks = blockTimeouts[timeoutBlock];
    if (!queuedBlocks) {
        queuedBlocks = {};
        timeoutBlocks.push(timeoutBlock);
        blockTimeouts[timeoutBlock] = queuedBlocks;}
    queuedBlocks[id] = true;}

export const dismissGame = (id, game) => {
    if (!game.isDismissed) {
        updateGame(id, {isDismissed: true});}
    removeActiveGame(id);
    renderGame(id, "Dismiss Game:");}

// HOOKS TO BE PROVIDED BY THE APPLICATION
// TODO: in a future version, have a higher-order function that takes
// these "hook" functions as parameters,
// in a way such that we can compose sub-activities together.
/** Process a game, making all automated responses that do not require user input.
    This is perhaps the heart of the algorithm.
    Takes the confirmedBlock number, the game id, a continuation,
    computes and continues with unit.
    : int => GameId => Kont()
 */
export let processGameAtHook;

/** Given the data string from event from the factory contract's logs,
    as well as the blockNumber and txHash for the event, decode the data into a game object.
    The object can be compared to an existing game object with gameMatches() below
    to check whether their initial parameters match.
    : (string0x, int, txHash) => game
*/
export let decodeGameCreationEvent;

/** Return a game event given its topic, raw event data, block number, and transaction hash.
   : blockchainEvent => gameEvent
*/
export let decodeGameEvent;

/** Given game data from a game creation event, and a previously-registered game (possibly undefined),
    determine if the two match.
   : (game, game) => bool
*/
export let gameMatches;

/** Given a game and a user's address, determine whether the game is relevant to the user.
   : (game, address) => bool
*/
export let isGameRelevantToUser;

/** Given a game and a user's address, determine whether the user initiated the game.
   : (game, address) => bool
*/
export let isGameInitiator;

/** Given the previous state, and a decoded event, determine the next state
   : (state, event) => state
 */
export let stateUpdate;

/** Has this game been confirmed yet?
    NB: this depends on the game having a blockNumber for its creation date (see decodeGameCreationEvent).
   : game => bool
 */
export const isGameConfirmed =
    game => game.blockNumber + config.confirmationsWantedInBlocks < nextUnprocessedBlock;

/** Process a game, making all automated responses that do not require user input.
    This is perhaps the heart of the algorithm.
 */
// TODO: are we triggering a renderGame here when something changes, or somewhere else?
export const processGameAt = confirmedBlock => id => k => {
    // TODO: move the beginning of this function to a common file...
    const game = getGame(id);
    //logging("processGameAt", id, game)();
    if (!game // No game: It was skipped due to non-atomicity of localStorage, or Garbage-Collected.
        || !isGameConfirmed(game) // Game issued, but no confirmed state yet. Wait for confirmation.
        || game.isDismissed) { // Game already dismissed
        return k();}
    return processGameAtHook(confirmedBlock)(id)(k);}

export const processGame = id => k =>
    getConfirmedBlockNumber(block => processGameAt(block)(id)(() => {
        renderGame(id, "Processed Game:") ; return k()}))

export const attemptGameCreation = game => func => (...args) => {
    const id = addUnconfirmedGame(game);
    // TODO: add the ID to the contract call for tracking purpose? Use the low bits of the escrow?
    // Or the high bits of the hand? No, use the commitment and the rest of the data.
    // Somehow when we restart transactions, we match them by content
    // and the salted commitment ought to provide enough unicity.
    // We could use the nonce for the transaction, but there's no atomic access to it.
    // Could we save the TxHash locally *before* sending it online? Unhappily web3 doesn't allow that:
    // < https://github.com/MetaMask/metamask-extension/issues/3475 >.
    renderGame(id, "Creating Game:");
    return errbacK(func)(...args)(
        txHash => confirmGame(id, {txHash}),
        error => {
            removeUnconfirmedGame(id);
            return loggedAlert(error);});}

export const handleTimeoutQueueBefore = confirmedBlock => k => {
    if (timeoutBlocks.length == 0 || timeoutBlocks.peek() > confirmedBlock) {
        return k();
    } else {
        const block = timeoutBlocks.pop();
        const gameSet = popEntry(blockTimeouts, block);
        const gameList = Object.keys(gameSet).map(stringToInt);
        return forEachK(processGameAt(confirmedBlock))(gameList)(
            () => handleTimeoutQueueBefore(confirmedBlock)(k)); }}

export const handleTimeoutQueue = (_oldBlock, currentBlock) =>
    handleTimeoutQueueBefore(currentBlock - config.confirmationsWantedInBlocks);

/** Decode an event into a game, associate this game to an existing game, or register a new one.
   : GameCreationEvent => Kont()
 */
export const processNewGame = event => k => {
    const game = decodeGameCreationEvent(event.data, event.blockNumber, event.transactionHash);
    //logging("processNewGame", game, gamesByTxHash, event.transactionHash)();
    if (!game || !isGameRelevantToUser(game, userAddress)) {
        return k();}
    let id = gamesByTxHash[event.transactionHash];
    if (id !== undefined) {
        const g = games.get(id);
        if (g && g.contract) { // TODO: this assume one contract per game.
            // Known game. Assume blockNumber is also known and the game is rendered already.
            return k();}
        updateGame(id, game); // TODO: have a "creation" subset of the information?
        return k();}
    if (!isGameInitiator(game, userAddress)) {
        // A game proposed by someone else, that is relevant and not yet registered. Register it!
        addGame(game); // or should we keep the blockdata separate?
        return k();}
    // Handle the case where we're player0 but we crashed between
    // the time the transaction was published and
    // the time we could save the txHash to localStorage,
    // by keeping the set of interrupted games.
    for (let id = previousUnconfirmedId; id < 0; id++) {
        if (gameMatches(game, getGame(id))) {
            confirmGame(id, game);
            return k();}}
    // This is a relevant game of which we're the initiator,
    // but the game does not match any of those locally stored.
    // That's a big problem when the game was started with private data (i.e. salt and hand0).
    // Hopefully, the game was started on another browser
    // (the bad case is when the state was lost because localStorage isn't atomic).
    // The frontend needs to warn the user somehow that they better reactivate
    // the client that has the data before they timeout, or tell them about their lost.
    addGame(game);
    return k();
}

// TODO: this only works for single-factory-contract hooks.
export const watchNewGames = k =>
    registerConfirmedEventHook(
        "confirmedNewGames",
        // TODO: only track starting from 2 timeout periods in the past(?),
        // or at least from nextUnprocessedBlock? (beware any race condition there)
        config.contract.creationBlock,
        {address: config.contract.address},
        processNewGame)(k);

// fold the list of events in a variant of an error monad
export const reduceStateUpdates = (events, stateUpdate, state) => {
    if (state.error) { return state; }
    for (let i in events) {
        try { state = stateUpdate(state, events[i]); }
        catch (error) { return {...state, error}}}
    return state}

export const processGameEvents = (id, lastUnprocessedBlock, events) => {
    const g = getGame(id);
    assert(g, `processGameEvents: undefined game ${id}`);
    const nextUnprocessedBlock = lastUnprocessedBlock - config.confirmationsWantedInBlocks + 1;
    const isConfirmed = e => e.blockNumber < nextUnprocessedBlock;
    const isUnconfirmed = e => e.blockNumber >= nextUnprocessedBlock;
    const previouslyConfirmed = g.confirmedEvents || [];
    const newlyConfirmed = events.filter(isConfirmed);
    const confirmedEvents = [...previouslyConfirmed, ...newlyConfirmed];
    const unconfirmedEvents = events.filter(isUnconfirmed);
    const history = {confirmedEvents, unconfirmedEvents, nextUnprocessedBlock};
    const game = reduceStateUpdates(newlyConfirmed, stateUpdate, merge(history)(g));
    assert(game, () => `processGameEvents: bad state update for game ${id}, before: ${JSON.stringify(g)}, events: ${JSON.stringify(newlyConfirmed)}`);
    games.set(id, game);
    renderGame(id, "Processed Game Events");}

export const registerFrontendHooks = hooks => {
    renderGame = hooks.renderGame;
}
export const registerBackendHooks = hooks => {
    processGameAtHook = hooks.processGameAtHook;
    decodeGameCreationEvent = hooks.decodeGameCreationEvent;
    decodeGameEvent = hooks.decodeGameEvent;
    gameMatches = hooks.gameMatches;
    isGameRelevantToUser = hooks.isGameRelevantToUser;
    isGameInitiator = hooks.isGameInitiator;
    stateUpdate = hooks.stateUpdate
}

// TODO: this function supposes a contract with a queryState approach.
// We probably want to use event tracking instead when generating code.
export const processActiveGame = lastUnprocessedBlock => id => k => {
    const game = getGame(id);
    // logging("processActiveGame", id, lastUnprocessedBlock, game)();
    if (!game || game.isCompleted) {
        removeActiveGame(id);
        return k();
    }
    if (!game.contract) { // Nothing to watch (yet)
        return k();
    }
    const fromBlock = game.nextUnprocessedBlock || game.blockNumber;
    const toBlock = lastUnprocessedBlock;
    return web3.eth.filter({address: game.contract, fromBlock, toBlock})
        .get(handlerK(
            events => {
                if (events.length > 0) {
                    processGameEvents(id, lastUnprocessedBlock, events.map(decodeGameEvent));
                    return processGame(id)(k);
                } else {
                    return k();}},
            error => logErrorK(error)(k)))}

export const processActiveGames = (_firstUnprocessedBlock, lastUnprocessedBlock) => k =>
      forEachK(processActiveGame(lastUnprocessedBlock))(activeGamesList())(k);

export const watchActiveGames = k => {
    /* eslint-disable no-console */
    logging("watchActiveGames", activeGamesList())();
    newBlockHooks["confirmedActiveGames"] = processActiveGames;
    getConfirmedBlockNumber(
        block => processActiveGames(0, block)(k),
        error => logErrorK(error)(k))}

export const initGame = id => {
    let game = getGame(id);
    if (!game) { return; }
    if (game.txHash) {
        gamesByTxHash[game.txHash] = id;
    }
    if (!game.isCompleted && !game.isDismissed) { addActiveGame(id); }
    renderGame(id, "Init Game:")}

export const initGames = k => { for(let i=previousUnconfirmedId;i<nextId;i++) {initGame(i)} return k()}

export const resumeGames = k => forEachK(processGame)(range(previousUnconfirmedId, nextId-previousUnconfirmedId))(k);

export const checkContract = (k = kLogResult, kError = kLogError) => {
    const {address, codeHash, creationHash, creationBlock} = config.contract;
    errbacK(web3.eth.getTransaction)(creationHash)(txInfo => {
    try {
        assert(txInfo, "No txInfo");
        assert(txInfo.blockNumber === creationBlock, () => `bad creation block: expected ${creationBlock} but got ${txInfo.blockNumber}`);
        assert(digestHex(txInfo.input) === codeHash, () => `bad code: expected ${codeHash} but got ${digestHex(txInfo.input)}.`);
    } catch (e) {return kError(e)}
    return errbacK(web3.eth.getTransactionReceipt)(creationHash)(
        receipt => {
        try {
            assert(receipt, "No tx receipt");
            assert(receipt.transactionHash === creationHash, "Bad tx hash");
            assert(receipt.blockNumber === creationBlock, "bad creation block");
            assert(receipt.contractAddress === address, "bad contract address");
        } catch (e) {return kError(e);}
        logging("Contract is OK.")()
        return k()},
        kError)})}

/** : Kont() */
export const initRuntime = k => {
    config = networkConfig[networkId];
    userId = `${networkId}.${userAddress}`;
    nextUnprocessedBlock = userStorage.get("nextUnprocessedBlock", 0);
    nextId = userStorage.get("nextId", 0);
    previousUnconfirmedId = userStorage.get("previousUnconfirmedId", 0);
    // For debugging purposes only:
    newBlockHooks["newBlock"] = (from, to) => loggingK("newBlock! from:", from, "to:", to)();
    return k()}

registerInit({
    Runtime: {fun: initRuntime, dependsOn: ["Web3"]},
    WatchBlockchain: {fun: initWatchBlockchain, dependsOn: ["Runtime"]},
    Games: {fun: initGames, dependsOn: ["Frontend"]},
    ResumeGames: {fun: resumeGames, dependsOn: ["Games"]},
    WatchNewGames: {fun: watchNewGames, dependsOn: ["ResumeGames", "WatchBlockchain"]},
    WatchActiveGames: {fun: watchActiveGames, dependsOn: ["WatchNewGames"]}});

// Local Variables:
// mode: JavaScript
// End:
