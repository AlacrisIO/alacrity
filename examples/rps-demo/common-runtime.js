/** Common runtime for Alacris DApps when targetting Ethereum using web3 */
'use strict';

/* TODO LATER:

 * We use web3 for posting transactions, even though it's unreliable.
   In the future, we should post transactions only after persisting locally,
   and carefully play the auction game so as to post transactions without paying too much in fees.
   In other words, we should be using our legilogic_ethereum library,
   as compiled from OCaml to JS using bucklescript.

 * Handle "query returned more than 1000 results" when too many contracts created in interval!!!
 */

/** Get an identifier for the current network.
   TODO: when using web3 1.0, use web3.eth.net.getId().then(k) or whatever it exports,
   to also extract the chainId, which can notably distinguish between ETH and ETC
   (both claim to be networkId 1, but the latter uses chainId 61)
   : () => string */
const getNetworkId = () => web3.currentProvider.networkVersion;
let networkId;

/** Get the address of the current user.
    : () => address */
const getUserAddress = () => web3.currentProvider.selectedAddress;
let userAddress;

const zeroAddress = "0x0000000000000000000000000000000000000000";
const optionalAddressOf0x = x => x == zeroAddress ? undefined : x;
const optionalAddressTo0x = x => x || zeroAddress;
const optionalAddressMatches = (pattern, address) => !pattern || pattern == address;

/** The networkConfig is an object that maps the networkId (TODO: in the future, plus chain Id?)
    to the config below. It is the responsibility of the DApp developers to define networkConfig
    as part of their DApp deployment, typically in a dapp-config.js file loaded from the HTML page.
    : StringTable(config)
 */
let networkConfig;

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
let config;

// Unit conversion
const toBN = web3.toBigNumber;
const weiPerEth = toBN(1e18);
const ethToWei = e => toBN(e).mul(weiPerEth).floor();
const weiToEth = w => toBN(w).div(weiPerEth);
const meth = x => toBN(1e15).mul(x);

/** Convert a hex string to a BigNumber
    : string => BigNumber */
const hexToBigNumber = hex => toBN(hexTo0x(hex));
const uint32ToHex = u => web3.toHex(u + 0x100000000).slice(3);
const hexToInt = x => parseInt(x, 16);

const digest = web3.sha3;
const digestHex = x => web3.sha3(x, {encoding: "hex"});
const saltedDigest = toHex => (salt, ...data) => digestHex(salt + toHex(...data));

/** Count the number of confirmations for a transaction given by its hash.
    Return -1 if the transaction is yet unconfirmed.
    : String0x => KontE(int) */
const getConfirmations = txHash => (k = kLogResult, kError = kLogError) =>
    errbacK(web3.eth.getTransaction)(txHash)(
        txInfo => // Get TxInfo
            // When transaction is unconfirmed, its block number is null.
            // In this case we return -1 as number of confirmations
            (txInfo === null || txInfo.blockNumber === null) ? k(-1) :
            errbacK(web3.eth.getBlockNumber)()(
                currentBlock => k(currentBlock - txInfo.blockNumber),
                kError),
        kError);

/** Wait for a transaction to be confirmed
    : String0x => KontE() */
const confirmEtherTransaction =
      (txHash, confirmations = config.confirmationsWantedInBlocks) => (k = kLogResult) => {
    const kRetry = () =>
          setTimeout((() => confirmEtherTransaction(txHash, confirmations)(k)),
                     config.blockPollingPeriodInSeconds * 1000);
    return errbacK(getConfirmations)(txHash)(
        txConfirmations => (txConfirmations >= confirmations) ? k() : kRetry(),
        error => logErrorK(error)(kRetry))}

/** Get the number of the latest confirmed block
   : KontE(int) */
const getConfirmedBlockNumber = (k = kLogResult, kError = kLogError) =>
    errbacK(web3.eth.getBlockNumber)()(
        currentBlock => k(currentBlock - config.confirmationsWantedInBlocks),
        kError);

// General Purpose Ethereum Blockchain Watcher
/** : int */
let nextUnprocessedBlock;

/** : StringTable(K(int)) */
const newBlockHooks = {};

/** Process new blocks.
    After processing, continue with continuation k.
    Invoke k whether or not any new blocks were processed, and
    whether or not the processing was successful.
    : Kont() */
const processNewBlocks = k =>
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
const watchBlockchain = () =>
    processNewBlocks(() => setTimeout(watchBlockchain, config.blockPollingPeriodInSeconds * 1000));

/** hook to synchronously watch all events of some kind as the chain keeps getting updated */
const processEvents = (filter, processK) => (fromBlock, toBlock) => k => {
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
const registerConfirmedEventHook = (name, fromBlock, filter, processK, confirmations = config.confirmationsWantedInBlocks) => k => {
    newBlockHooks[name] = (firstUnprocessedBlock, lastUnprocessedBlock) => k =>
        processEvents(filter, processK)
            (firstUnprocessedBlock - confirmations, lastUnprocessedBlock - confirmations)(k);
    return processEvents(filter, processK)(fromBlock, nextUnprocessedBlock - 1)(k);}

const registerUnconfirmedEventHook = (name, filter, processK, confirmations = config.confirmationsWantedInBlocks) => k => {
    const hook = (firstUnprocessedBlock, lastUnprocessedBlock) => k =>
        processEvents(filter, processK)
            (Math.max(firstUnprocessedBlock, lastUnprocessedBlock - confirmations + 1),
             "pending")(k);
    newBlockHooks[name] = hook;
    const fromBlock = nextUnprocessedBlock - 1 - confirmations;
    return hook(filter, processK)(fromBlock, "pending")(k);}

/** Given some code in 0x form (.bin output from solc), deploy a contract with that code
    and CPS-return its transactionHash
    : String0x => Kont(digest) */
const deployContract = code => errbacK(web3.eth.sendTransaction)({data: code});


// The code in the section below might belong to some library to manage multiple interactions.
// Managing interactions
let nextId; // runs up from 0, in the table of games as per games.get
let previousUnconfirmedId; // runs down from -1, in the same table of games as games.get.
const activeGames = {}; // maps id to true
const gamesByTxHash = {}; // maps txHash to id.

let logGame = (id, tag = "Game:") => logging(tag, id, userStorage.get(intToString(id)))();

// Hook for rendering a game, to be replaced later by the UI frontend.
// Default behavior: just log the updated game.
let renderGameHook = logGame;

const games = rekeyContainer(userStorage, intToString);
const getGame = games.get;
const updateGame = (id, gameUpdate) => games.modify(id, merge(gameUpdate));
const deleteGame = games.remove;
const deleteGameField = (id, field) => games.modify(id, g => delete g[field]);

const getUnconfirmedGameId = () => {
    const unconfirmedGameId = --previousUnconfirmedGameId;
    userStorage.set("previousUnconfirmedGameId", previousUnconfirmedGameId);
    return unconfirmedGameId;}
const addUnconfirmedGame = game => {
    const unconfirmedGameId = getUnconfirmedGameId();
    userStorage.set(unconfirmedGameId, game);
    return unconfirmedGameId;}
const removeUnconfirmedGame = id => {
    if (id === previousUnconfirmedGameId) {
        previousUnconfirmedGameId++;
        userStorage.set("previousUnconfirmedGameId", previousUnconfirmedGameId);}
    removeUserStorage(id);}
const confirmGame = (unconfirmedId, data) => {
    const game = merge(data)(games.get(unconfirmedId));
    const id = addGame(game);
    removeUnconfirmedGame(id);
    gamesByTxHash[game.txHash] = id;
    addActiveGame(id);
    renderGameHook(id, "Confirm Game:");
    return id;}


const getGameId = () => {
    const gameId = nextId++;
    userStorage.set("nextId", nextId);
    return gameId;}
// : game => id
const addGame = game => {
    const id = getGameId();
    games.set(id, game);
    activeGames[id] = true;
    if (game.txHash) {
        gamesByTxHash[game.txHash] = id;
    }
    // Don't display yet: let a later hook do it based on further confirmed state information.
    // Indeed, if the game is irrelevant, we don't even want to have to dismiss it.
    // renderGameHook(id, "Add Game:");
    return id;}
const removeGame = id => {
    const g = getGame(id);
    if (g.txHash) {
        delete gamesByTxHash[g.txHash];}
    deleteGame(id);
    removeActiveGame(id);
    renderGameHook(id);
    if (id == nextId - 1) {
        nextId = id;
        userStorage.set("nextId", nextId);}}

const addActiveGame = id => activeGames[id] = true;
const removeActiveGame = id => delete activeGames[id];
const activeGamesList = () => Object.keys(activeGames).map(stringToInt);

const timeoutBlocks = new TinyQueue; // blocks (by number) that include a timeout.
const blockTimeouts = {}; // for each block (by number, stringified), a set of game ids to mind.

const queueGame = (id, timeoutBlock) => {
    let queuedBlocks = blockTimeouts[timeoutBlock];
    if (!queuedBlocks) {
        queuedBlocks = {};
        timeoutBlocks.push(timeoutBlock);
        blockTimeouts[timeoutBlock] = queuedBlocks;}
    queuedBlocks[id] = true;}

const dismissGame = (id, game) => {
    if (!game.isDismissed) {
        updateGame(id, {isDismissed: true});}
    removeActiveGame(id);
    renderGameHook(id, "Dismiss Game:");}

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
let processGameAtHook;

/** Given the data string from event from the factory contract's logs,
    as well as the blockNumber and txHash for the event, decode the data into a game object.
    The object can be compared to an existing game object with gameMatches() below
    to check whether their initial parameters match.
    : (string0x, int, txHash) => game
*/
let decodeGameCreationEvent;

/** Given a game as extracted by decodeGameCreationEvent, extract the initial game data:
    creation txHash, creation blockNumber, any session identifier or contract address,
    dictionary or vector of public parameters, dictionary or vector of private variables, etc.
   : (string0x, int, txHash) => game
*/
let decodeGameEvent;

/** Given game data from a game creation event, and a previously-registered game (possibly undefined),
    determine if the two match.
   : (game, game) => bool
*/
let gameMatches;

/** Given a game and a user's address, determine whether the game is relevant to the user.
   : (game, address) => bool
*/
let isGameRelevantToUser;

/** Given a game and a user's address, determine whether the user initiated the game.
   : (game, address) => bool
*/
let isGameInitiator;

/** Process a game, making all automated responses that do not require user input.
    This is perhaps the heart of the algorithm.
 */
// TODO: are we triggering a renderGame here when something changes, or somewhere else?
const processGameAt = confirmedBlock => id => k => {
    // TODO: move the beginning of this function to a common file...
    const game = getGame(id);
    // logging("processGameAt", id, game)();
    if (!game // No game: It was skipped due to non-atomicity of localStorage, or Garbage-Collected.
        || !game.confirmedState // Game issued, but no confirmed state yet. Wait for confirmation.
        || game.isDismissed) { // Game already dismissed
        return k();}
    if (game.confirmedState.state == State.Completed) { // Game already completed, nothing to do.
        updateGame(id, {isCompleted: true});
        removeActiveGame(id);
        return k();}
    return processGameAtHook(confirmedBlock)(id)(k);}

const processGame = id => k =>
    getConfirmedBlockNumber(block => processGameAt(block)(id)(k));

const attemptGameCreation = game => func => (...args) => {
    const id = addUnconfirmedGame(game);
    // TODO: add the ID to the contract call for tracking purpose? Use the low bits of the escrow?
    // Or the high bits of the hand? No, use the commitment and the rest of the data.
    // Somehow when we restart transactions, we match them by content
    // and the salted commitment ought to provide enough unicity.
    // We could use the nonce for the transaction, but there's no atomic access to it.
    // Could we save the TxHash locally *before* sending it online? Unhappily web3 doesn't allow that:
    // < https://github.com/MetaMask/metamask-extension/issues/3475 >.
    renderGameHook(id);
    return errbacK(func)(...args)(
        txHash => confirmGame(id, {txHash}),
        error => {
            removeUnconfirmedGame(id);
            return loggedAlert(error);});}

const handleTimeoutQueueBefore = confirmedBlock => k => {
    if (timeoutBlocks.length == 0 || timeoutBlocks.peek() > confirmedBlock) {
        return k();
    } else {
        const block = timeoutBlocks.pop();
        const gameSet = popEntry(blockTimeouts, block);
        const gameList = Object.keys(gameSet).map(stringToInt);
        return forEachK(processGameAt(confirmedBlock))(gameList)(
            () => handleTimeoutQueueBefore(confirmedBlock)(k)); }}

const handleTimeoutQueue = (_oldBlock, currentBlock) =>
    handleTimeoutQueueBefore(currentBlock - config.confirmationsWantedInBlocks);

/** Decode an event into a game, associate this game to an existing game, or register a new one.
   : GameCreationEvent => Kont()
 */
const processNewGame = event => k => {
    const game = decodeGameCreationEvent(event.data, event.blockNumber, event.transactionHash);
    if (!isGameRelevantToUser(game, userAddress)) {
        return k();
    }
    let id = gamesByTxHash[event.transactionHash];
    if (id) {
        const g = games.get(id);
        if (g.contract) { // TODO: this assume one contract per game.
            // Known game. Assume blockNumber is also known and the game is rendered already.
            return k();
        }
        updateGame(id, game); // TODO: have a "creation" subset of the information?
        return k();
    }
    if (!isGameInitiator(game, userAddress)) {
        // A game proposed by someone else, that is relevant and not yet registered. Register it!
        addGame(game); // or should we keep the blockdata separate?
        return k();
    }
    // Handle the case where we're player0 but we crashed between
    // the time the transaction was published and
    // the time we could save the txHash to localStorage,
    // by keeping the set of interrupted games.
    for (let id = previousUnconfirmedId; id < 0; id++) {
        if (gameMatches(game, getGame(id))) {
            confirmGame(id, game);
            return k();
        }
    }
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
const watchNewGames = k =>
    registerConfirmedEventHook(
        "confirmedNewGames",
        // TODO: only track starting from 2 timeout periods in the past(?)
        config.contract.creationBlock,
        {address: config.contract.address},
        processNewGame)(k);

const initGame = id => {
    let game = getGame(id);
    if (!game) { return; }
    if (game.txHash) {
        gamesByTxHash[game.txHash] = id;
    }
    if (!game.isCompleted && !game.isDismissed) { addActiveGame(id); }
    renderGameHook(id, "Init Game:");
}

const initGames = k => { for(let i=0;i<nextId;i++) {initGame(i)}; return k(); }

const resumeGames = k => forEachK(processGame)(range(previousUnconfirmedId, nextId-previousUnconfirmedId))(k);

/** : Kont() */
const initRuntime = k => {
    networkId = getNetworkId();
    userAddress = getUserAddress(); // NB: assuming a call to .toLowercase() is redundant
    if (!userAddress) {
        loggedAlert(`Your user address is undefined. \
Please reload this page with metamask enabled and an account selected.`)
    }
    config = networkConfig[networkId];
    userId = `${networkId}.${userAddress}`;
    nextUnprocessedBlock = userStorage.get("nextUnprocessedBlock", 0);
    watchBlockchain();
    nextId = userStorage.get("nextId", 0);
    previousUnconfirmedId = userStorage.get("previousUnconfirmedId", 0);
    // For debugging purposes only:
    newBlockHooks["newBlock"] = (from, to) => loggingK("newBlock! from:", from, "to:", to)();
    return k();
}
registerInit({
    Runtime: {fun: initRuntime},
    Games: {fun: initGames, dependsOn: ["Frontend"]},
    ResumeGames: {fun: resumeGames, dependsOn: ["Games"]},
})
