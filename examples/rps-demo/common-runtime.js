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
   to also extract the chainID, which can notably distinguish between ETH and ETC
   (both claim to be networkID 1, but the latter uses chainID 61)
   : () => string */
const getNetworkID = () => web3.currentProvider.networkVersion;
let networkID;

/** Get the address of the current user.
    : () => address */
const getUserAddress = () => web3.currentProvider.selectedAddress;
let userAddress;

const zeroAddress = "0x0000000000000000000000000000000000000000";


/** The networkConfig is an object that maps the networkID (TODO: in the future, plus chain ID?)
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
                    putUserStorage("nextUnprocessedBlock", nextUnprocessedBlock);
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
        k(); }

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
    return processEvents(filter, processK)(fromBlock, nextUnprocessedBlock - 1)(k); }

const registerUnconfirmedEventHook = (name, filter, processK, confirmations = config.confirmationsWantedInBlocks) => k => {
    const hook = (firstUnprocessedBlock, lastUnprocessedBlock) => k =>
        processEvents(filter, processK)
            (Math.max(firstUnprocessedBlock, lastUnprocessedBlock - confirmations + 1),
             "pending")(k);
    newBlockHooks[name] = hook;
    const fromBlock = nextUnprocessedBlock - 1 - confirmations;
    return hook(filter, processK)(fromBlock, "pending")(k); }

/** Given some code in 0x form (.bin output from solc), deploy a contract with that code
    and CPS-return its transactionHash
    : String0x => Kont(digest) */
const deployContract = code => errbacK(web3.eth.sendTransaction)({data: code});


// The code in the section below might belong to some library to manage multiple interactions.
// Managing interactions
let nextID;
const activeGames = {};
const gamesByTxHash = {};
const unconfirmedGames = {};

const idToString = uint32ToHex;
const stringToId = hexToInt;

let logGame = (id, tag = "Game:") => logging(tag, id, getUserStorage(idToString(id)))();

// Hook for rendering a game, to be replaced later by the UI frontend.
// Default behavior: just log the updated game.
let renderGameHook = logGame;

const getGame = id => getUserStorage(idToString(id));
const putGame = (id, game) => putUserStorage(idToString(id), game);
const updateGame = (id, gameUpdate) => updateUserStorage(idToString(id), gameUpdate);
const deleteGame = id => removeUserStorage(idToString(id));
const deleteGameField = (id, field) => deleteUserStorageField(idToString(id), field);

const getGameID = () => {
    const gameID = nextID++;
    putUserStorage("nextID", nextID);
    return gameID;
}

// : game => ()
const registerGame = game => {
    const id = getGameID();
    putGame(id, game);
    activeGames[id] = true;
    if (game.txHash) {
        gamesByTxHash[game.txHash] = id;
    }
    renderGameHook(id, "Register Game:");
}

const removeGame = id => {
    const g = getGame(id);
    if (g.txHash) {
        delete gamesByTxHash[g.txHash];
    }
    deleteGame(id);
    const idString = idToString(id);
    delete unconfirmedGames[idToString(id)];
    removeActiveGame(id);
    renderGameHook(id);
    if (id == nextID - 1) {
        nextID = id;
        putUserStorage("nextID", nextID);
    }
}

const addActiveGame = id => activeGames[id] = true;
const removeActiveGame = id => delete activeGames[id];
const activeGamesList = () => Object.keys(activeGames).map(parseDecimal);

const timeoutBlocks = new TinyQueue; // blocks (by number) that include a timeout.
const blockTimeouts = {}; // for each block (by number, stringified), a set of game ids to mind.

const queueGame = (id, timeoutBlock) => {
    let queuedBlocks = blockTimeouts[timeoutBlock];
    if (!queuedBlocks) {
        queuedBlocks = {};
        timeoutBlocks.push(timeoutBlock);
        blockTimeouts[timeoutBlock] = queuedBlocks;
    }
    queuedBlocks[idToString(id)] = true;
}


/** : Kont() */
const initRuntime = k => {
    networkID = getNetworkID();
    userAddress = getUserAddress(); // NB: assuming a call to .toLowercase() is redundant
    if (!userAddress) {
        loggedAlert(`Your user address is undefined. \
Please reload this page with metamask enabled and an account selected.`)
    }
    config = networkConfig[networkID];
    userID = `${networkID}.${userAddress}`;
    nextUnprocessedBlock = getUserStorage("nextUnprocessedBlock", 0);
    watchBlockchain();
    nextID = getUserStorage("nextID", 0);
    // For debugging purposes only:
    newBlockHooks["newBlock"] = (from, to) => loggingK("newBlock! from:", from, "to:", to)();
    return k();
}
registerInit({Runtime: {fun: initRuntime}});
