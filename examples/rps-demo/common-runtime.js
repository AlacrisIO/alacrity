'use strict';

/** Get an identifier for the current network.
   TODO: when using web3 1.0, use web3.eth.net.getId().then(k) or whatever it exports,
   to also extract the chainID, which can notably distinguish between ETH and ETC
   (both networkID 1, the latter chainID 61)
   : () => string */
const getNetworkID = () => web3.currentProvider.networkVersion;

/** Get the address of the current user.
    : () => address */
const getUserAddress = () => web3.currentProvider.selectedAddress;

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
const weiPerEth = web3.toBigNumber(1e18);
const ethToWei = (e) => web3.toBigNumber(e).mul(weiPerEth).floor();
const weiToEth = (w) => web3.toBigNumber(w).div(weiPerEth);

const hexToBigNumber = (hex) => web3.toBigNumber(hexTo0x(hex));

const digest = web3.sha3;
const digestHex = (x) => web3.sha3(x, {encoding: "hex"});

/** Run an ethereum query. Eat errors and stop.
    TODO: handle error and/or have a combinator to invoke continuation after either error or success.
    : (...'a => Kont(error, 'b)) => ...'a => Kont('b) */
const ethQuery = (func) => (...args) => (successK = identityK, errorK = logErrorK) =>
      func(...args, handlerK(successK, errorK));

/** Count the number of confirmations for a transaction given by its hash.
    Return -1 if the transaction is yet unconfirmed.
    : String0x => Kont(int) */
const getConfirmations = (txHash) => (k) =>
    ethQuery(web3.eth.getTransaction)(txHash)((txInfo) => // Get TxInfo
    ethQuery(web3.eth.getBlockNumber)()((currentBlock) => // Get current block number
    // When transaction is unconfirmed, its block number is null.
    // In this case we return -1 as number of confirmations
    k(tx.blockNumber === null ? -1 : currentBlock - txInfo.blockNumber)));

/** Wait for a transaction to be confirmed
    : String0x => Kont() */
const confirmEtherTransaction = (txHash, confirmations = config.confirmationsWantedInBlocks) => (k) =>
    ethQuery(getConfirmations)(txHash)((txConfirmations) =>
    (txConfirmations >= confirmations) ? k() :
    setTimeout((() => confirmEtherTransaction(txHash, confirmations)(k)),
              config.blockPollingPeriodInSeconds * 1000));

/** Get the number of the latest confirmed block
    TODO: use an error,continuation monad?
   : Kont(int) */
const getConfirmedBlockNumber = (k) =>
    ethQuery(web3.eth.getBlockNumber)()((currentBlock) => // Get current block number
    k(currentBlock - config.confirmationsWantedInBlocks));


// General Purpose Ethereum Blockchain Watcher
/** : int */
let nextUnprocessedBlock;

/** : StringTable(K(int)) */
const confirmedHooks = {};

/** : StringTable(K(int)) */
const unconfirmedHooks = {};

// Have a parallel variant?
/** : StringTable(Not(...'a)) => ...'a => Kont() */
const runHooks = (hooks) => (...args) => (k) =>
    forEachK((entry) => entry[1](...args))(Object.entries(hooks).sort(compareFirst))(k);

/** Kont()
 */
const newBlockHooks = {};
const processNewBlocks = (k) =>
    web3.eth.getBlockNumber(handlerK(
        ((currentBlock) =>
         (currentBlock >= nextUnprocessedBlock) ?
         runHooks(newBlockHooks)(nextUnprocessedBlock, currentBlock)
         (() => {
             nextUnprocessedBlock = currentBlock + 1;
             putUserStorage("nextUnprocessedBlock", nextUnprocessedBlock);
             return k();}) :
         k()),
        seqK(logErrorK, k)));

/** Kont() */
const watchBlockchain = () =>
      processNewBlocks(() => setTimeout(watchBlockchain, config.blockPollingPeriodInSeconds * 1000));

/** hook to synchronously watch all events of some kind as the chain keeps getting updated */
const processEvents = (filter, processK) => (fromBlock, toBlock) => (k) => {
    fromBlock = Math.max(fromBlock,0);
    (fromBlock <= toBlock) ?
        web3.eth.filter({...filter, fromBlock, toBlock})
        .get(handlerThenK(forEachK(processK), logErrorK)(k)) :
    k(); }

/** Register a confirmed event hook.
   NB: *IF* some event has hooks for both confirmed and unconfirmed events, then
   (1) the confirmed event hook must be registered *before*, and
   (2) the name of the confirmed event hook must be lexicographically strictly less
   than the name for the corresponding unconfirmed event hook.
   */
const registerConfirmedEventHook = (name, fromBlock, filter, processK, confirmations = config.confirmationsWantedInBlocks) => (k) => {
    newBlockHooks[name] = (firstUnprocessedBlock, lastUnprocessedBlock) => (k) =>
        processEvents(filter, processK)
            (firstUnprocessedBlock - confirmations, lastUnprocessedBlock - confirmations)(k);
    return processEvents(filter, processK)(fromBlock, nextUnprocessedBlock - 1)(k); }

const registerUnconfirmedEventHook = (name, filter, processK, confirmations = config.confirmationsWantedInBlocks) => (k) => {
    const hook = (firstUnprocessedBlock, lastUnprocessedBlock) => (k) =>
        processEvents(filter, processK)
            (Math.max(firstUnprocessedBlock, lastUnprocessedBlock - confirmations),
             lastUnprocessedBlock)(k);
    newBlockHooks[name] = hook;
    const toBlock = nextUnprocessedBlock - 1;
    const fromBlock = toBlock - confirmations;
    hook(filter, processK)(fromBlock, toBlock)(k); }

/** Given some code in 0x form (.bin output from solc), deploy a contract with that code
    and CPS-return its transactionHash
    : String0x => Kont(digest) */
const deployContract = (code) => (k) => ethQuery(web3.eth.sendTransaction)({data: code})(k);

/** : Kont() */
const initRuntime = (k) => {
    const networkID = getNetworkID();
    config = networkConfig[networkID];
    userID = `${networkID}.${getUserAddress()}`;
    nextUnprocessedBlock = getUserStorage("nextUnprocessedBlock", 0);
    return k();
}
