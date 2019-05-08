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

// Combinators for regular functions
/** : 'a => 'a */
const identity = (x) => x;

/** : 'a => ...'b => 'a */
const konstant = (x) => (...y) => x;

/** : (...'z => 'y => 'x) => (...'z => 'y) => ...'z => 'x */
const smelt = (x) => (y) => (...z) => x(...z)(y(...z));

/** : (...'z => ...'y => 'x) => ...'y => ...'z => 'x */
const flip = (x) => (...y) => (...z) => x(...z)(...y);

/** : ('y => 'z) => (...'x => 'y) => ...'x => 'z */
const kompose = (f) => (g) => (...x) => f(g(...x));

/** : (...'x => 'y) => ('y => 'z) => ...'x => 'z */
const seq = (f) => (g) => (...x) => g(f(...x));

const compose = (...fa) => {
    const l = fa.length;
    if (l == 0) { return identity; }
    else if (l == 1) { return fa[0]; }
    else if (l == 2) { return kompose(fa[0])(fa[1]); }
    else { const f = l.pop(); return kompose(compose(...fa))(f);};}

// UNTESTED! Combinators for CPS functions
// type Not(...'a) = (...'a) => 'r
// type Kont(...'a) = Not(Not(...'a))

/** call a direct function from CPS
    Kont.arrow
    : (...'a => 'b) => ...'a => Kont('b) */
const arrowK = (f) => (...x) => (k) => k(f(...x));

/** Kont.pure
    : ...'a => Kont(...'a) */
const identityK = (...x) => (k) => k(...x);

/** : ...'a => ...'b => Kont(...'a) */
const konstantK = (...x) => (...y) => (k) => k(...x);

/** : (...'z => 'y => Kont('x)) => (...'z => Kont('y)) => ...'z => Kont('x) */
const smeltK = (x) => (y) => (...z) => (k) => x(...z)((xz) => y(...z)(seq(xz)(k)));

/** : (...'z => ...'y => Kont('x)) => ...'y => ...'z => Kont('x) */
const flipK = (x) => (...y) => (...z) => (k) => x(...z)((xz) => xz(...y)(k));

/** : (...'y => Kont(...'z)) => (...'x => Kont(...'y)) => ...'x => Kont(...'z) */
const komposeK = (f) => (g) => (...x) => (k) => g(...x)((...y) => f(...y)(k));

/** : (...'x => Kont(...'y)) => (...'y => Kont(...'z)) => ...'x => Kont(...'z) */
const seqK = (f) => (g) => komposeK(g)(f);

const composeK = (...fa) => {
    const l = fa.length;
    if (l == 0) { return identityK; }
    else if (l == 1) { return fa[0]; }
    else if (l == 2) { return komposeK(fa[0])(fa[1]); }
    else { const f = l.pop(); return komposeK(composeK(...fa))(f);};}


// Unit conversion
const weiPerEth = web3.toBigNumber(1e18);
const ethToWei = (e) => web3.toBigNumber(e).mul(weiPerEth).floor();
const weiToEth = (w) => web3.toBigNumber(w).div(weiPerEth);

const digest = web3.sha3;
const digestHex = (x) => web3.sha3(x, {encoding: "hex"});

/** : Uint8 => string */
const byteToHex = (byte) => ('0' + (byte & 0xFF).toString(16)).slice(-2);

/** : Uint8Array => string */
const bytesToHex = (bytes) => Array.from(bytes, byteToHex).join('');

/** : string => String0x */
const hexTo0x = (hex) => "0x" + hex;

/** : Uint8Array => String0x */
const bytesTo0x = (bytes) => hexTo0x(bytesToHex(bytes));

/** Strip the 0x prefix
    : String0x => string */
const un0x = (s) => {
    console.assert(s.slice(0,2) == "0x");
    return s.slice(2);}

/** Prepend "0x" to a hex string
    : string => String0x */
const hexToAddress = (hex) => hexTo0x(hex.slice(-40));

/** Convert a hex string to a BigNumber
    : string => BigNumber */
const hexToBigNumber = (hex) => web3.toBigNumber(hexTo0x(hex));

/** Return a random salt
    : () => String0x */
const randomSalt = () => {
    const array = new Uint8Array(32);
    window.crypto.getRandomValues(array);
    return bytesTo0x(array);}

/** : (Array('a), 'a) => Array('a) */
const snoc = (l, e) => [...l, e];

const logErrorK = (error) => (k) => {console.log("error: ", error); return k();}

const handlerK = (successK = identityK, errorK = logErrorK) => (error, result) =>
      error ? errorK(error) : successK(result);

const handlerThenK = (successK = identityK, errorK = logErrorK) => (k) =>
      handlerK(seqK(successK, k), seqK(errorK, k));

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

// Local Storage for the DApp, localized to the current Ethereum user.
// TODO: use remote storage and implement distributed transactions, for redundancy.
/** : string */
let userID;
const keyValuePair = (key, value) => { let o = {}; o[key] = value; return o; }
const getStorage = (key, default_ = null) => JSON.parse(window.localStorage.getItem(key)) || default_;
const putStorage = (key, value) => window.localStorage.setItem(key, JSON.stringify(value));
const updateStorage = (key, update) => putStorage(key, {...getStorage(key), ...update});
const userKey = (key) => `${userID}.${key}`;
const getUserStorage = (key, default_ = null) => getStorage(userKey(key), default_);
const putUserStorage = (key, value) => putStorage(userKey(key), value);
const updateUserStorage = (key, update) => updateStorage(userKey(key), update);
const putUserStorageField = (key, field, value) => updateUserStorage(key, keyValuePair(field, value));


// General Purpose Ethereum Blockchain Watcher
/** : int */
let nextUnprocessedBlock;

/** : StringTable(K(int)) */
const confirmedHooks = {};

/** : StringTable(K(int)) */
const unconfirmedHooks = {};

/** : ('a => Kont()) => Array('a) => Kont() */
const forEachK = (f) => (l) => (k) => {
    const loop = () => {
        if (l.length == 0) {
            return k();
        } else if (l.length == 1) {
            return f(l[0])(k);
        } else {
            return f(l.shift())(loop);
        };};
    return loop(); }

const compareFirst = (a, b) => a[0].localeCompare(b[0]);

// Have a parallel variant?
/** : StringTable(Not(...'a)) => ...'a => Kont() */
const runHooks = (hooks) => (...args) => (k) =>
    forEachK((entry) => entry[1](...args))(Object.entries(hooks).sort(compareFirst))(k);

const log = (result) => {console.log("logging: ", JSON.stringify(result)); return result;};

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

/** Debugging stuff */
let r;
const setr = (result) => {r = result; return log(r); };
const setrr = seq(Array.of)(setr);
const setrk = (result) => (k) => k(setr(result));
const setrrk = seq(Array.of)(setrk);
const srf = (func) => {r = undefined; return func(setr);}
const srrf = seqK(Array.of)(srf);
