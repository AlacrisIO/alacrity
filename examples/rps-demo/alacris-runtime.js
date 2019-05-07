'use strict';

// TODO: when using web3 1.0, use web3.eth.net.getId().then(k) or whatever is exported,
// to also extract the chainID, which can notably distinguish between ETH and ETC
// (both networkID 1, the latter chainID 61)
// () => string
const getNetworkID = () => web3.currentProvider.networkVersion;

// () => address
const getUserAddress = () => web3.currentProvider.selectedAddress;


/** The networkConfig is an object that maps the networkID (TODO: in the future, plus chain ID?)
    to the config below. It is the responsibility of the DApp developers to define networkConfig
    as part of their DApp deployment, typically in a dapp-config.js file loaded from the HTML page.
 */
var networkConfig;

/** DApp initialization should instantiate the config configuration.

    contract: data about the contract, an object with the fields address (an address as a 0xString),
    creationHash (a digest as a 0xString), and creationBlock (a regular integer),
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
var config;

const weiPerEth = web3.toBigNumber(1e18);
const ethToWei = (e) => web3.toBigNumber(e).mul(weiPerEth).floor();
const weiToEth = (w) => web3.toBigNumber(w).div(weiPerEth);

const digest = web3.sha3;
const digestHex = (x) => web3.sha3(x, {encoding: "hex"});

// (Uint8) => string
const byteToHex = (byte) => ('0' + (byte & 0xFF).toString(16)).slice(-2);

// (Uint8Array) => string
const bytesToHex = (bytes) => Array.from(bytes, byteToHex).join('');

// (string) => string
const hexTo0x = (hex) => "0x" + hex;

// (Uint8Array) => string
const bytesTo0x = (bytes) => hexTo0x(bytesToHex(bytes));

// Strip the 0x prefix
// string => string
const un0x = (s) => {
    console.assert(s.slice(0,2) == "0x");
    return s.slice(2);
}

// Prepend "0x" to a hex string
// (string) => string
const hexToAddress = (hex) => hexTo0x(hex.slice(-40));

// Convert a hex string to a BigNumber
// (string) => BigNumber
const hexToBigNumber = (hex) => web3.toBigNumber(hexTo0x(hex));

// Return a random salt as a hex string in 0x format
// () => string
const randomSalt = () => {
    const array = new Uint8Array(32);
    window.crypto.getRandomValues(array);
    return bytesTo0x(array);
}

// (Array(`a), `a) => Array(`a)
const snoc = (l, e) => l.concat([e]);

// (`...a, (error, `b) => `c)) => (`b => `c) => `c
const ethQuery = (func) => (...args) => (k) =>
      func.apply(null, snoc(args, ((error, result) => error ? console.log(error) : k(result))));

// With inspiration from https://medium.com/pixelpoint/track-blockchain-transactions-like-a-boss-with-web3-js-c149045ca9bf
// The string must be in "0x..." format.
// (string) => int
const getConfirmations = (txHash) => (k) =>
    ethQuery(web3.eth.getTransaction)(txHash)((txInfo) => // Get TxInfo
    ethQuery(web3.eth.getBlockNumber)()((currentBlock) => // Get current block number
    // When transaction is unconfirmed, its block number is null.
    // In this case we return -1 as number of confirmations
    k(tx.blockNumber === null ? -1 : currentBlock - txInfo.blockNumber)));

const confirmEtherTransaction = (txHash, confirmations = config.confirmationsWantedInBlocks) => (k) =>
    ethQuery(getConfirmations)(txHash)((txConfirmations) =>
    (txConfirmations >= confirmations) ? k() :
    setTimeout((() => confirmEtherTransaction(txHash, confirmations)(k)),
              config.blockPollingPeriodInSeconds * 1000));

// (int => `a) => `a
const confirmedBlockNumber = (k) =>
    ethQuery(web3.eth.getBlockNumber)()((currentBlock) => // Get current block number
    k(currentBlock - config.confirmationsWantedInBlocks));

// Local Storage for the DApp, localized to the current Ethereum user.
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
var latestBlockProcessed;
const confirmedHooks = {};
const unconfirmedHooks = {};

// Have a parallel variant?
const runHooks = (hooks, args, k) => {
    hookList = Object.entries(hooks).sort((a, b) => a[0].localeCompare(b[0]));
    const loop = () => {
        if (list.length == 0) {
            k();
        } else {
            return hookList.shift().apply(null, [...args, loop]);
        };};
    return loop(); }

const watchChain = () => {
    ethQuery(web3.eth.getBlockNumber)()((currentBlock) => { // TODO: handle error, too
    const again = () => {
        lastBlockProcessed = currentBlock;
        putUserStorage("lastBlockProcessed", lastBlockProcessed);
        return setTimeout(watchChain, config.blockPollingPeriodInSeconds * 1000);
    };
    if (currentBlock > lastBlockProcessed) {
        const oldConfirmed = lastBlockProcessed - config.confirmationsWantedInBlocks;
        const newConfirmed = current - config.confirmationsWantedInBlocks;
        return runHooks(confirmedHooks, [oldconfirmed, newConfirmed], () =>
        runHooks(unconfirmedHooks, [lastBlockProcessed, currentBlock], again));
    } else {
        return again();
    }})};

const initRuntime = (k) => {
    const networkID = getNetworkID();
    config = networkConfig[networkID];
    userID = `${networkID}.${getUserAddress()}`;
    latestBlockProcessed = getUserStorage("latestBlockProcessed", 0);
    return k();
}


/** Debugging stuff */
var r;
var setr = (result) => {r = result; console.log("result: ", r.toString()); return r;};
var setrr = (...results) => {setr(results);};
var srf = (func) => {r = undefined; return func(setr);}

const deployContract = (code) => srf(ethQuery(web3.eth.sendTransaction)({data: code}));
