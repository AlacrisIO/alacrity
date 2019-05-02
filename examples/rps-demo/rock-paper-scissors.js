// web3 client for rock-paper-scissors.
// NB: we use web3 for posting transactions, even though it's unreliable.
// In the future, we should post transactions only after persisting locally
// and carefully play the auction game so as to post transactions without paying too much in fees.

'use strict';

let web3;

if (typeof window.web3 !== 'undefined') {
    web3 = window.web3;
} else {
    alert('You need a Web3-compatible browser. Consider downloading the MetaMask extension.');
}

// Output of solc --abi rock-paper-scissors.sol
const rockPaperScissorsFactoryAddress = "0xY...Z";
const rockPaperScissorsFactoryAbi =
      // const rockPaperScissorsFactoryAbi = require('./rockPaperScissorsFactoryAbi.json');
      [{"constant":false,"inputs":[{"name":"_commitment","type":"bytes32"},{"name":"_player1_address","type":"address"},{"name":"_wager_amount","type":"uint256"}],"name":"createRockPaperScissors","outputs":[{"name":"","type":"address"}],"payable":true,"stateMutability":"payable","type":"function"},{"anonymous":false,"inputs":[{"indexed":false,"name":"_contract","type":"address"},{"indexed":false,"name":"_player0","type":"address"},{"indexed":false,"name":"_player1","type":"address"},{"indexed":false,"name":"_commitment","type":"bytes32"},{"indexed":false,"name":"_wager_amount","type":"uint256"},{"indexed":false,"name":"_escrow_amount","type":"uint256"}],"name":"Created","type":"event"}];

const rockPaperScissorsAbi =
      // const rockPaperScissorsAbi = require('./rockPaperScissorsAbi.json');
      [{"constant":false,"inputs":[{"name":"salt","type":"bytes32"},{"name":"hand0","type":"uint8"}],"name":"player0_reveal","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":true,"inputs":[],"name":"query_state","outputs":[{"name":"","type":"uint8"},{"name":"","type":"uint8"},{"name":"","type":"uint256"},{"name":"","type":"address"},{"name":"","type":"address"},{"name":"","type":"bytes32"},{"name":"","type":"uint256"},{"name":"","type":"uint256"},{"name":"","type":"uint8"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_hand1","type":"uint8"}],"name":"player1_show_hand","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[],"name":"player1_win_by_default","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[],"name":"player0_rescind","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[{"name":"_commitment","type":"bytes32"},{"name":"_player1_address","type":"address"},{"name":"_wager_amount","type":"uint256"}],"payable":true,"stateMutability":"payable","type":"constructor"}];

const BN = web3.toBigNumber;
const digest = web3.sha3;
const rock = 0;
const paper = 1;
const scissors = 2;

// (Uint8) => string
const byteToHex = (byte) => {
    return ('0' + (byte & 0xFF).toString(16)).slice(-2);
}
// (Uint8Array) => string
function bytesToHex(bytes) {
    return Array.from(bytes, byteToHex).join('');
}
// (Uint8Array) => string
function bytesTo0x (bytes) {
    return "0x" + bytesToHex(bytes);
}

// () => Uint8Array(32)
const random_salt = () => {
    const array = new Uint8Array(32);
    window.crypto.getRandomValues(array);
    return array;
}

const snoc = (l, e) => l.concat([e]);

const eth_query = (f) => (...args) => (k) =>
    f.apply(snoc(args,(error, result) => error ? console.log(error) : k(result)));

// With inspiration from https://medium.com/pixelpoint/track-blockchain-transactions-like-a-boss-with-web3-js-c149045ca9bf
// The string must be in "0x..." format.
// (string) => int
const getConfirmations = (txHash) => (k) =>
    eth_query(web3.eth.getTransaction)(txHash)((txInfo) => // Get TxInfo
    eth_query(web3.eth.getBlockNumber)()((currentBlock) => // Get current block number
    // When transaction is unconfirmed, its block number is null.
    // In this case we return -1 as number of confirmations
    k(tx.blockNumber === null ? -1 : currentBlock - txInfo.blockNumber)));

/** configuration
    confirmations_wanted_in_blocks: Number of confirmations (in blocks) wanted
    to consider a transaction done.
    1 is only good enough for toys / tests / demos.
    12 is what metamask uses by default for regular transactions.
    100 or more should be used for serious / large transactions, even more on ETC.

    block_polling_period_in_seconds: How often to poll the chain for results.
*/
const config = {
    confirmations_wanted_in_blocks: 1,
    block_polling_period_in_seconds: 5 };

const confirmEtherTransaction = (txHash, confirmations = config.confirmations_wanted_in_blocks) => (k) =>
    eth_query(getConfirmations)(txHash)((txConfirmations) =>
    (txConfirmations >= confirmations) ? k() :
    setTimeout((() => confirmEtherTransaction(txHash, confirmations)(k)),
              config.block_polling_period_in_seconds * 1000));

// (Uint8Array(32), Uint8, address, BN, BN) => ((address) => `a) => `a
const createRockPaperScissors = (salt, hand, player1_address, wagerAmount, escrowAmount) => (k) => {
    const commitment = web3.utils.soliditySha3(
        {t: 'bytes32', value: bytesTo0x(salt)},
        {t: 'uint8', value: hand});
    const totalAmount = wagerAmount.add(escrowAmount);
    eth_query(rockPaperScissorsFactory.methods
              .createRockPaperScissors(commitment, player1_address, wagerAmount).send)
    ({value: totalAmount})((txHash) =>
    confirmEtherTransaction(txHash)(() =>
    eth_query(web3.eth.getTransactionReceipt)(txHash)((receipt) =>
    k(receipt.contractAddress)))) };

// (address, BN, Uint8) => (() => `a) => `a
const player1_show_hand = (contractAddress, wagerAmount, hand) => (k) => {
    const rockPaperScissors = web3.eth.contract(rockPaperScissorsAbi).at(contractAddress);
    eth_query(rockPaperScissors.methods.player1_show_hand(hand).send)({value: wagerAmount})((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

// (address, Uint8Array(32), Uint8) => (() => `a) => `a
const player0_reveal = (contractAddress, salt, hand) => (k) => {
    const rockPaperScissors = web3.eth.contract(rockPaperScissorsAbi).at(contractAddress);
    eth_query(rockPaperScissors.methods.player0_reveal(bytesTo0x(salt),hand).send)
    ({value: wagerAmount})((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

// (address) => (() => `a) => `a
const player0_rescind = (contractAddress) => (k) => {
    const rockPaperScissors = web3.eth.contract(rockPaperScissorsAbi).at(contractAddress);
    eth_query(rockPaperScissors.methods.player0_rescind().send)()((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

// (address) => (() => `a) => `a
const player1_win_by_default = (contractAddress) => (k) => {
    const rockPaperScissors = web3.eth.contract(rockPaperScissorsAbi).at(contractAddress);
    eth_query(rockPaperScissors.methods.player1_win_by_default().send)()((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

// state, outcome, previous_block, player0_address, player1_address, player0_commitment, wager_amount, escrow_amount, hand1
// (address) => ((Uint8, Uint8, int, address, address, bytes32, BN, BN, uint8) => `a) => `a
const queryState = (contractAddress, blockNumber) => (k) => {
    const rockPaperScissors = web3.eth.contract(rockPaperScissorsAbi).at(contractAddress);
    eth_query(rockPaperScissors.methods.player1_win_by_default().call)({}, blockNumber)(k.apply); };

// (int => `a) => `a
const confirmedBlockNumber = (k) =>
    eth_query(web3.eth.getBlockNumber)()((currentBlock) => // Get current block number
    k(currentBlock-config.confirmations_wanted_in_blocks));

const queryConfirmedState = (contractAddress) => (k) =>
    confirmedBlockNumber((blockNumber) => queryState(contractAddress, blockNumber)(k));

window.alacrisRps = {
    rock,
    paper,
    scissors,
    random_salt,
    byteToHex,
    bytesToHex,
    bytesTo0x,
    snoc,
    eth_query,
    getConfirmations,
    config,
    confirmEtherTransaction,
    createRockPaperScissors,
    player1_show_hand,
    player0_reveal,
    player0_rescind,
    player1_win_by_default,
    queryState,
    confirmedBlockNumber,
    queryConfirmedState
}
