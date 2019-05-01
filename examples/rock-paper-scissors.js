// web3 client for rock-paper-scissors.
// NB: we use web3 for posting transactions, even though it's unreliable.
// In the future, we should post transactions only after persisting locally
// and carefully play the auction game so as to post transactions without paying too much in fees.

const Web3 = require('web3');


// Output of solc --abi rock-paper-scissors.sol
const rockPaperScissorsFactoryAddress = "0xY...Z";
const rockPaperScissorsFactoryAbi =
      // const rockPaperScissorsFactoryAbi = require('./rockPaperScissorsFactoryAbi.json');
      [{"constant":false,"inputs":[{"name":"_commitment","type":"bytes32"},{"name":"_player1_address","type":"address"},{"name":"_wager_amount","type":"uint256"}],"name":"createRockPaperScissors","outputs":[],"payable":true,"stateMutability":"payable","type":"function"}];
const rockPaperScissorsFactory = web3.eth.contract(rockPaperScissorsFactoryAbi).at(rockPaperScissorsFactoryAddress);

let rockPaperScissorsAbi =
      // const rockPaperScissorsAbi = require('./rockPaperScissorsAbi.json');
    [{"constant":false,"inputs":[{"name":"salt","type":"bytes32"},{"name":"hand0","type":"uint8"}],"name":"player0_reveal","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":true,"inputs":[],"name":"query_state","outputs":[{"name":"","type":"uint8"},{"name":"","type":"uint8"},{"name":"","type":"uint256"},{"name":"","type":"address"},{"name":"","type":"address"},{"name":"","type":"bytes32"},{"name":"","type":"uint256"},{"name":"","type":"uint256"},{"name":"","type":"uint8"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_hand1","type":"uint8"}],"name":"player1_show_hand","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[],"name":"player1_win_by_default","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[],"name":"player0_rescind","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[{"name":"_commitment","type":"bytes32"},{"name":"_player1_address","type":"address"},{"name":"_wager_amount","type":"uint256"}],"payable":true,"stateMutability":"payable","type":"constructor"}];

const BN = web3.utils.BN;
const digest = web3.utils.keccak256;
const rock = 0;
const paper = 1;
const scissors = 2;

// () => Uint8Array(32)
function random_salt () {
    let array = new Uint8Array(32);
    window.crypto.getRandomValues(array);
    return array;
}

// (Uint8) => string
function byteToHex(byte) {
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

// With inspiration from https://medium.com/pixelpoint/track-blockchain-transactions-like-a-boss-with-web3-js-c149045ca9bf
// The string must be in "0x..." format.
// (string) => int
async function getConfirmations(txHash) {
    try {
        // Get transaction details
        const tx = await web3.eth.getTransaction(txHash);

        // Get current block number
        const currentBlock = await web3.eth.getBlockNumber();

        // When transaction is unconfirmed, its block number is null.
        // In this case we return 0 as number of confirmations
        return tx.blockNumber === null ? Number.MIN_SAFE_INTEGER : currentBlock - tx.blockNumber;
    }
    catch (error) {
        console.log(error);
    }
}

/** Number of confirmations (in blocks) wanted to consider a transaction done.
    2 is only good enough for toys / tests / demos.
    12 is what metamask uses by default for regular transactions.
    100 or more should be used for serious / large transactions, even more on ETC.
  */
var confirmations_wanted_in_blocks = 2;
var block_polling_period_in_seconds = 10;

async function confirmEtherTransaction(txHash, confirmations = confirmations_wanted_in_blocks) {
    const txConfirmations = await getConfirmations(txHash);
    if (txConfirmations >= confirmations) {
        return;
    } else {
        setTimeout(async () => {
            return confirmEtherTransaction(txHash, confirmations, k);
        }, block_polling_period_in_seconds * 1000);
    }
}

async function createRockPaperScissors (salt, hand, player1_address, wagerAmount, escrowAmount) {
    let commitment = web3.utils.soliditySha3(
        {t: 'bytes', value: bytesTo0x(salt)},
        {t: 'uint8', value: hand});
    let totalAmount = wagerAmount.add(escrowAmount);
    let txHash = await rockPaperScissorsFactory.methods
        .createRockPaperScissors(commitment, player1_address, wagerAmount)
        .send({value: totalAmount});
    await confirmEtherTransaction(txHash);
    let receipt = await web3.eth.getTransactionReceipt(txHash);
    return receipt.contractAddress;
}

async function player1_show_hand (contractAddress, wagerAmount, hand) {
    const rockPaperScissors = web3.eth.contract(rockPaperScissorsAbi).at(contractAddress);
    let c = rockPaperScissors.
    let txHash = await rockPaperScissors.methods
        .player1_show_hand(hand)
        .send({value=wagerAmount});
    return txHash;
}

/*
async function player0_reveal (contractAddress, wagerAmount, hand) {
    const rockPaperScissors = web3.eth.contract(rockPaperScissorsAbi).at(contractAddress);
    let c = rockPaperScissors.
    let txHash = await rockPaperScissors.methods
        .player1_show_hand(hand)
        .send({value=wagerAmount});
    return txHash;
}

function player0_reveal (bytes32 salt, uint8 hand0) external payable
function player0_rescind () external payable
function player1_win_by_default () external payable
function query_state () external view returns(State, Outcome, uint, address, address, bytes32, uint, uint, uint8)
*/
