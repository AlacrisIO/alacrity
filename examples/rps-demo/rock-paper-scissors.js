// web3 client for rock-paper-scissors.
/*
  TODO:
  * Get it to work at all.

  * We should be using window.localStorage to persist the state of computations across reloads,
    and in the future, maybe something similar but with remote replicas.
    https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage

  * We use web3 for posting transactions, even though it's unreliable.
    In the future, we should post transactions only after persisting locally,
    and carefully play the auction game so as to post transactions without paying too much in fees.
    In other words, we should be using the legilogic_ethereum library,
    compiled from OCaml to JS using bucklescript.

  * Improve discoverability so users don't have to care too much about being player0 or player1.

  * On frontend, have a list of common playing partners, with short aliases.
*/

'use strict';

// Contracts to use:
// contract 0xe7de0191a6f9683dd3f2011214e5533d00d604fe deployed on Rinkeby, created by transaction 0x47632009af8c66df628449f1a422d86aa3fd21d96ed93e050ca752bfcec2af93, with 11520 block timeout, from source at commit 99631383c862bea336afcc6bfbd615fd425834f9
// contract 0x07326e7ec3935b4ff8411c08fa94deb0ad1c7e98 deployed on Rinkeby, created by transaction 0x61a83184580f8bc3d5bc077578c7533073dbf4eff7cde23fe30795e3c1ccd766, with 10 block timeout (very short!), from source at commit 68ceb3cfd3dbacf88c1df4152307572a357bc80e, except with reverse winner logic - oops
// contract 0xa526caaabf917554bb49756409759fb0101e2ccd deployed on Rinkeby, created by transaction 0xf43b8c50018ec9a819e1022d9b6a7c07218a1a24e19260b6ca375d1cd34f6ec3, with 10 block timeout (very short!), from source at commit 68ceb3cfd3dbacf88c1df4152307572a357bc80e

const rockPaperScissorsFactoryAddress = "0xa526caaabf917554bb49756409759fb0101e2ccd";

// Output of solc --abi rock-paper-scissors.sol
const rockPaperScissorsFactoryAbi =
      // const rockPaperScissorsFactoryAbi = require('./_build/rockPaperScissorsFactory.abi');
      [{"constant":true,"inputs":[{"name":"_salt","type":"bytes32"},{"name":"_hand0","type":"uint8"}],"name":"compute_commitment_digest","outputs":[{"name":"_digest","type":"bytes32"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":true,"inputs":[{"name":"_salt","type":"bytes32"},{"name":"_hand0","type":"uint8"}],"name":"compute_commitment_message","outputs":[{"name":"_message","type":"bytes"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":false,"inputs":[{"name":"_player1_address","type":"address"},{"name":"_commitment","type":"bytes32"},{"name":"_wager_amount","type":"uint256"}],"name":"player0_start_game","outputs":[{"name":"_contract_address","type":"address"}],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"_salt","type":"bytes32"},{"name":"_hand0","type":"uint8"}],"name":"compute_commitment","outputs":[{"name":"_digest","type":"bytes32"},{"name":"_message","type":"bytes"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"anonymous":false,"inputs":[{"indexed":false,"name":"_contract","type":"address"},{"indexed":false,"name":"_player0","type":"address"},{"indexed":false,"name":"_player1","type":"address"},{"indexed":false,"name":"_commitment","type":"bytes32"},{"indexed":false,"name":"_wager_amount","type":"uint256"},{"indexed":false,"name":"_escrow_amount","type":"uint256"}],"name":"Created","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"_hash","type":"bytes32"},{"indexed":false,"name":"_message","type":"bytes"}],"name":"Commitment","type":"event"}];

const rockPaperScissorsAbi =
      // const rockPaperScissorsAbi = require('./_build/rockPaperScissors.abi');
      [{"constant":false,"inputs":[{"name":"salt","type":"bytes32"},{"name":"hand0","type":"uint8"}],"name":"player0_reveal","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":true,"inputs":[],"name":"query_state","outputs":[{"name":"_state","type":"uint8"},{"name":"_outcome","type":"uint8"},{"name":"_previous_block","type":"uint256"},{"name":"_player0_address","type":"address"},{"name":"_player1_address","type":"address"},{"name":"_player0_commitment","type":"bytes32"},{"name":"_wager_amount","type":"uint256"},{"name":"_escrow_amount","type":"uint256"},{"name":"_hand1","type":"uint8"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_hand1","type":"uint8"}],"name":"player1_show_hand","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[],"name":"player1_win_by_default","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[],"name":"player0_rescind","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[{"name":"_player0_address","type":"address"},{"name":"_player1_address","type":"address"},{"name":"_commitment","type":"bytes32"},{"name":"_wager_amount","type":"uint256"}],"payable":true,"stateMutability":"payable","type":"constructor"}];

const rockPaperScissorsFactory = web3.eth.contract(rockPaperScissorsFactoryAbi).at(rockPaperScissorsFactoryAddress);

const digest = web3.sha3;
const digest_hex = (x) => web3.sha3(x, {encoding: "hex"});
const rock = 0;
const paper = 1;
const scissors = 2;

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
    ethQuery(getConfirmations)(txHash)((txConfirmations) =>
    (txConfirmations >= confirmations) ? k() :
    setTimeout((() => confirmEtherTransaction(txHash, confirmations)(k)),
              config.block_polling_period_in_seconds * 1000));

// (bytes32_0x_string, Uint8, address, BN, BN) => ((address) => `a) => `a
const player0StartGame = (salt, hand, player1_address, wagerAmount, escrowAmount) => (k) => {
    const commitment =
          // This web3 1.0 function is NOT AVAILABLE IN METAMASK! web3.utils.soliditySha3({t: 'bytes32', value: salt}, {t: 'uint8', value: hand});
          digest_hex(salt + byteToHex(hand));
    const totalAmount = wagerAmount.add(escrowAmount);
    return ethQuery(rockPaperScissorsFactory.player0_start_game)
    (player1_address, commitment, wagerAmount, {value: totalAmount})((txHash) =>
    confirmEtherTransaction(txHash)(() =>
    ethQuery(web3.eth.getTransactionReceipt)(txHash)((receipt) =>
    k(receipt.contractAddress)))) };

// (address, BN, Uint8) => (() => `a) => `a
const player1ShowHand = (contractAddress, wagerAmount, hand) => (k) => {
    const rockPaperScissors = web3.eth.contract(rockPaperScissorsAbi).at(contractAddress);
    return ethQuery(rockPaperScissors.player1_show_hand)(hand, {value: wagerAmount})((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

// (address, Uint8Array(32), Uint8) => (() => `a) => `a
const player0Reveal = (contractAddress, salt, hand) => (k) => {
    const rockPaperScissors = web3.eth.contract(rockPaperScissorsAbi).at(contractAddress);
    return ethQuery(rockPaperScissors.player0_reveal)(salt, hand, {})((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

// (address) => (() => `a) => `a
const player0Rescind = (contractAddress) => (k) => {
    const rockPaperScissors = web3.eth.contract(rockPaperScissorsAbi).at(contractAddress);
    return ethQuery(rockPaperScissors.player0_rescind)()((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

// (address) => (() => `a) => `a
const player1WinByDefault = (contractAddress) => (k) => {
    const rockPaperScissors = web3.eth.contract(rockPaperScissorsAbi).at(contractAddress);
    return ethQuery(rockPaperScissors.player1_win_by_default().send)()((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

// state, outcome, previous_block, player0_address, player1_address, player0_commitment, wager_amount, escrow_amount, hand1
// (address) => (Tuple(Uint8, Uint8, int, address, address, bytes32, BN, BN, uint8) => `a) => `a
const queryState = (contractAddress, blockNumber) => (k) => {
    const rockPaperScissors = web3.eth.contract(rockPaperScissorsAbi).at(contractAddress);
    return ethQuery(rockPaperScissors.query_state.call)({}, blockNumber)(k); };

// (int => `a) => `a
const confirmedBlockNumber = (k) =>
    ethQuery(web3.eth.getBlockNumber)()((currentBlock) => // Get current block number
    k(currentBlock-config.confirmations_wanted_in_blocks));

// (int => `a) => `a
const queryConfirmedState = (contractAddress) => (k) =>
    confirmedBlockNumber((blockNumber) => queryState(contractAddress, blockNumber)(k));

// () => string
const getNetworkID = () => web3.currentProvider.networkVersion;

// () => address
const getUserAddress = () => web3.currentProvider.selectedAddress;

// Prepend "0x" to a hex string
// (string) => string
const hexToAddress = (hex) => hexTo0x(hex.slice(-40));

// Convert a hex string to a BigNumber
// (string) => BigNumber
const hexToBigNumber = (hex) => web3.toBigNumber(hexTo0x(hex));

// contractAddress, player0_address, player0_address, commitment, wager_amount, escrow_amount
// string => address, address, address, bytes32, BN, BN
const decodeRpsCreationData = (data) => {
    let x = (i) => data.slice(2+i*64,66+i*64);
    return [hexToAddress(x(0)), hexToAddress(x(1)), hexToAddress(x(2)),
            hexTo0x(x(3)), hexToBigNumber(x(4)), hexToBigNumber(x(5))];};

// contractAddress, player0_address, player0_address, commitment, wager_amount, escrow_amount
// (txHash) => address, address, address, bytes32, BN, BN
const getRockPaperScissorsCreationData = (txHash) => (k) => {
    ethQuery(web3.eth.getTransactionReceipt)(txHash)((receipt) => {
    let result = decodeRpsCreationData(receipt.logs[0].data);
    let [contractAddress, player0_address, player1_address,
         commitment, wager_amount, escrow_amount] = result;
    if(receipt.transactionHash == txHash
       && receipt.status == "0x1"
       && receipt.from == player0_address
       && receipt.to == rockPaperScissorsFactoryAddress
       && receipt.logs.length == 1) {
        return k(result);
    } else {
        console.log("bad rps creation data receipt", txHash, receipt, result)
        return k(false);
    }})};

// (bytes32_0x_string, Uint8) => (bytes32, bytes)
const computeCommitment = (salt, hand) => (k) => {
    return ethQuery(rockPaperScissorsFactory.compute_commitment.call)
    (salt, hand, {})(k); }

// (bytes32_0x_string, Uint8) => bytes32
const computeCommitmentDigest = (salt, hand) => (k) => {
    return ethQuery(rockPaperScissorsFactory.compute_commitment_digest.call)
    (salt, hand, {})(k); }

//    confirmEtherTransaction(txHash)(() =>
//    ethQuery(web3.eth.getTransactionReceipt)(txHash)((receipt) =>
//    k(receipt.contractAddress)))) };

const exports = {
    rock,
    paper,
    scissors,
    byteToHex,
    bytesToHex,
    bytesTo0x,
    randomSalt,
    snoc,
    ethQuery,
    getConfirmations,
    config,
    confirmEtherTransaction,
    player0StartGame,
    player1ShowHand,
    player0Reveal,
    player0Rescind,
    player1WinByDefault,
    queryState,
    confirmedBlockNumber,
    queryConfirmedState
};

window.RockPaperScissors = exports;

/** Debugging stuff */
var f = function(... args) {console.log(args)};
var r; var setr = (result) => { r = result; }; var setrr = (...results) => { r = results; };
var srf = (func) => { r = undefined; func(setr); return r;}

var bytecode = "0x608060405234801561001057600080fd5b50611070806100206000396000f3fe60806040526004361061005c576000357c01000000000000000000000000000000000000000000000000000000009004806318a55908146100615780631ced222f146100bd578063286a5fdd1461017e57806390d188ba14610216575b600080fd5b34801561006d57600080fd5b506100a76004803603604081101561008457600080fd5b8101908080359060200190929190803560ff1690602001909291905050506102de565b6040518082815260200191505060405180910390f35b3480156100c957600080fd5b50610103600480360360408110156100e057600080fd5b8101908080359060200190929190803560ff1690602001909291905050506102f9565b6040518080602001828103825283818151815260200191508051906020019080838360005b83811015610143578082015181840152602081019050610128565b50505050905090810190601f1680156101705780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b6101d46004803603606081101561019457600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff1690602001909291908035906020019092919080359060200190929190505050610353565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b34801561022257600080fd5b5061025c6004803603604081101561023957600080fd5b8101908080359060200190929190803560ff1690602001909291905050506104ea565b6040518083815260200180602001828103825283818151815260200191508051906020019080838360005b838110156102a2578082015181840152602081019050610287565b50505050905090810190601f1680156102cf5780820380516001836020036101000a031916815260200191505b50935050505060405180910390f35b60006102ea83836102f9565b80519060200120905092915050565b60608282604051602001808381526020018260ff1660ff167f010000000000000000000000000000000000000000000000000000000000000002815260010192505050604051602081830303815290604052905092915050565b60008034338686866103636105ba565b808573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018381526020018281526020019450505050506040518091039082f0801580156103f5573d6000803e3d6000fd5b50905090507fbda71eb4d20ecf3ff1f2c6f12682f52fa181944682a8f781c96e574b1c17ea948133878787883403604051808773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001848152602001838152602001828152602001965050505050505060405180910390a1809150509392505050565b60006060806104f985856102f9565b90506000818051906020012090507f73034e83d747faa6cfa133a69463d722cc49c74f74d8b569c917db2ce314ace181836040518083815260200180602001828103825283818151815260200191508051906020019080838360005b83811015610570578082015181840152602081019050610555565b50505050905090810190601f16801561059d5780820380516001836020036101000a031916815260200191505b50935050505060405180910390a180829350935050509250929050565b604051610a7a806105cb8339019056fe608060405260008060006101000a81548160ff0219169083600381111561002257fe5b0217905550604051608080610a7a8339810180604052608081101561004657600080fd5b81019080805190602001909291908051906020019092919080519060200190929190805190602001909291905050506000600381111561008257fe5b6000809054906101000a900460ff16600381111561009c57fe5b1415156100a857600080fd5b80341115156100b657600080fd5b80600481905550600454340360058190555083600260006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055508160038190555082600660006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff16021790555060016000806101000a81548160ff0219169083600381111561016f57fe5b021790555043600181905550505050506108ec8061018e6000396000f3fe608060405260043610610067576000357c01000000000000000000000000000000000000000000000000000000009004806301b1f3461461006c578063670eefd1146100a75780639fbe35ee14610184578063ba2f3181146101b5578063d2af80e3146101bf575b600080fd5b6100a56004803603604081101561008257600080fd5b8101908080359060200190929190803560ff1690602001909291905050506101c9565b005b3480156100b357600080fd5b506100bc61039e565b604051808a60038111156100cc57fe5b60ff1681526020018960068111156100e057fe5b60ff1681526020018881526020018773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018581526020018481526020018381526020018260ff1660ff168152602001995050505050505050505060405180910390f35b6101b36004803603602081101561019a57600080fd5b81019080803560ff169060200190929190505050610450565b005b6101bd610536565b005b6101c76105d4565b005b600260038111156101d657fe5b6000809054906101000a900460ff1660038111156101f057fe5b1415156101fc57600080fd5b61020461066f565b60038160ff1610801561026e57508181604051602001808381526020018260ff1660ff167f01000000000000000000000000000000000000000000000000000000000000000281526001019250505060405160208183030381529060405280519060200120600354145b151561027957600080fd5b60006003826003600660149054906101000a900460ff16010360ff1681151561029e57fe5b06905060018160ff1614156102e8576002600660156101000a81548160ff021916908360068111156102cc57fe5b02179055506102e3600554600454600202016106cd565b610376565b60028160ff161415610336576003600660156101000a81548160ff0219169083600681111561031357fe5b0217905550610326600454600202610739565b6103316005546106cd565b610375565b6001600660156101000a81548160ff0219169083600681111561035557fe5b0217905550610365600454610739565b610374600554600454016106cd565b5b5b60036000806101000a81548160ff0219169083600381111561039457fe5b0217905550505050565b60008060008060008060008060008060009054906101000a900460ff16600660159054906101000a900460ff16600154600260009054906101000a900473ffffffffffffffffffffffffffffffffffffffff16600660009054906101000a900473ffffffffffffffffffffffffffffffffffffffff16600354600454600554600660149054906101000a900460ff16859550849450985098509850985098509850985098509850909192939495969798565b6001600381111561045d57fe5b6000809054906101000a900460ff16600381111561047757fe5b14151561048357600080fd5b61048b6107a5565b6004543414151561049b57600080fd5b60038160ff161015156104ad57600080fd5b33600660006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff16021790555080600660146101000a81548160ff021916908360ff16021790555060026000806101000a81548160ff0219169083600381111561052757fe5b02179055504360018190555050565b6002600381111561054357fe5b6000809054906101000a900460ff16600381111561055d57fe5b14151561056957600080fd5b61057161084d565b6105796108ab565b6004600660156101000a81548160ff0219169083600681111561059857fe5b02179055506105af60055460045460020201610739565b60036000806101000a81548160ff021916908360038111156105cd57fe5b0217905550565b600160038111156105e157fe5b6000809054906101000a900460ff1660038111156105fb57fe5b14151561060757600080fd5b61060f61066f565b6106176108ab565b6005600660156101000a81548160ff0219169083600681111561063657fe5b021790555061064a600554600454016106cd565b60036000806101000a81548160ff0219169083600381111561066857fe5b0217905550565b3373ffffffffffffffffffffffffffffffffffffffff16600260009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff161415156106cb57600080fd5b565b600260009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff166108fc829081150290604051600060405180830381858888f19350505050158015610735573d6000803e3d6000fd5b5050565b600660009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff166108fc829081150290604051600060405180830381858888f193505050501580156107a1573d6000803e3d6000fd5b5050565b600073ffffffffffffffffffffffffffffffffffffffff16600660009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1614156108425733600660006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff16021790555061084b565b61084a61084d565b5b565b3373ffffffffffffffffffffffffffffffffffffffff16600660009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff161415156108a957600080fd5b565b600a60015401431115156108be57600080fd5b56fea165627a7a723058201f45743381e608c728dd49169f8cef96b6f4f93feac32ce46052bcc0dafd07b70029a165627a7a72305820d91f6dc5e857c8fc65246d837cbab90e3ed605f802dc8b3607d226072e4e38fd0029";
var deploy = () => srf(ethQuery(web3.eth.sendTransaction)({data: bytecode}));

var gsalt = "0x30f6cb71704ee3321c0bb552120a492ab2406098f5a89b0a765155f4f5dd9124";
var alice = "0x60B8193167c5355779B4d03cA37A0C11807e667f";
var bob = "0xa8c6677434CDb243E9f19Cca387a916487da8C2f";
var meth = web3.toBigNumber(1e15).mul;

var play0 = () => srf(player0StartGame(gsalt, 2, alice, meth(1000), meth(100)));
var c0 = () => srf(computeCommitment(gsalt, 2));

var g0tx = "0xbc75c759901430f54694ff2ae4888e9b772c15f6364320ccbcab5cfbc9f870c3";
var g0v0 = () => srf(getRockPaperScissorsCreationData(g0tx));
var g0c = "0xd933e31efb452bfcc6f993e578946aafd6aa75d0"
var g0p1 = () => srf(player1ShowHand(g0c, meth(1000), 0));
var g0p2 = () => srf(player1WinByDefault(g0c));
var g0s = () => srf(queryConfirmedState(g1c));

var g1c = "0x21a63393301aa265ea7fbde8a7d43ea96a40f08f";
var g1p1 = () => srf(player1ShowHand(g1c, meth(1000), 0));
var g1p2 = () => srf(player0Reveal(g1c, gsalt, 2));
var g1s = () => srf(queryConfirmedState(g1c));
