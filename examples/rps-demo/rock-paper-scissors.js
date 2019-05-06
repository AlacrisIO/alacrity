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

// TODO: when using web3 1.0, use web3.eth.net.getId().then(k) or whatever is exported,
// to also extract the chainID, which can notably distinguish between ETH and ETC
// (both networkID 1, the latter chainID 61)
// () => string
const getNetworkID = () => web3.currentProvider.networkVersion;

// () => address
const getUserAddress = () => web3.currentProvider.selectedAddress;

// Map(string, string)
const networkParameters =
  { "1": /* Frontier, Homestead, Metropolis, the Ethereum public PoW main network */
    {contract: null,
     timeout_in_blocks: 11520,
     timeout_string: "about 48 hours"},
    "4": /* Rinkeby, the public Geth-only PoA testnet */
    {contract: "0x73e900e2f6b69a62a78cea31736f536d4076532b",
     timeout_in_blocks: 10,
     timeout_string: "about 2.5 minutes"},
    /* Rinkeby */ "4":
      {contract: "0x73e900e2f6b69a62a78cea31736f536d4076532b",
       timeout_in_blocks: 10,
       timeout_string: "about 2.5 minutes"},
// contract 0x73e900e2f6b69a62a78cea31736f536d4076532b deployed on Rinkeby, created by transaction 0x2a37d19da8523b09ea507220874229f02c7d4ed20af9d977cf41b1239b0f6e2a, with user-selectable timeout, from source at commit 2bb0e1977b57939e57b80a4d0aacc3cad8b4bc60
// Older contracts:
// contract 0xa526caaabf917554bb49756409759fb0101e2ccd deployed on Rinkeby, created by transaction 0xf43b8c50018ec9a819e1022d9b6a7c07218a1a24e19260b6ca375d1cd34f6ec3, with 10 block timeout (very short!), from source at commit 68ceb3cfd3dbacf88c1df4152307572a357bc80e
// contract 0x07326e7ec3935b4ff8411c08fa94deb0ad1c7e98 deployed on Rinkeby, created by transaction 0x61a83184580f8bc3d5bc077578c7533073dbf4eff7cde23fe30795e3c1ccd766, with 10 block timeout (very short!), from source at commit 68ceb3cfd3dbacf88c1df4152307572a357bc80e, except with reverse winner logic - oops
// contract 0xe7de0191a6f9683dd3f2011214e5533d00d604fe deployed on Rinkeby, created by transaction 0x47632009af8c66df628449f1a422d86aa3fd21d96ed93e050ca752bfcec2af93, with 11520 block timeout, from source at commit 99631383c862bea336afcc6bfbd615fd425834f9
  };

var rockPaperScissorsFactoryAddress;
var rockPaperScissorsFactory;
var timeout_in_blocks;
var timeout_string;

// Output of solc --abi rock-paper-scissors.sol
const rockPaperScissorsFactoryAbi =
      // const rockPaperScissorsFactoryAbi = require('./_build/rockPaperScissorsFactory.abi');
      [{"constant":true,"inputs":[{"name":"_salt","type":"bytes32"},{"name":"_hand0","type":"uint8"}],"name":"compute_commitment_digest","outputs":[{"name":"_digest","type":"bytes32"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":true,"inputs":[{"name":"_salt","type":"bytes32"},{"name":"_hand0","type":"uint8"}],"name":"compute_commitment_message","outputs":[{"name":"_message","type":"bytes"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":false,"inputs":[{"name":"_player1","type":"address"},{"name":"_timeout_in_blocks","type":"uint256"},{"name":"_commitment","type":"bytes32"},{"name":"_wager_amount","type":"uint256"}],"name":"player0_start_game","outputs":[{"name":"_contract","type":"address"}],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"_salt","type":"bytes32"},{"name":"_hand0","type":"uint8"}],"name":"compute_commitment","outputs":[{"name":"_digest","type":"bytes32"},{"name":"_message","type":"bytes"}],"payable":false,"stateMutability":"nonpayable","type":"function"},{"anonymous":false,"inputs":[{"indexed":false,"name":"_contract","type":"address"},{"indexed":false,"name":"_player0","type":"address"},{"indexed":false,"name":"_player1","type":"address"},{"indexed":false,"name":"_timeout_in_blocks","type":"uint256"},{"indexed":false,"name":"_commitment","type":"bytes32"},{"indexed":false,"name":"_wager_amount","type":"uint256"},{"indexed":false,"name":"_escrow_amount","type":"uint256"}],"name":"Created","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"_hash","type":"bytes32"},{"indexed":false,"name":"_message","type":"bytes"}],"name":"Commitment","type":"event"}];

const rockPaperScissorsAbi =
      // const rockPaperScissorsAbi = require('./_build/rockPaperScissors.abi');
      [{"constant":false,"inputs":[{"name":"salt","type":"bytes32"},{"name":"hand0","type":"uint8"}],"name":"player0_reveal","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":true,"inputs":[],"name":"query_state","outputs":[{"name":"_state","type":"uint8"},{"name":"_outcome","type":"uint8"},{"name":"_timeout_in_blocks","type":"uint256"},{"name":"_previous_block","type":"uint256"},{"name":"_player0","type":"address"},{"name":"_player1","type":"address"},{"name":"_player0_commitment","type":"bytes32"},{"name":"_wager_amount","type":"uint256"},{"name":"_escrow_amount","type":"uint256"},{"name":"_hand1","type":"uint8"}],"payable":false,"stateMutability":"view","type":"function"},{"constant":false,"inputs":[{"name":"_hand1","type":"uint8"}],"name":"player1_show_hand","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[],"name":"player1_win_by_default","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[],"name":"player0_rescind","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[{"name":"_player0","type":"address"},{"name":"_player1","type":"address"},{"name":"_timeout_in_blocks","type":"uint256"},{"name":"_commitment","type":"bytes32"},{"name":"_wager_amount","type":"uint256"}],"payable":true,"stateMutability":"payable","type":"constructor"}];

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

// state, outcome, timeout_in_blocks, previous_block, player0_address, player1_address, player0_commitment, wager_amount, escrow_amount, hand1
// (address) => (Tuple(Uint8, Uint8, uint, uint, address, address, bytes32, BN, BN, uint8) => `a) => `a
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

// contractAddress, player0_address, player1_address, timeout_in_blocks, commitment, wager_amount, escrow_amount
// (txHash) => [address, address, address, BN, bytes32, BN, BN]
const getRockPaperScissorsCreationData = (txHash) => (k) => {
    ethQuery(web3.eth.getTransactionReceipt)(txHash)((receipt) => {
    let result = decodeRpsCreationData(receipt.logs[0].data);
    let [contractAddress, player0_address, player1_address,
         timeout_in_blocks, commitment, wager_amount, escrow_amount] = result;
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

const initBackend = (k) => {
    const parameters = networkParameters[getNetworkID()];
    rockPaperScissorsFactoryAddress = parameters.contract;
    timeout_in_blocks = parameters.timeout_in_blocks;
    timeout_string = parameters.timeout_string;
    rockPaperScissorsFactory = web3.eth.contract(rockPaperScissorsFactoryAbi).at(rockPaperScissorsFactoryAddress);
    return k();
}

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
var r;
var setr = (result) => {r = result; console.log("result: ", r.toString()); return r;};
var setrr = (...results) => {setr(results);};
var srf = (func) => {r = undefined; return func(setr);}

/* Code for the RockPaperScissorsFactory */
var rpsfCode = "0x608060405234801561001057600080fd5b50611101806100206000396000f3fe60806040526004361061005c576000357c01000000000000000000000000000000000000000000000000000000009004806318a55908146100615780631ced222f146100bd5780639051c3a51461017e57806390d188ba14610220575b600080fd5b34801561006d57600080fd5b506100a76004803603604081101561008457600080fd5b8101908080359060200190929190803560ff1690602001909291905050506102e8565b6040518082815260200191505060405180910390f35b3480156100c957600080fd5b50610103600480360360408110156100e057600080fd5b8101908080359060200190929190803560ff169060200190929190505050610303565b6040518080602001828103825283818151815260200191508051906020019080838360005b83811015610143578082015181840152602081019050610128565b50505050905090810190601f1680156101705780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b6101de6004803603608081101561019457600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190803590602001909291908035906020019092919050505061035d565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b34801561022c57600080fd5b506102666004803603604081101561024357600080fd5b8101908080359060200190929190803560ff169060200190929190505050610505565b6040518083815260200180602001828103825283818151815260200191508051906020019080838360005b838110156102ac578082015181840152602081019050610291565b50505050905090810190601f1680156102d95780820380516001836020036101000a031916815260200191505b50935050505060405180910390f35b60006102f48383610303565b80519060200120905092915050565b60608282604051602001808381526020018260ff1660ff167f010000000000000000000000000000000000000000000000000000000000000002815260010192505050604051602081830303815290604052905092915050565b60008034338787878761036e6105d5565b808673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001848152602001838152602001828152602001955050505050506040518091039082f080158015610407573d6000803e3d6000fd5b50905090507f0c013fc1b773695e4db147b67ee73e6f7c8294360666826d7785e3a396130168813388888888893403604051808873ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200185815260200184815260200183815260200182815260200197505050505050505060405180910390a180915050949350505050565b60006060806105148585610303565b90506000818051906020012090507f73034e83d747faa6cfa133a69463d722cc49c74f74d8b569c917db2ce314ace181836040518083815260200180602001828103825283818151815260200191508051906020019080838360005b8381101561058b578082015181840152602081019050610570565b50505050905090810190601f1680156105b85780820380516001836020036101000a031916815260200191505b50935050505060405180910390a180829350935050509250929050565b604051610af0806105e68339019056fe608060405260008060006101000a81548160ff0219169083600381111561002257fe5b021790555060405160a080610af0833981018060405260a081101561004657600080fd5b8101908080519060200190929190805190602001909291908051906020019092919080519060200190929190805190602001909291905050506000600381111561008c57fe5b6000809054906101000a900460ff1660038111156100a657fe5b1415156100b257600080fd5b80341115156100c057600080fd5b80600681905550600654340360078190555084600360006002811015156100e357fe5b0160006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550836003600160028110151561013357fe5b0160006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550826001819055508160058190555060016000806101000a81548160ff0219169083600381111561019e57fe5b0217905550436002819055505050505050610932806101be6000396000f3fe608060405260043610610067576000357c01000000000000000000000000000000000000000000000000000000009004806301b1f3461461006c578063670eefd1146100a75780639fbe35ee1461018b578063ba2f3181146101bc578063d2af80e3146101c6575b600080fd5b6100a56004803603604081101561008257600080fd5b8101908080359060200190929190803560ff1690602001909291905050506101d0565b005b3480156100b357600080fd5b506100bc6103a5565b604051808b60038111156100cc57fe5b60ff1681526020018a60058111156100e057fe5b60ff1681526020018981526020018881526020018773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018581526020018481526020018381526020018260ff1660ff1681526020019a505050505050505050505060405180910390f35b6101ba600480360360208110156101a157600080fd5b81019080803560ff16906020019092919050505061047c565b005b6101c4610521565b005b6101ce6105bf565b005b600260038111156101dd57fe5b6000809054906101000a900460ff1660038111156101f757fe5b14151561020357600080fd5b61020b61065a565b60038160ff1610801561027557508181604051602001808381526020018260ff1660ff167f01000000000000000000000000000000000000000000000000000000000000000281526001019250505060405160208183030381529060405280519060200120600554145b151561028057600080fd5b60006003826003600860009054906101000a900460ff16010360ff168115156102a557fe5b06905060018160ff1614156102ef576002600860016101000a81548160ff021916908360058111156102d357fe5b02179055506102ea600754600654600202016106c7565b61037d565b60028160ff16141561033d576003600860016101000a81548160ff0219169083600581111561031a57fe5b021790555061032d600654600202610742565b6103386007546106c7565b61037c565b6001600860016101000a81548160ff0219169083600581111561035c57fe5b021790555061036c600654610742565b61037b600754600654016106c7565b5b5b60036000806101000a81548160ff0219169083600381111561039b57fe5b0217905550505050565b6000806000806000806000806000806000809054906101000a900460ff16600860019054906101000a900460ff16600154600254600360006002811015156103e957fe5b0160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff166003600160028110151561041b57fe5b0160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff16600554600654600754600860009054906101000a900460ff16859550849450995099509950995099509950995099509950995090919293949596979899565b6001600381111561048957fe5b6000809054906101000a900460ff1660038111156104a357fe5b1415156104af57600080fd5b6104b76107bd565b600654341415156104c757600080fd5b60038160ff161015156104d957600080fd5b80600860006101000a81548160ff021916908360ff16021790555060026000806101000a81548160ff0219169083600381111561051257fe5b02179055504360028190555050565b6002600381111561052e57fe5b6000809054906101000a900460ff16600381111561054857fe5b14151561055457600080fd5b61055c610883565b6105646108f0565b6004600860016101000a81548160ff0219169083600581111561058357fe5b021790555061059a60075460065460020201610742565b60036000806101000a81548160ff021916908360038111156105b857fe5b0217905550565b600160038111156105cc57fe5b6000809054906101000a900460ff1660038111156105e657fe5b1415156105f257600080fd5b6105fa61065a565b6106026108f0565b6005600860016101000a81548160ff0219169083600581111561062157fe5b0217905550610635600754600654016106c7565b60036000806101000a81548160ff0219169083600381111561065357fe5b0217905550565b3373ffffffffffffffffffffffffffffffffffffffff166003600060028110151561068157fe5b0160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff161415156106c557600080fd5b565b600360006002811015156106d757fe5b0160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff166108fc829081150290604051600060405180830381858888f1935050505015801561073e573d6000803e3d6000fd5b5050565b6003600160028110151561075257fe5b0160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff166108fc829081150290604051600060405180830381858888f193505050501580156107b9573d6000803e3d6000fd5b5050565b600073ffffffffffffffffffffffffffffffffffffffff16600360016002811015156107e557fe5b0160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16141561087857336003600160028110151561083457fe5b0160006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550610881565b610880610883565b5b565b3373ffffffffffffffffffffffffffffffffffffffff16600360016002811015156108aa57fe5b0160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff161415156108ee57600080fd5b565b600154600254014311151561090457600080fd5b56fea165627a7a72305820eed34ac9a5d1bda10d879d2c7a318a1e583e10184805c8cac626d85d6f5a62730029a165627a7a72305820ed7e59928ff0676c50417552944d5d6600c39edc4dd4590009954af21bbe54d40029";
var deploy = () => srf(ethQuery(web3.eth.sendTransaction)({data: rpsfCode}));


/** Test games */
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
