// web3 client for rock-paper-scissors.
/*
  TODO:
  * Get the contract to work at all.

  * Track games:
    * Always compute both player0 and player1 actions.
    * Always compute both confirmed and unconfirmed status.
    * Display the unconfirmed if different from confirmed.
    * Offer user to decide based on the unconfirmed, but only act when confirmed.

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

const rock = 0;
const paper = 1;
const scissors = 2;
const validateHand = (x) => Number.isInteger(x) && (x == 0 || x == 1 || x == 2);

// web3.utils.soliditySha3({t: 'bytes32', value: salt}, {t: 'uint8', value: hand}); // This web3 1.0 function is NOT AVAILABLE IN METAMASK! So we do things manually.
const makeCommitment = (salt, hand) => digestHex(salt + byteToHex(hand));

// (bytes32_0xString, Uint8, address, BN, BN) => ((address) => `a) => `a
const player0StartGame =
      (salt, hand, player1Address, timeoutInBlocks, wagerInWei, escrowInWei) => (k) => {
    const commitment = makeCommitment(salt, hand);
    const totalAmount = wagerInWei.add(escrowInWei);
    return ethQuery(rpsFactory.player0_start_game)
    (player1Address, timeoutInBlocks, commitment, wagerInWei, {value: totalAmount})(k);
}

// (address, BN, Uint8) => (() => `a) => `a
const player1ShowHand = (contractAddress, wagerInWei, hand) => (k) => {
    const rps = web3.eth.contract(rpsAbi).at(contractAddress);
    return ethQuery(rps.player1_show_hand)(hand, {value: wagerInWei})((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

// (address, Uint8Array(32), Uint8) => (() => `a) => `a
const player0Reveal = (contractAddress, salt, hand) => (k) => {
    const rps = web3.eth.contract(rpsAbi).at(contractAddress);
    return ethQuery(rps.player0_reveal)(salt, hand, {})((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

// (address) => (() => `a) => `a
const player0Rescind = (contractAddress) => (k) => {
    const rps = web3.eth.contract(rpsAbi).at(contractAddress);
    return ethQuery(rps.player0_rescind)()((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

// (address) => (() => `a) => `a
const player1WinByDefault = (contractAddress) => (k) => {
    const rps = web3.eth.contract(rpsAbi).at(contractAddress);
    return ethQuery(rps.player1_win_by_default().send)()((txHash) =>
    confirmEtherTransaction(txHash)(k)) };

const decodeState = (x) => {
    let [state, outcome, timeoutInBlocks, previousBlock, player0, player1, player0Commitment, wagerInWei, escrowInWei, salt, hand0, hand1] = x;
    state = state.toNumber();
    outcome = outcome.toNumber();
    timeoutInBlocks = timeoutInBlocks.toNumber();
    previousBlock = previousBlock.toNumber();
    hand0 = hand0.toNumber();
    hand1 = hand1.toNumber();
    return {state, outcome, timeoutInBlocks, previousBlock, player0, player1, player0Commitment, wagerInWei, escrowInWei, salt, hand0, hand1};};

// (address) => {state: Uint8, outcome: Uint8, timeoutInBlocks: int, previousBlock: int, player0: address, player1: address, player0Commitment: bytes32, wagerInWei: BN, escrowInWei: BN, salt: bytes32, hand0: Uint8, hand1: Uint8} => `a) => `a
const queryState = (contractAddress, blockNumber) => (k) => {
    const rps = web3.eth.contract(rpsAbi).at(contractAddress);
    return ethQuery(rps.query_state.call)({}, blockNumber)((x) => k(decodeState(x)))};

// (int => `a) => `a
const queryConfirmedState = (contractAddress) => (k) =>
    confirmedBlockNumber((blockNumber) => queryState(contractAddress, blockNumber)(k));

const decodeGameCreationData = (data, blockNumber) => {
    const x = (i) => data.slice(2+i*64,66+i*64);
    const contract = hexToAddress(x(0));
    const player0 = hexToAddress(x(1));
    const player1 = hexToAddress(x(2));
    const timeoutInBlocks = hexToBigNumber(x(3)).toNumber();
    const commitment = hexTo0x(x(4));
    const wagerInWei = hexToBigNumber(x(5));
    const escrowInWei = hexToBigNumber(x(6));
    return {contract, player0, player1, timeoutInBlocks,
            commitment, wagerInWei, escrowInWei, blockNumber};}


// TODO: better error handling
// (txHash) => {contract: address, player0: address, player1: address, timeoutInBlocks: integer,
// commitment: bytes32, wagerInWei: BN, escrowInWei: BN, blockNumber: integer}
const getGameCreationData = (txHash) => (k) => {
    ethQuery(web3.eth.getTransactionReceipt)(txHash)((receipt) => {
    const result = decodeGameCreationData(receipt.logs[0].data, receipt.blockNumber);
    if(receipt.transactionHash == txHash
       && receipt.status == "0x1"
       && receipt.from == result.player0
       && receipt.to == config.contract.address
       && receipt.logs.length == 1) {
        return k(result);
    } else {
        console.log("bad rps game creation data receipt", txHash, receipt, result)
        return k(false);
    }})};

const processNewGameK = (event) => (k) => {
    return loggingK("newEvent:")(event)(k); }

const watchNewGames = (k) =>
    registerConfirmedEventHook(
        "confirmedNewGames",
        fromBlock = config.contract.creationBlock, // TODO: only from 2 timeouts in the past(?)
        filter = {address: config.contract.address},
        processNewGameK)(k);

const initBackend = (k) => {
    rpsFactory = web3.eth.contract(rpsFactoryAbi).at(config.contract.address);
    return k();
}


/** Test games */
var gsalt = "0x30f6cb71704ee3321c0bb552120a492ab2406098f5a89b0a765155f4f5dd9124";
var alice = "0x60B8193167c5355779B4d03cA37A0C11807e667f";
var bob = "0xa8c6677434CDb243E9f19Cca387a916487da8C2f";
var meth = web3.toBigNumber(1e15).mul;

var play0 = () => srf(player0StartGame(gsalt, 2, alice, meth(1000), meth(100)));
var c0 = () => srf(computeCommitment(gsalt, 2));

var g0tx = "0xbc75c759901430f54694ff2ae4888e9b772c15f6364320ccbcab5cfbc9f870c3";
var g0v0 = () => srf(getGameCreationData(g0tx));
var g0c = "0xd933e31efb452bfcc6f993e578946aafd6aa75d0"
var g0p1 = () => srf(player1ShowHand(g0c, meth(1000), 0));
var g0p2 = () => srf(player1WinByDefault(g0c));
var g0s = () => srf(queryConfirmedState(g1c));

var g1c = "0x21a63393301aa265ea7fbde8a7d43ea96a40f08f";
var g1p1 = () => srf(player1ShowHand(g1c, meth(1000), 0));
var g1p2 = () => srf(player0Reveal(g1c, gsalt, 2));
var g1s = () => srf(queryConfirmedState(g1c));

var wb = () => {
    newBlockHooks["newBlock"] = (from, to) => (k) => {
        console.log("newBlock! from:", from, "to:", to); return k(); };
    return watchBlockchain(); }
