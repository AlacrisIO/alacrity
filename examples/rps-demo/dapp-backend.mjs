/** web3 client backend for rock-paper-scissors. */
/*
  DONE:

  * All the messaging TO the blockchain, including creating a new game.
  * Follow the factory contract and collect its events.
  * Filter the events to only consider games where the user is either player0 or player1,
    or where player1 is open (0)
  * If the user is player0, try to match with games in our local storage,
    by given details.
  * Now register the game, if not already registered.
  * Have a hook for (re)displaying a contract purely based on its localStorage state,
    and have the frontend get into that hook (will skip the game if already dismissed).
    Call the hook any time a change is detected in a game.
  * For every registered game that isn't complete,
    have a loop that polls for changes in game confirmed state
    (or, alternatively, also look at unconfirmed changes).
    We're polling for recomputed state at every block.
  * Handle timeouts
  * If the user is player0 and the txHash is not be already known, do something useful
    (due to race condition with the browser crashing before localStorage was done, or use in another browser)
  * Use a post-frontend-initialization hook to only start expensive computations
    after the frontend is initialized.
  * Handle the case where an open game was accepted by someone else already...
    special display, and make it dismissable.
  * Instead of polling for contract state all the time, we could have the contract emit
    events at state transitions, and recompute the state from them.
    That would be cheaper to compute on the client and much lighter on the node, though
    it is involves more code to write. It might not cost more gas in the contract,
    since emitting events and requiring users to resubmit relevant state as arguments
    might be cheaper than storing state.

  TODO FOR DEMO:

  * Move to the .ala file the things from which that are in this file
    that should be provided by the high-level programmer, such as the human-readable descriptions.

  TODO LATER MAYBE:

  * Refactor to extract a clean systematic interface between frontend and backend for user input.
  * Improve discoverability so users don't have to care too much about being player0 or player1.
  * Survive spamming of the chain by games to DoS clients that would run out of memory:
    Only check for new games in recent blocks; cap the number of open games;
    require a small fee for opening a game?
*/
import {byteToHex, registerInit, errbacK, kLogError, hexToAddress, hexTo0x, checkRequirement, setrr,
        loggedAlert, merge, flip, logErrorK, randomSalt, logging
       } from "./common-utils.mjs";
import {web3, crypto, userAddress} from "./web3-prelude.mjs";
import {saltedDigest, registerBackendHooks, renderGame, config, getConfirmedBlockNumber,
        toBN, optionalAddressOf0x, optionalAddressMatches, hexToBigNumber, deployContract,
        getGame, updateGame, removeActiveGame, queueGame, attemptGameCreation, optionalAddressTo0x,
        isGameConfirmed, digestHex
       } from "./common-runtime.mjs";
import {renderWei} from "./common-ui.mjs";
import {rpsAbi, rpsFactoryAbi, rpsFactoryCode} from "./build/dapp-contract.mjs";

// The contract object, to be fulfilled from config after initialization.
let rpsFactory;

const rpsContract = web3.eth.contract(rpsAbi);
const rps = contractAddress => rpsContract.at(contractAddress);

/// TYPES INVOLVED

// The things below should be automatically generated from the type declaration,
// plus optionally a few trivial deriving macros.
export const Hand = Object.freeze({Rock: 0, Paper: 1, Scissors: 2});
export const handName = hand => ["Rock", "Paper", "Scissors"][hand];
export const isValidHand = x => Number.isInteger(x) && (x == 0 || x == 1 || x == 2);
export const randomHand = () => {
    const array = new Uint8Array(6);
    crypto.getRandomValues(array);
    return (array[0]+array[1]+array[2]+array[3]+array[4]+array[5]) % 3; // NB: 256 % 3 == 1
};
export const handToHex = byteToHex;

// (salt, hand) => web3.utils.soliditySha3({t: 'bytes32', value: salt}, {t: 'uint8', value: hand});
// The above web3 1.0 function is NOT AVAILABLE IN METAMASK, that only has web3 0.2!
export const makeCommitment = saltedDigest(handToHex);

export const Outcome = Object.freeze({
    Player1Wins: 0,
    Draw: 1,
    Player0Wins: 2,
    Player1WinByDefault: 3,
    Player0Rescinds: 4
    });
export const outcomeOfHands = (hand0, hand1) => (hand0 + 4 - hand1) % 3

export const State = Object.freeze({
    WaitingForPlayer1: 0,        // player0 funded wager+escrow and published a commitment
    WaitingForPlayer0Reveal: 1,  // player1 showed his hand
    Completed: 2                 // end of game (in the future, have a way to reset the contract to some Uninitialized state to repeat plays?)
});


// SENDING DATA TO THE BLOCKCHAIN
// The section below is automatically generated as FFI at the same time as the contract is generated.

// TODO: decode into two parts, the part already known to the creator,
// and the part generated by the blockchain (e.g. contractAddress, blockNumber, txHash, etc.)?
// Thus we can match in a straightforward way using the former, and add the latter if needed?
//
// Decode Game Creation Data, found in event logs as posted by the contract.
// To be generated from the event type declaration, possibly with multiple cases.
export const decodeGameCreationEvent = (data, blockNumber, txHash) => {
    const x = i => data.slice(2+i*64,66+i*64);
    const contract = hexToAddress(x(0));
    const player0 = hexToAddress(x(1));
    const player1 = optionalAddressOf0x(hexToAddress(x(2)));
    const timeoutInBlocks = hexToBigNumber(x(3)).toNumber();
    const player0Commitment = hexTo0x(x(4));
    const wagerInWei = hexToBigNumber(x(5));
    const escrowInWei = hexToBigNumber(x(6));
    return {contract, player0, player1, timeoutInBlocks, player0Commitment, wagerInWei, escrowInWei,
            previousBlock: blockNumber, state: State.WaitingForPlayer1, player1filter: player1,
            blockNumber, txHash}}

export const MsgType = Object.freeze({
    Player0StarGame: 0,
    Player1ShowHand: 1,
    Player0Reveal: 2,
    Player0Rescind: 3,
    Player1WinByDefault: 4
});

export const decodeGameEvent = event => {
    const topic = event.topics[0];
    const data = event.data;
    const blockNumber = event.blockNumber;
    const txHash = event.transactionHash;
    const x = i => data.slice(2+i*64,66+i*64);
    if (topic == topics.Player1ShowHand) {
        return {msgType: MsgType.Player1ShowHand,
                player1: hexToAddress(x(0)),
                hand1: hexToBigNumber(x(1)).toNumber(),
                blockNumber, txHash}
    } else if (topic == topics.Player0Reveal) {
        return {msgType: MsgType.Player0Reveal,
                salt: hexTo0x(x(0)),
                hand0: hexToBigNumber(x(1)).toNumber(),
                outcome: hexToBigNumber(x(2)).toNumber(),
                blockNumber, txHash}
    } else if (topic == topics.Player0Rescind) {
        return {msgType: MsgType.Player0Rescind,
                blockNumber, txHash}
    } else if (topic == topics.Player1WinByDefault) {
        return {msgType: MsgType.Player1WinByDefault,
                blockNumber, txHash}}
    loggedAlert(`Unrecognized topic ${JSON.stringify({topic, data, blockNumber, txHash})}`);}

// NB: None of these checkRequirement's is useful if we trust the contract.
// NB: if we are doing speculative execution of unconfirmed messages, though,
// we may still check them to avoid a switcheroo attack, whereby the adversary
// sends a transaction to create a contract, then gets a different contract confirmed,
// but you reply to the first contract.
export const player1ShowHand = (g, msg, player1, hand1) => {
    checkRequirement(g.state == State.WaitingForPlayer1,
                     () => ["Event received in incorrect state", msg, g]);
    checkRequirement(web3.isAddress(player1) && optionalAddressMatches(g.player1, player1),
                     () => ["Invalid player1 address in event", msg, g]);
    checkRequirement(isValidHand(hand1),
                     () => ["Invalid hand1 in event", msg, g]);
    // should we check the value, too, as extracted from the tx receipt???
    return merge({player1, hand1,
                  previousBlock: msg.blockNumber, state: State.WaitingForPlayer0Reveal})(g);}

export const player0Reveal = (g, msg, salt, hand0, outcome) => {
    checkRequirement(g.state == State.WaitingForPlayer0Reveal,
                     () => ["Event received in incorrect state", msg, g]);
    checkRequirement(isValidHand(hand0),
                     () => ["Invalid hand0 in event", msg]);
    checkRequirement(!g.salt || salt == g.salt,
                     () => ["Not the salt we knew!"]);
    checkRequirement(!g.hand0 || hand0 == g.hand0,
                     () => ["Not the hand we knew!"]);
    checkRequirement(makeCommitment(salt, hand0) == g.player0Commitment,
                     () => ["commitments do not match", msg, g]);
    checkRequirement(outcome == outcomeOfHands(hand0, g.hand1),
                     () => ["unexpected outcome", msg, g]);
    return merge({salt, hand0, outcome,
                  previousBlock: msg.blockNumber, state: State.Completed, isCompleted: true})(g)}

export const checkTimeout = (g, msg) =>
    checkRequirement(msg.blockNumber > g.previousBlock + g.timeoutInBlocks,
                    () => "Over-early timeout");

export const player0Rescind = (g, msg) => {
    checkRequirement(g.state == State.WaitingForPlayer1,
                    () => "Invalid state");
    checkTimeout(g, msg);
    // TODO: also check that the contract did distribute the funds as it should have?
    return merge({outcome: Outcome.Player0Rescinds,
                  previousBlock: msg.blockNumber, state: State.Completed, isCompleted: true})(g);}

export const player1WinByDefault = (g, msg) => {
    checkRequirement(g.state == State.WaitingForPlayer0Reveal,
                    () => "Invalid state");
    checkTimeout(g, msg);
    // TODO: also check that the contract did distribute the funds as it should have?
    return merge({outcome: Outcome.Player1WinByDefault,
                  previousBlock: msg.blockNumber, state: State.Completed, isCompleted: true})(g);}

export const stateUpdate = (state, event) => {
    switch (event.msgType) {
    case MsgType.Player1ShowHand:
        return player1ShowHand(state, event, event.player1, event.hand1);
    case MsgType.Player0Reveal:
        return player0Reveal(state, event, event.salt, event.hand0, event.outcome);
    case MsgType.Player0Rescind:
        return player0Rescind(state, event);
    case MsgType.Player1WinByDefault:
        return player1WinByDefault(state, event)}}

// RECEIVING DATA FROM THE BLOCKCHAIN

// Given game data (from a decoded game creation event) and game (from local user storage),
// determine if the data matches the game.
// This can be produced automatically by the compiler, together with emitting events of the proper type,
// when creating a game (with or without its own contract).
export const gameMatches = (d, g) =>
    g &&
    d.player0 == g.player0 &&
    d.player1 == g.player1 &&
    d.timeoutInBlocks == g.timeoutInBlocks &&
    d.player0Commitment == g.player0Commitment &&
    d.wagerInWei == g.wagerInWei &&
    d.escrowInWei == g.escrowInWei;

// Do we need this to automatically deal with player sets?
// const gamePlayers = game => {const {player0, player1} = game;return [player0, player1]}

// We probably don't need this function, though maybe it will help our code generator
// to have separate sub-data-structures, so as to avoid name clashes;
// alternatively, we could have hierarchical-variable-names?
// And/or we have syntax objects, for which we generate distinct variable names,
// and we know that some of those objects are mapped to fixed variable names in the backend;
// and we know to not abuse gensym suffixes to avoid clashes?
//
// const gameParameters = game => {
//    const {player0, player1, timeoutinBlocks, player0Commitment, wagerInWei, escrowInWei} = game;
//    return {player0, player1, timeoutinBlocks, player0Commitment, wagerInWei, escrowInWei}}

// This can be generated by the contract from the place of user addresses in its events.
export const isGameRelevantToUser = (game, userAddress) =>
    game.player0 == userAddress || optionalAddressMatches(game.player1, userAddress)
export const isGameInitiator = (game, userAddress) => game.player0 == userAddress;


// Frontend functions
// --- whenever we sign a transaction that isn't the result of an interaction with the frontend,
// we first need to explain why in an alert, and we must provide a predictably named hook
// for the frontend to provide this explanation. [Or the user must provide the secret key
// of an account with enough gas to pay, after having independently signed the messages
// with the main key, and the contract must support this kind of usage; this is not how
// Ethereum usually works, but that doesn't mean we shouldn't do it --- decouple
// message authentication and gas provision.]
export let player0RevealContext;


/** Process a game, making all automated responses that do not require user input.
    This is perhaps the heart of the algorithm.
 */
// TODO: are we triggering a renderGame here when something changes, or somewhere else?
export const processGameAtHook = confirmedBlock => id => k => {
    // TODO: move the beginning of this function to a common file...
    const game = getGame(id);
    // logging("processGameAtHook", id, game)();
    if (game.state == State.Completed) { // Game already completed, nothing to do.
        updateGame(id, {isCompleted: true});
        removeActiveGame(id);
        return k();}
    if (game.player0 == userAddress &&
        game.state == State.WaitingForPlayer0Reveal &&
        !game.player0RevealTxHash) {
        const salt = game.salt;
        const hand0 = game.hand0;
        const hand1 = game.hand1;
        const context = player0RevealContext(id, hand0, hand1, game.wagerInWei, game.escrowInWei);
        if (salt && isValidHand(hand0)) {
            loggedAlert(`${context} Please sign the following transaction.`);
            return errbacK(rps(game.contract).player0_reveal)(salt, hand0, {})(
                txHash => {
                    updateGame(id, {player0RevealTxHash: txHash})
                    // Register txHash for confirmation? Nah, we're just polling for state change!
                    // But if we switch to event-tracking, that's where it would happen.
                    return k();},
                error => {loggedAlert(error); return k();})
        } else {
            loggedAlert(`${context} However, you do not have the salt and hand data in this client.
Be sure to start a client that has this data before the deadline.`);}} // TODO: print the deadline!
    const timeoutBlock = game.previousBlock + game.timeoutInBlocks;
    if (confirmedBlock < timeoutBlock) {
        // We haven't yet confirmed that future blocks will be > previous + timeout
        // So add the current game to the queue, if it wasn't added yet.
        queueGame(id, timeoutBlock);
        return k();}
    if (game.player0 == userAddress &&
        game.state == State.WaitingForPlayer1) {
        if (game.player0RescindTxHash) {
            return k();}
        const stakeInWei = toBN(game.wagerInWei).add(game.escrowInWei);
        loggedAlert(`Player1 timed out in game ${id},
sending a transaction to recover your stake of ${renderWei(stakeInWei)}`);
        // TODO register the event, don't send twice.
        return errbacK(rps(game.contract).player0_rescind)()(
            txHash => { updateGame(id, { player0RescindTxHash: txHash }); return k(); },
            error => { loggedAlert(error); return k()})}
    if (game.player1 == userAddress &&
        game.state == State.WaitingForPlayer0Reveal &&
        !game.player1WinByDefaultTxHash) {
        const stakeInWei = toBN(game.wagerInWei).add(game.escrowInWei);
        loggedAlert(`Player0 timed out in game ${id},
sending a transaction to recover your ${renderWei(game.wagerInWei)} wager
and their ${renderWei(stakeInWei)} stake`);
        return errbacK(rps(game.contract).player1_win_by_default)({})(
            txHash => {
                updateGame(id, {player1WinByDefaultTxHash: txHash});
                return k()},
            flip(logErrorK)(k))}
    return k()}

export const createNewGame = (wagerInWei, escrowInWei, player1, hand0) => {
    const salt = randomSalt();
    const player0Commitment = makeCommitment(salt, hand0);
    const player0 = userAddress;
    const timeoutInBlocks = config.timeoutInBlocks;
    const totalAmount = toBN(wagerInWei).add(escrowInWei);
    // TODO: add the ID to the contract call for tracking purpose? Use the low bits of the escrow?
    // Or the high bits of the hand? No, use the commitment and the rest of the data.
    // Somehow when we restart transactions, we match them by content
    // and the salted commitment ought to provide enough unicity.
    // We could use the nonce for the transaction, but there's no atomic access to it.
    // Could we save the TxHash locally *before* sending it online? Unhappily web3 doesn't allow that:
    // < https://github.com/MetaMask/metamask-extension/issues/3475 >.
    return attemptGameCreation(
        {salt, hand0, player0Commitment, player0, player1, timeoutInBlocks, wagerInWei, escrowInWei})(
        rpsFactory.player0_start_game)(
        optionalAddressTo0x(player1), timeoutInBlocks, player0Commitment, wagerInWei,
            {value: totalAmount})}

/** Accept a game of given id, playing given hand.
    Assumes the game is waiting for player1 and we're authorized.
    The alerts also suppose some manual annotation on some relatively low-level code.
 */
export const acceptGame = (id, hand1) => {
    const game = getGame(id);
    // This test can be generated from a generic pattern on joining games.
    if (!isGameConfirmed(game)) {
        // If that's the case, make a transaction that we only send later? No, we can't with web3.
        loggedAlert(`Game ${id} isn't confirmed yet`);
        return;}
    // This test can be generated from the state machine,
    // but generating the text of the alert requires more cleverness or human intervention.
    // Some more generic text might do "You are trying to ... but this action is not available
    // at the currently confirmed state of the game (... click to show state graph...),
    // which instead expects ..."
    // and/or since this is about joining a game, some more specialized message could be available.
    if (game.state != State.WaitingForPlayer1) {
        loggedAlert(`Game ${id} isn't open to a wager`);
        return;}
    // Since this is about joining a game, and this could be automatically generated from a pattern
    if (!optionalAddressMatches(game.player1, userAddress)) {
        loggedAlert(`Game ${id} isn't open to you`);
        return;}
    if (game.player1ShowHandTxHash) {
        loggedAlert(`You already played ${game.hand1} on game ${id} in tx ${game.player1ShowHandTxHash}`);
        return;}
    updateGame(id, {hand1});
    return errbacK(rps(game.contract).player1_show_hand)(hand1, {value: game.wagerInWei})(
        txHash => {
            updateGame(id, {player1ShowHandTxHash: txHash});
            renderGame(id, "Accept Game:"); },
        loggedAlert);}

export const topics = {}

const initBackend = k => {
    if (config && config.contract) { // Avoid erroring on an unconfigured network
        rpsFactory = web3.eth.contract(rpsFactoryAbi).at(config.contract.address);
        if (digestHex(rpsFactoryCode) !== config.contract.codeHash) {
            logging(`Warning: deployed contract has code hash ${config.contract.codeHash} \
but the latest version of the contract has code hash ${digestHex(rpsFactoryCode)}`)();}}
    topics.Created = rpsFactory.Created().options.topics[0];
    //topics.Player0StartGame = rps().Player0StartGame().options.topics[0];
    topics.Player1ShowHand = rps().Player1ShowHand().options.topics[0];
    topics.Player0Reveal = rps().Player0Reveal().options.topics[0];
    topics.Player0Rescind = rps().Player0Rescind().options.topics[0];
    topics.Player1WinByDefault = rps().Player1WinByDefault().options.topics[0];
    return k()}

export const registerRpsHooks = hooks => {
    player0RevealContext = hooks.player0RevealContext
}

registerBackendHooks({
    processGameAtHook, decodeGameCreationEvent, decodeGameEvent,
    gameMatches, isGameRelevantToUser, isGameInitiator, stateUpdate})

registerInit({
    Backend: {fun: initBackend, dependsOn: ["Runtime"]}})

export const deployRps = () => deployContract(rpsFactoryCode)(setrr)

// Local Variables:
// mode: JavaScript
// End:
