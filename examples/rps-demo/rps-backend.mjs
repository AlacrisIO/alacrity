// Abstraction layer between manual and automatic backends

import {crypto} from "./alacrity-runtime.mjs";

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

export let MsgType, createNewGame, acceptGame, isGameRelevantToUser

export const registerRpsBackendHooks = hooks => {
    MsgType = hooks.MsgType
    createNewGame = hooks.createNewGame
    acceptGame = hooks.acceptGame
    isGameRelevantToUser = hooks.isGameRelevantToUser
}

// Frontend functions
// --- whenever we sign a transaction that isn't the result of an interaction with the frontend,
// we first need to explain why in an alert, and we must provide a predictably named hook
// for the frontend to provide this explanation. [Or the user must provide the secret key
// of an account with enough gas to pay, after having independently signed the messages
// with the main key, and the contract must support this kind of usage; this is not how
// Ethereum usually works, but that doesn't mean we shouldn't do it --- decouple
// message authentication and gas provision.]
export let player0RevealContext;

export const registerRpsFrontendHooks = hooks => {
    player0RevealContext = hooks.player0RevealContext
}



// Local Variables:
// mode: JavaScript
// End:
