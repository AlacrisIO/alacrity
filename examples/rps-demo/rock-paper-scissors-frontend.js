'use strict';

const web3 = window.web3
const rps = window.RockPaperScissors;

/** State is a list of games.
 * Each game has a state:
   */

const userID = `${getNetworkID()}.${getUserAddress()}`;

const getStorage = (key, default_ = null) => JSON.parse(window.localStorage.getItem(key)) || default_;
const putStorage = (key, value) => window.localStorage.setItem(key, JSON.stringify(value));
const getUserStorage = (key, default_ = null) => getStorage(`${userID}.${key}`, default_);
const putUserStorage = (key, value) => putStorage(`${userID}.${key}`, value);

let activeGamesById = {};
let activeGamesByTxHash = {};
let nextId = 1000000000;

const setNodeBySelector = (selector, content) => {
    const node = document.querySelector(selector);
    node.innerHTML = '';
    node.appendChild(content);
}

// TODO: offer easy standard amounts for the amount
// TODO: either warn about the 10% escrow, and/or let the user edit it.
// TODO: determine a minimum acceptable escrow, and suggest that?
const renderWager = (editable, amount) => {
    const wagerStyle = 'font-size: 18pt;';
    return `
    <label style="text-align: center;">
        Wager amount:
        <br>
        ${editable ?
            `<input style="${wagerStyle}" type="number" name="wager" required
    pattern="[0-9]+([.][0-9]+)?|[.][0-9]+" min="1">`
            :
            `<output style="${wagerStyle}" name="wager">${amount}</output>`
        }
    </label>`;};

// TODO: have a greyed out message "default: anyone" in the input style
// TODO: support a list of known opponents, and giving nicknames to known opponents
// TODO: add known-partner and nickname support to MetaMask (?)
const renderOpponent = (opponent) => {
    const common='style="font-size: 18pt;" name="opponent"';
    return `
    <label style="text-align: center;">
        Opponent:
        <br>
        ${opponent ?
            `<output ${common}">${opponent}</output>` :
            `<input ${common}" pattern="0x[0-9A-Fa-f]{40}">`}</label>`;};

// TODO: use nice icons.
const iconOfHand = (hand) => ['✊', '✋', '✌'][hand] || '';
const labelOfHand = (hand) => ['Rock', 'Paper', 'Scissors'][hand] || '';
const renderHandOption = (hand) => `
    <label style="display: inline-block; margin: .5em; text-align: center;">
        <input type="radio" value="${hand}" required name="hand">
        <div class="symbol">${iconOfHand(hand)}</div>
        ${labelOfHand(hand)}
    </label>`;
const renderHandChoice = () => `
    <fieldset style="margin-top: .25em; text-align: center;">
        <legend>Choose your hand (default: random):</legend>
        ${renderHandOption(0)}
        ${renderHandOption(1)}
        ${renderHandOption(2)}
    </fieldset>`;

const gameParametersOfForm = (e) => {
    return {
        wager: e.target.elements.wager.value,
        opponent: e.target.elements.opponent.value,
        hand: e.target.elements.hand.value
    };};

const submitNewGame = (e) => {
        e.preventDefault();
        const {wager, opponent, hand} = getFormValuesForEvent(e);
        const escrow = wager * 0.1;
        const confirmation = `You are going to start a new game ${opponent ? "with hand" : ""} for a wager of ${wager}, with an escrow of ${escrow}, and played ${labelOfHand(hand)}.`;
        window.prompt(`${confirmation}`);
        window.alert(confirmation);
    };

// TODO: let you override the random salt (option hidden by default).
const renderGameChoice = (wager, opponent) => `
        ${renderWager(wager)}
        <br>
        ${renderOpponent(opponent)}
        <br>
        ${renderHandChoice()}
        <br>
        <button style="width: 100%;">Shoot!</button>
    `;

const renderNewGame = (id) => {
    const el = document.createElement('form');
    el.innerHTML = renderGameChoice (id, null, null);
    el.addEventListener('submit', submitNewGame);
    setNodeBySelector("#NewGame", el);
};

const restartGame = (id) => {
    // XXX TODO
}

// TODO: way to download the localState
// TODO: way to use a remote replicated backup service for encrypted state management.
const init = () => {
    nextId = getUserStorage("nextId") || 1000000000;
    activeGamesById = getUserStorage("activeGamesById") || {};
    renderNewGame(nextId);
    for(id in activeGamesById) { restartGame(id); }
}


init();
