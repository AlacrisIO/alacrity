'use strict';

const rps = window.RockPaperScissors;

/** State is a list of games.
 * Each game has a state:
   */

const userID = `${getNetworkID()}.${getUserAddress()}`;

const getStorage = (key, default_ = null) => JSON.parse(window.localStorage.getItem(key)) || default_;
const putStorage = (key, value) => window.localStorage.setItem(key, JSON.stringify(value));
const getUserStorage = (key, default_ = null) => getStorage(`${userID}.${key}`, default_);
const putUserStorage = (key, value) => putStorage(`${userID}.${key}`, value);

let activeGamesById;
let activeGamesByTxHash = {};
let nextId;

const setNodeBySelector = (selector, content) => {
    const node = document.querySelector(selector);
    node.innerHTML = '';
    node.appendChild(content);
}

const simple_number_regex = "[0-9]+([.][0-9]+)?|[.][0-9]+";
const address_regex = "0x[0-9A-Fa-f]{40}";

// TODO: offer easy standard amounts for the amount
// TODO: let the users negotiate the escrow.
// TODO: determine a minimum acceptable escrow, and suggest that?
// TODO: determine a minimum amount based on the minimum escrow?
const renderWager = (amount) => {
    const common = 'style="${font-size: 10pt}" name="wager"';
    return `
    <label style="text-align: center;">
       Wager amount${amount ? "" : " (plus automatic 10% escrow)"}:
       <br>
       ${amount ? `<output >${amount}</output>` :
         `<input style="${common}" pattern="${simple_number_regex}"
           type="number" min=".01" step=".01" value="1" required />`}
    </label>`;};

// TODO: have a greyed out message "default: anyone" in the input style
// TODO: support a list of known opponents, and giving nicknames to known opponents
// TODO: add known-partner and nickname support to MetaMask (?)
const renderOpponent = (opponent) => {
    const common='style="font-size: 10pt;" name="opponent"';
    return `
    <label style="text-align: center;">
        Opponent address: <br />
        ${opponent ?
            `<output ${common}">${opponent}</output>` :
            `<input ${common}" pattern="${address_regex}" size="50">`}</label>`;};

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
        <legend>Choose your hand (default: random)</legend>
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
const renderGameChoice = (id, wager, opponent) => `
        ${renderWager(wager)}
        <br>
        ${renderOpponent(opponent)}
        <br>
        ${renderHandChoice()}
        <br>
        <button style="width: 100%;">Shoot!</button>
        NB: Using a timeout of ${timeout_in_blocks} blocks (${timeout_string}).
    `;

const renderNewGame = (id) => {
    const el = document.createElement('form');
    el.innerHTML = renderGameChoice (id, null, null);
    el.addEventListener('submit', submitNewGame);
    setNodeBySelector("#NewGame", el);
};

const restartGame = (node, id) => {
    // XXX TODO
}

const renderActiveGames = () => {
    const node = document.createElement('div');
    if (activeGamesById) {
        for(id in activeGamesById) { restartGame(node, id); }
    } else {
        node.innerHTML = "<p>(No currently active game)</p>";
    }
    setNodeBySelector("#ActiveGames", node);
}

// TODO: way to download the localState
// TODO: way to use a remote replicated backup service for encrypted state management.
const initFrontend = () => {
    setNodeBySelector("#Prerequisites", document.createTextNode(""));
    nextId = getUserStorage("nextId") || 1000000000;
    activeGamesById = getUserStorage("activeGamesById");
    renderNewGame(nextId);
    renderActiveGames();
}
