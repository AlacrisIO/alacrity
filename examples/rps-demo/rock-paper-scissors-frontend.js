'use strict';

const rps = window.RockPaperScissors;

/** State is a list of games.
 * Each game has a state:
   */

let nextID;
let activeGamesById;
const activeGamesByTxHash = {};
const activeGamesByCommitment = {};

const setNodeBySelector = (selector, content) => {
    const node = document.querySelector(selector);
    node.innerHTML = '';
    node.appendChild(content);
}

const simpleNumberRegex = "[0-9]+([.][0-9]+)?|[.][0-9]+";
const addressRegex = "0x[0-9A-Fa-f]{40}";

// TODO: offer easy standard amounts for the amount
// TODO: let the users negotiate the escrow.
// TODO: determine a minimum acceptable escrow, and suggest that?
// TODO: determine a minimum amount based on the minimum escrow?
const minWager = .01;
const defaultWager = 1;

const renderWager = (amount) => {
    const common = 'style="${font-size: 10pt}" name="wagerInEth"';
    return `
    <label style="text-align: center;">
       Wager amount in ETH${amount ? "" : " (plus automatic 10% escrow)"}:
       <br>
       ${amount ? `<output >${amount}</output>` :
         `<input style="${common}" pattern="${simpleNumberRegex}"
           type="number" min="${minWager}" step="${minWager}" value="${defaultWager}" required />`}
    </label>`;};

// TODO: have a greyed out message "default: anyone" in the input style
// TODO: support a list of known opponents, and giving nicknames to known opponents
// TODO: add known-partner and nickname support to MetaMask (?)
const renderOpponent = (opponent) => {
    const common='style="font-size: 10pt;" name="opponent"';
    return `
    <label style="text-align: center;">
        Opponent address${opponent ? "" : " (leave empty for an open game)"}: <br />
        ${opponent ?
            `<output ${common}">${opponent}</output>` :
            `<input ${common}" pattern="${addressRegex}" size="50">`}</label>`;};

// TODO: use nice icons.
const iconOfHand = (hand) => ['✊', '✋', '✌'][hand] || '';
const labelOfHand = (hand) => ['Rock', 'Paper', 'Scissors'][hand] || '';
const renderHandOption = (hand) => `
    <label style="display: inline-block; margin: .5em; text-align: center;">
        <input type="radio" value="${hand}" name="hand">
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
    // TODO: validate input, instead of having a default values!
    let wager = e.target.elements.wager.value;
    try {
        wager = web3.toBigNumber(wager);
        if (wager < minWager) { throw "foo"; }
    } catch (err) {
        throw("Invalid wager amount " + e.target.elements.wager.value);
    }
    let opponent = e.target.elements.opponent.value;
    if (opponent == "" || web3.toBigNumber(opponent).isZero()) {
        opponent = undefined;
    } else if (!web3.isAddress(opponent)) {
        throw("Invalid opponent " + e.target.elements.opponent.value);
    }
    let hand = e.target.elements.hand.value;
    if (hand == "") {
        hand = undefined;
    } else {
        hand = parseInt(hand, 10);
        if (! (hand >=0 && hand < 3)) {
            throw("Invalid hand " + e.target.elements.hand.value);
        }
    }
    return {wager, opponent, hand};};

const randomHand = () => {
    const array = new Uint8Array(1);
    window.crypto.getRandomValues(array);
    return array[0] % 3;
};

const uint32ToHex = (u) => web3.toHex(u + 0x100000000).slice(3);

const getGameID = () => {
    const gameID = nextID;
    nextID = nextID + 1;
    putUserStorage("nextID", nextID);
    return gameID;
}

const createNewGame = (wager, escrow, _opponent, hand) => {
    const gameID = getGameID();
    const key = uint32ToHex(gameID);
    // TODO: let advanced users override the salt? Not without better transaction tracking.
    // Right now we rely on robust randomness to track the transactions by commitment.
    const salt = randomSalt();
    const opponent = _opponent || "0x0000000000000000000000000000000000000000";
    // TODO: add the ID to the contract call for tracking purpose? Use the low bits of the escrow?
    // Or the high bits of the hand? No, use the commitment:
    // const commitment = makeCommitment(salt, hand);
    // Somehow when we restart transactions, we must match them that way.
    // We could use the nonce for the transaction, but there's no atomic access to it.
    // Could we save the TxHash locally *before* sending it online? Unhappily web3 doesn't allow that:
    // < https://github.com/MetaMask/metamask-extension/issues/3475 >.
    putUserStorage(key, {role: 0, status: "Creating the game…",
                         salt, hand, opponent, timeoutInBlocks, wagerInWei, escrowInWei});
    const commitment = makeCommitment(salt, hand);
    activeGamesByCommitment[commitment] = key;
    return player0StartGame(salt, hand, opponent, timeoutInBlocks, wagerInWei, escrowInWei)((txHash) => {
        activeGamesByTxHash[txHash] = key;
        updateUserStorage(key, {status: "Posted the game-creation transaction…", txHash})});}

const submitNewGame = (e) => {
        e.preventDefault();
        try {
            const {wagerInEth, opponent, hand} = gameParametersOfForm(e);
            const escrowInEth = wagerInEth.mul(0.1);
            const h = hand || randomHand();
            const confirmation = `You are going to start a new game with ${opponent ? opponent : "anyone who will play"} for a wager of ${wager} ETH, with an escrow of ${escrow} ETH, and play ${labelOfHand(h)}.`;
            const confirmed = window.confirm(confirmation);
            if (confirmed) {
                const wagerInWei = ethToWei(wagerInEth);
                const escrowInWei = ethToWei(escrowInEth);
                createNewGame(wagerInWei, escrowInWei, opponent, h);
                renderNewGame();
            }
        } catch (e) {
            window.alert(e);
        }
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
        NB: Using a ${config.timeoutInBlocks}-block timeout (${config.timeoutString}).
    `;

const renderNewGame = () => {
    const el = document.createElement('form');
    el.innerHTML = renderGameChoice (nextID, null, null);
    el.addEventListener('submit', submitNewGame);
    setNodeBySelector("#NewGame", el);
};

const restartGame = (node, id) => {
    // XXX TODO
}
const renderOpenGames = () => {
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
    nextID = getUserStorage("nextID", 0);
    activeGamesById = getUserStorage("activeGamesById");
    renderNewGame();
    renderOpenGames();
    renderActiveGames();
}
