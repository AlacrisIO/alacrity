/** web3 client frontend for rock-paper-scissors. */
/* TODO:

 * Display the game in each of its possible states

 * Have a flag to dismiss completed games

 * TODO LATER

 * Display the unconfirmed if different from confirmed.

 * Offer user to decide based on the unconfirmed, but only act when confirmed.
*/
'use strict';

/** State is a list of games.
 * Each game has a state:
   */

const htmlToElement = html => {
    const template = document.createElement('template');
    html = html.trim(); // Never return a text node of whitespace as the result
    template.innerHTML = html;
    return template.content.firstChild;
}

const htmlToElements = html => {
    const template = document.createElement('template');
    template.innerHTML = html;
    return template.content.childNodes;
}

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
const minWagerInEth = .01;
const defaultWagerInEth = 1;

const renderWager = amountInWei => {
    const common = 'style="${font-size: 10pt}" name="wagerInEth"';
    return `
    <label style="text-align: center;">
       Wager amount in ETH${amountInWei ? "" : " (plus automatic 10% escrow)"}:
       <br>
       ${amountInWei ? `<output >${weiToEth(amountInWei)}</output>` :
         `<input style="${common}" pattern="${simpleNumberRegex}"
           type="number" min="${minWagerInEth}" step="${minWagerInEth}" value="${defaultWagerInEth}" required />`}
    </label>`;};

// TODO: have a greyed out message "default: anyone" in the input style
// TODO: support a list of known opponents, and giving nicknames to known opponents
// TODO: add known-partner and nickname support to MetaMask (?)
const renderOpponent = opponent => {
    const common='style="font-size: 10pt;" name="opponent"';
    return `
    <label style="text-align: center;">
        Opponent address${opponent ? "" : " (leave empty for an open game)"}: <br />
        ${opponent ?
            `<output ${common}">${opponent}</output>` :
            `<input ${common}" pattern="${addressRegex}" size="50">`}</label>`;};

// TODO: use nice icons.
const iconOfHand = hand => ['✊', '✋', '✌'][hand] || '';
const labelOfHand = hand => ['Rock', 'Paper', 'Scissors'][hand] || '';
const renderHandOption = hand => `
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

const gameParametersOfForm = e => {
    // TODO: validate input, instead of having a default values!
    const wagerInput = e.target.elements.wagerInEth.value;
    let wagerInEth;
    try {
        wagerInEth = web3.toBigNumber(wagerInput);
        if (wagerInEth < minWagerInEth) { throw "foo"; }
    } catch (err) {
        throw("Invalid wager amount " + wagerInput);
    }
    const opponentInput = e.target.elements.opponent.value;
    let opponent;
    if (opponentInput == "" || web3.toBigNumber(opponentInput).isZero()) {
        opponent = undefined;
    } else if (!web3.isAddress(opponentInput)) {
        throw("Invalid opponent " + opponentInput);
    } else {
        opponent = opponentInput;
    }
    let handInput = e.target.elements.hand.value;
    let hand;
    if (handInput == "") {
        hand = undefined;
    } else {
        hand = parseInt(handInput, 10);
        if (! (hand >=0 && hand < 3)) {
            throw("Invalid hand " + handInput);
        }
    }
    return {wagerInEth, opponent, hand};};

const randomHand = () => {
    const array = new Uint8Array(1);
    window.crypto.getRandomValues(array);
    return array[0] % 3;
};

const createNewGame = (wagerInWei, escrowInWei, opponent, hand) => {
    const gameID = getGameID();
    const id = idToString(gameID);
    // TODO: let advanced users override the salt? Not without better transaction tracking.
    // Right now we rely on robust randomness to track the transactions by commitment.
    const salt = randomSalt();
    const commitment = makeCommitment(salt, hand);
    const player0 = userAddress;
    const player1 = opponent || "0x0000000000000000000000000000000000000000";
    const timeoutInBlocks = config.timeoutInBlocks;
    // TODO: add the ID to the contract call for tracking purpose? Use the low bits of the escrow?
    // Or the high bits of the hand? No, use the commitment:
    // const commitment = makeCommitment(salt, hand);
    // Somehow when we restart transactions, we must match them that way.
    // We could use the nonce for the transaction, but there's no atomic access to it.
    // Could we save the TxHash locally *before* sending it online? Unhappily web3 doesn't allow that:
    // < https://github.com/MetaMask/metamask-extension/issues/3475 >.
    putUserStorage(id, {role: 0, status: "Creating the game…",
                        salt, hand, commitment, player0, player1, timeoutInBlocks, wagerInWei, escrowInWei});
    activeGamesByCommitment[commitment] = key;
    return player0StartGame(salt, hand, opponent, timeoutInBlocks, wagerInWei, escrowInWei)(txHash => {
        gamesByTxHash[txHash] = key;
        updateUserStorage(key, {status: "Posted the game-creation transaction…", txHash})});}

const submitNewGame = e =>
 {
        e.preventDefault();
        try {
            const {wagerInEth, opponent, hand} = gameParametersOfForm(e);
            const escrowInEth = wagerInEth.mul(0.1);
            const h = hand || randomHand();
            const confirmation = `You are going to start a new game with ${opponent ? opponent : "anyone who will play"} for a wager of ${wagerInEth} ETH, with an escrow of ${escrowInEth} ETH, and play ${labelOfHand(h)}.`;
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
const renderGameChoice = (id, wagerInWei, opponent) => `
        ${renderWager(wagerInWei)}
        <br>
        ${renderOpponent(opponent)}
        <br>
        ${renderHandChoice()}
        <br>
        <button style="width: 100%;">Shoot!</button>
        NB: Running on ${config.networkName} with a ${config.timeoutInBlocks}-block timeout (${config.timeoutString}).
    `;

const renderNewGame = () => {
    const el = document.createElement('form');
    el.innerHTML = renderGameChoice (nextID, null, null); // TODO: only allocate ID after game is started
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
    if (activeGames.length > 0) {
        for(let id in activeGames) { restartGame(node, id); }
    } else {
        node.innerHTML = "<p>(No currently active game)</p>";
    }
    setNodeBySelector("#ActiveGames", node);
}

// TODO: way to download the localState
// TODO: way to use a remote replicated backup service for encrypted state management.
const initFrontend = k => {
    setNodeBySelector("#Prerequisites", document.createTextNode(""));
    if (config && config.contract) {
        renderNewGame();
        renderOpenGames();
        renderActiveGames();
    } else {
        setNodeBySelector("#Play", htmlToElement(
            "<b>No contract deployed on this network. <br/>" +
            "Please reload this page after connecting to Rinkeby.</b>"));
    };
    return k();
}

registerInit(initFrontend);
