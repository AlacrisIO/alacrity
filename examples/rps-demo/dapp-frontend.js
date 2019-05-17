/** web3 client frontend for rock-paper-scissors. */
/*
  TODO:

  * Display the game in each of its possible states

  * Have a flag to dismiss completed games

  TODO LATER MAYBE

  * Add speculative information from unconfirmed transactions,
    with clear distinction between what's confirmed,
    and what's unconfirmed by which player.

  * Offer user to decide based on the unconfirmed, but only publicly act when confirmed
    (allow to take back what wasn't done publicly yet?)

  * On frontend, have a list of common playing partners, with short aliases.
*/
'use strict';

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
    if (node) {
        node.innerHTML = '';
        node.appendChild(content);
    }
}

const simpleNumberRegex = "[0-9]+([.][0-9]+)?|[.][0-9]+";
const addressRegex = "0x[0-9A-Fa-f]{40}";

// TODO: offer easy standard amounts for the amount
// TODO: let the users negotiate the escrow.
// TODO: determine a minimum acceptable escrow, and suggest that?
// TODO: determine a minimum amount based on the minimum escrow?
const minWagerInEth = .01;
const defaultWagerInEth = 1;

const wagerToEscrow = wager => toBN(wager).div(10);

const renderWager = amountInWei => {
    const common = 'style="${font-size: 10pt}" name="wagerInEth"';
    return `
    <label style="text-align: center;">
       Wager amount in ETH${amountInWei ? "" : " (plus automatic 10% escrow)"}:
       <br>
       ${amountInWei ? `<output ${common}>${weiToEth(amountInWei)}</output>` :
         `<input ${common} pattern="${simpleNumberRegex}"
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

const inputWager = e => (k, kError = kLogError) => {
    const wagerInput = e.target.elements.wagerInEth.value;
    let wagerInEth, error;
    try {
        wagerInEth = toBN(wagerInput);
        if (wagerInEth < minWagerInEth) { throw "foo"; }
    } catch (err) {
        error = err;
    }
    if (error) {
        return kError(`invalid game id ${wagerInput} ${error}`);
    } else {
        return k(ethToWei(wagerInEth));
    }
}

const inputOpponent = e => (k, kError = kLogError) => {
    const opponentInput = e.target.elements.opponent.value;
    if (opponentInput == "" || toBN(opponentInput).isZero()) {
        return k(undefined);
    } else if (web3.isAddress(opponentInput)) {
        return k(opponentInput);
    } else {
        return kError(`Invalid opponent ${opponentInput}`);
    }
}

const inputHand = e => (k, kError = kLogError) => {
    const handInput = e.target.elements.hand.value;
    if (handInput == "") {
        return k(undefined);
    }
    try {
        const hand = parseInt(handInput, 10);
        if (isValidHand(hand)) {
            return k(hand);
        }
    } catch (_) { }
    return kError(`Invalid hand ${handInput}`);
}

const randomHand = () => {
    const array = new Uint8Array(6);
    window.crypto.getRandomValues(array);
    return (array[0]+array[1]+array[2]+array[3]+array[4]+array[5]) % 3;
};

const submitNewGameClick = e => {
    e.preventDefault();
    inputWager(e)(
    wagerInWei =>
    inputOpponent(e)(
    opponent =>
    inputHand(e)(
    hand_ => {
    const escrowInWei = wagerToEscrow(wagerInWei);
    const hand = hand_ || randomHand();
    const confirmation = `You are going to start a new game with ${opponent ? opponent : "anyone who will play"} for a wager of ${renderWei(wagerInWei)}, with an escrow of ${renderWei(escrowInWei)}, and play ${labelOfHand(hand)}.`;
    const confirmed = window.confirm(confirmation);
    if (confirmed) {
        createNewGame(wagerInWei, escrowInWei, opponent, hand);
        renderNewGame();
    }},
    loggedAlert), loggedAlert), loggedAlert)}

// TODO: let you override the random salt (option hidden by default).
const renderGameChoice = () => `
        ${renderHandChoice()}
        <br>
        <button style='width: 100%;'>Shoot!</button>
        NB: Running on ${config.networkName} with a ${config.timeoutInBlocks}-block timeout (${config.timeoutString}).
    `;

const renderNewGame = () => {
    const form = document.createElement('form');
    form.innerHTML = `
        ${renderWager()}
        <br>
        ${renderOpponent()}
        <br>
        ${renderGameChoice()}`;
    form.addEventListener('submit', submitNewGameClick);
    setNodeBySelector("#NewGame", form);
};

let gamesNode;

const gameFormId = id => `game_${id}`;

const findOrCreateGameForm = id => {
    const nodeId = gameFormId(id);
    const node = document.getElementById(nodeId);
    if (node) { return node; }
    const form = document.createElement('form');
    form.setAttribute("id", nodeId);
    gamesNode.appendChild(form);
    return form;
}

const pronoun = (who, you, lowercase) => {
    const capitalized = who == you ? "You" : "They";
    return lowercase ? capitalized.toLowerCase() : capitalized; }

const shorten0x = string0x =>
    string0x.length < 11 ? string0x : `${string0x.slice(0, 6)}…${string0x.slice(-4)}`;

// TODO: replace the middle letters by "…" ?
const renderTransaction = txHash =>
      txHash ? `<a href="${config.txExplorerUrl}${txHash}">${shorten0x(txHash)}</a>` : "unknown";
const renderAddress = address =>
      address ? address == zeroAddress ? "anyone" :
      `<a href="${config.addressExplorerUrl}${address}">${shorten0x(address)}</a>` : "unknown";
const renderWei = amountInWei =>
      `${weiToEth(amountInWei)} ETH`;
const renderCommitment = (commitment, txHash) =>
      commitment ?
      txHash ? `<a href="${config.txExplorerUrl}${txHash}#eventlog">${shorten0x(commitment)}</a>`
      : commitment : "unknown";

const renderGameOutcome = outcome =>
      outcome == 0 ? "unknown" :
      outcome == 1 ? "draw" :
      outcome == 2 ? "player0 won" :
      outcome == 3 ? "player1 won" :
      outcome == 4 ? "player0 timed out, player1 won by default" :
      outcome == 5 ? "player1 timed out, player0 rescinded wager" :
      "unknown";

const renderGameState = (state, outcome) =>
      state == State.uninitialized ? "Wager unconfirmed" :
      state == State.WaitingForPlayer1 ? "Waiting for player1" :
      state == State.WaitingForPlayer0Reveal ? "Waiting for player0 reveal" :
      state == State.Completed ? `Completed, ${renderGameOutcome(outcome)}` :
      "unknown";

const inputId = e => (k, kError = kLogError) => {
    const idInput = e.target.elements.id.value;
    let id, game, error;
    try {
        id = parseInt(idInput, 10);
        if (!Number.isInteger(id)) { throw "bad game id"; }
        game = getGame(id);
        if (game === null) { throw "no such game"; }
    } catch (err) {
        error = err;
    }
    if (error) {
        return kError(`invalid game id ${idInput} ${error}`);
    } else {
        return k(id, game);
    }
}

const dismissGameClick = e => {
    e.preventDefault();
    inputId(e)(dismissGame, loggedAlert);
}

const acceptGameClick = e => {
    e.preventDefault();
    inputId(e)((id, _game) =>
    inputHand(e)(hand => acceptGame(id, hand),
    loggedAlert), loggedAlert);}

const renderGame = id => {
    logGame(id, "Render Game:");
    const g = getGame(id);
    if (g.isDismissed) {
        setNodeBySelector(`#${gameFormId(id)}`, emptyNode());
        return;
    }
    const form = findOrCreateGameForm(id);
    const player0 = (g.unconfirmedState && g.unconfirmedState.player0) || g.player0;
    const player1 = (g.unconfirmedState && g.unconfirmedState.player1) || g.player1;
    const player0Commitment = (g.unconfirmedState && g.unconfirmedState.player0Commitment) || g.player0Commitment;
    const state = (g.unconfirmedState && g.confirmedState.state) || State.uninitialized;
    const outcome = g.unconfirmedState && g.confirmedState.outcome;
    // TODO: link to suitable network-dependent block viewer for txHash, contract, etc.
    const setup = `${pronoun(player0, userAddress)} ${renderAddress(player0)} as player0
wagered ${renderWei(g.wagerInWei)} (plus a ${renderWei(g.escrowInWei)} escrow)
with commitment ${renderCommitment(player0Commitment, g.txHash)}.<br />`;
    let current = "";
    // TODO: somehow estimate how much time there is before deadline, and
    // display both an estimated time and a countdown timer.
    // Also display when the deadline is past.

    switch (g.confirmedState && g.confirmedState.state) {

        case State.WaitingForPlayer1:
        if (player1 != userAddress && (player0 == userAddress || player1 != zeroAddress)) {
            current = player1 == zeroAddress ?
                "Nobody accepted the wager and chose a hand as player1 yet." :
                `${pronoun(player1, userAddress)} ${renderAddress(player1)} as player1
haven't accepted the wager and chose a hand yet.`;
        } else if (g.hand1) {
            // TODO: deal with non-atomicity of hand1 and player1TxHash
            current = `You played ${handName(g.hand1)}.
Waiting for your transaction ${renderTransaction(g.player1TxHash)} to be confirmed.` ;
        } else if (g.confirmedState.wagerInWei < ethToWei(minWagerInEth)) {
            current = `The proposed wager ${renderWei(g.confirmedTransaction.wagerInWei)} is too low.
DO NOT PLAY THIS GAME.`;
        } else if (g.confirmedState.escrowInWei < wagerToEscrow()) {
            current = `The proposed escrow ${renderWei(g.confirmedTransaction.escrowInWei)} is too low.
DO NOT PLAY THIS GAME.`;
        } else {
            current = `To accept the wager and play the game, choose a hand:<br />
${renderGameChoice()}`;
            form.addEventListener('submit', acceptGameClick);
        }
        break;

        case State.WaitingForPlayer0Reveal:
        if (player0 != userAddress) {
            current = `${pronoun(player1, userAddress)} ${renderAddress(player1)} as player0
haven't publicly revealed their hand yet.`;
        } else if (g.player0RevealTxHash) {
            // TODO: deal with non-atomicity of hand1 and player1TxHash
            current = `Waiting for your reveal transaction ${renderTransaction(g.player0RevealTxHash)} to be confirmed.` ;
        } else {
            current = `You should send a reveal transaction ASAP.`
        }
        break;

        case State.Completed:
        // TODO: show the outcome, etc.
        current = `<button>Dismiss</button>`;
        form.addEventListener('submit', dismissGameClick);
        break;

        default: current = "???";
    }

    form.innerHTML = `<p><b>Game <output name="id">${id}</output></b>${
g.txHash ? `, tx ${renderTransaction(g.txHash)}` : ""}${
g.contract ? `, contract ${renderAddress(g.contract)}` : ""}:<br />
<em>${renderGameState(state, outcome)}.</em><br />
${setup}${current}</p>`;
}
renderGameHook = renderGame;

const renderOpenGames = () => {
    // XXX TODO
}
const renderActiveGames = () => {
    const node = document.createElement('div');
    if (activeGames.length > 0) {
        for(let id in activeGames) { renderGame(node, id); }
    } else {
        node.innerHTML = '<p id="NoActiveGames">(No currently active game)</p>';
    }
    setNodeBySelector("#ActiveGames", node);
}

const emptyNode = () => document.createTextNode("");

// TODO: way to download the localState
// TODO: way to use a remote replicated backup service for encrypted state management.
const initFrontend = k => {
    logging("initFrontend")();
    setNodeBySelector("#Prerequisites", document.createTextNode(""));
    gamesNode = document.getElementById("ActiveGames");
    if (config && config.contract) {
        setNodeBySelector("#NoNewGames", emptyNode());
        // setNodeBySelector("#NoOpenGames", emptyNode());
        setNodeBySelector("#NoActiveGames", emptyNode());
        renderNewGame();
    } else {
        setNodeBySelector("#Play", htmlToElement(
            "<b>No contract deployed on this network. <br/>" +
            "Please reload this page after connecting to Rinkeby.</b>"));
    };
    return k();
}

registerInit(initFrontend);
