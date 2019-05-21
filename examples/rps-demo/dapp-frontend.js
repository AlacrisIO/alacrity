/** web3 client frontend for rock-paper-scissors. */
/*
  DONE:

  * Have a flag to dismiss completed games

  TODO:

  * Display the game in each of its possible states

  TODO LATER MAYBE

  * When the list of active games drops to zero, add a message; remove it when it become non-empty.

  * Add speculative information from unconfirmed transactions,
    with clear distinction between what's confirmed,
    and what's unconfirmed by which player.

  * Offer user to decide based on the unconfirmed, but only publicly act when confirmed
    (allow to take back what wasn't done publicly yet?)

  * On frontend, have a list of common playing partners, with short aliases.
*/
'use strict';

// TODO: offer easy standard amounts for the amount
// TODO: let the users negotiate the escrow.
// TODO: determine a minimum acceptable escrow, and suggest that?
// TODO: determine a minimum amount based on the minimum escrow?
const minWagerInEth = .01;
const defaultWagerInEth = .1;

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
const handIcon = hand => ['✊', '✋', '✌'][hand] || '';
const renderHandOption = hand => `
    <label style="display: inline-block; margin: .5em; text-align: center;">
        <input type="radio" value="${hand}" name="hand">
        <div class="symbol">${handIcon(hand)}</div>
        ${handName(hand)}
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
    let hand, error;
    try {
        hand = parseInt(handInput, 10);
        error = !isValidHand(hand);
    } catch (e) { error = e; }
    if (error) {
        return kError(`Invalid hand ${handInput}`);
    } else {
        return k(hand);
    }
}

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
    const confirmation = `You are going to start a new game with ${opponent ? opponent : "anyone who will play"} for a wager of ${renderWei(wagerInWei)}, with an escrow of ${renderWei(escrowInWei)}, and play ${handName(hand)}.`;
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
        <div align='center'><button style='width: 50%;'>Shoot!</button></div><br />
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

// TODO: replace the middle letters by "…" ?
const renderCommitment = (commitment, txHash) =>
      commitment ?
      txHash ? `<a href="${config.txExplorerUrl}${txHash}#eventlog">${shorten0x(commitment)}</a>`
      : shorten0x(commitment) : "unknown";

const renderGameOutcome = (outcome, player0, player1) =>
      outcome == Outcome.Unknown ? "The game isn't complete yet" :
      outcome == Outcome.Draw ? "The game is a draw" :
      outcome == Outcome.Player0Wins ? `${pronoun(player0, userAddress)} ${renderAddress(player0)} won as player0` :
      outcome == Outcome.Player1Wins ? `${pronoun(player1, userAddress)} ${renderAddress(player1)} won as player1` :
      outcome == Outcome.Player1WinsByDefault ? `${pronoun(player0, userAddress)} ${renderAddress(player0)} timed out as player0. \
${pronoun(player1, userAddress)} ${renderAddress(player1)} won by default as player1` :
      outcome == Outcome.Player0Rescinds ? (player1 ? `${pronoun(player1, userAddress)} ${renderAddress(player1)} timed out as player1` : "No one chose to accept the game as player1") +
`. ${pronoun(player0, userAddress)} ${renderAddress(player0)} as player0 rescinded the offer to play the game` :
      "invalid outcome";

const renderGameState = (state, outcome, player0, player1) =>
      state == State.uninitialized ? "Wager unconfirmed" :
      state == State.WaitingForPlayer1 ? "Waiting for player1" :
      state == State.WaitingForPlayer0Reveal ? "Waiting for player0 reveal" :
      state == State.Completed ? `Completed. ${renderGameOutcome(outcome, player0, player1)}` :
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

const displayedGames = {};

const renderActiveGameHook = () => {
    setNodeBySelector("#NoActiveGames", document.createTextNode(
        isEmpty(displayedGames) ? "(No currently active game)" : ""));
}

const unrenderGame = id => {
    setNodeBySelector(`#${gameFormId(id)}`, emptyNode());
    delete displayedGames[id];
    renderActiveGameHook();
}

const renderGame = (id, tag) => {
    logGame(id, "(Frontend) " + tag);
    const g = getGame(id);
    if (!g || g.isDismissed) { return unrenderGame(id); }
    const form = findOrCreateGameForm(id);
    const player0 = (g.unconfirmedState && g.unconfirmedState.player0) || g.player0;
    const player1 = (g.unconfirmedState && g.unconfirmedState.player1 != zeroAddress && g.unconfirmedState.player1) || (g.player1 != zeroAddress && g.player1);
    const player0Commitment = (g.unconfirmedState && g.unconfirmedState.player0Commitment) || g.player0Commitment;
    const state = (g.unconfirmedState && g.confirmedState.state) || State.uninitialized;
    const outcome = g.unconfirmedState && g.confirmedState.outcome;
    const hand0 = g.hand || (g.unconfirmedState && g.unconfirmedState.state == State.Completed && g.unconfirmedState.hand0);
    const hand1 = g.hand1 || (g.unconfirmedState && (g.unconfirmedState.state == State.Completed || g.unconfirmedState.state == State.WaitingForPlayer0Reveal) && g.unconfirmedState.hand1);
    const setup = `${pronoun(player0, userAddress)} ${renderAddress(player0)} as player0
wagered ${renderWei(g.wagerInWei)} (plus a ${renderWei(g.escrowInWei)} escrow)
with commitment ${renderCommitment(player0Commitment, g.txHash)}\
${hand0 ? ` (secretly playing ${handName(hand0)})` : ""}.<br />`;
    let current = "";
    // TODO: somehow estimate how much time there is before deadline, and
    // display both an estimated time and a countdown timer.
    // Also display when the deadline is past.
    // TODO: show a history of what happened as messages and the events emitted as their consequences.
    switch (g.confirmedState && g.confirmedState.state) {

        case State.WaitingForPlayer1:
        if (hand1) {
            // TODO: deal with non-atomicity of hand1 and player1TxHash
            current = `You ${renderAddress(player1)} as player1 played ${handName(hand1)}. \
Waiting for your transaction ${renderTransaction(g.player1ShowHandTxHash)} to be confirmed.` ;
        } else if (player0 == userAddress && (!player1 || player1 == userAddress)) {
            current = player1 ?
            `${pronoun(player1, userAddress)} ${renderAddress(player1)} as player1
haven't accepted the wager and chosen a hand yet.` :
            "Nobody accepted the wager and chose a hand as player1 yet.";
        } else if (g.confirmedState.wagerInWei < ethToWei(minWagerInEth)) {
            current = `The proposed wager ${renderWei(g.confirmedTransaction.wagerInWei)} is too low.
DO NOT PLAY THIS GAME.`;
            g.isCompleted = true;
        } else if (g.confirmedState.escrowInWei < wagerToEscrow()) {
            current = `The proposed escrow ${renderWei(g.confirmedTransaction.escrowInWei)} is too low.
DO NOT PLAY THIS GAME.`;
            g.isCompleted = true;
        } else {
            current = `To accept the wager and play the game, choose a hand:<br />
${renderGameChoice()}`;
            form.addEventListener('submit', acceptGameClick);
        }
        break;

        case State.WaitingForPlayer0Reveal:
        current = `${pronoun(player1, userAddress)} ${renderAddress(player1)} as player1 played ${handName(hand1)}. `;
        if (g.player0RevealTxHash) {
            // TODO: deal with non-atomicity of hand1 and player1TxHash
            current += `You ${renderAddress(userAddress)} as player0 posted transaction ${renderTransaction(g.player0RevealTxHash)} to reveal your hand ${handName(hand0)}. Waiting for it to be confirmed.` ;
        } if (player0 == userAddress) {
            current += `You ${renderAddress(userAddress)} as player0 should send a reveal transaction ASAP.`;
        } else {
            current += `${pronoun(player0, userAddress)} ${renderAddress(player0)} as player0 \
haven't publicly revealed their hand yet.`;
        }
        break;

        case State.Completed:
        // TODO: show what outcome, etc.
        g.isCompleted = true;
        switch (outcome) {
        case Outcome.Draw:
        case Outcome.Player0Wins:
        case Outcome.Player1Wins:
        case Outcome.Player1WinsByDefault:
        current = `${pronoun(player1, userAddress)} ${renderAddress(player1)} as player1 played ${handName(hand1)}. `;
        }
        switch (outcome) {
        case Outcome.Draw:
        case Outcome.Player0Wins:
        case Outcome.Player1Wins:
        current += `${pronoun(player0, userAddress)} ${renderAddress(player0)} as player0 revealed ${handName(hand0)}. `;
        }
        switch (outcome) {
        case Outcome.Player1WinsByDefault:
        current += `${pronoun(player0, userAddress)} ${renderAddress(player0)} as player0 timed out. `;
        }
        switch (outcome) {
        case Outcome.Player0Rescinds:
            current += player1 ?
                `${pronoun(player1, userAddress)} ${renderAddress(player1)} as player1 timed out. ` :
                `No one chose to accept the game. `;
        }
        break;
    }

    if (g.isCompleted) {
        removeActiveGame(id);
        current += `<br /><button style='width: 50%;'>Dismiss</button>`;
        form.addEventListener('submit', dismissGameClick);
    }

    form.innerHTML = `<p><b>Game <output name="id">${id}</output></b>${
g.txHash ? `, tx ${renderTransaction(g.txHash)}` : ""}${
g.contract ? `, contract ${renderAddress(g.contract)}` : ""}:<br />
<em>${renderGameState(state, outcome, player0, player1)}.</em><br />
${setup}${current}</p>`;
    displayedGames[id] = true;
    renderActiveGameHook();
}
renderGameHook = renderGame;

// TODO: way to download the localState
// TODO: way to use a remote replicated backup service for encrypted state management.
const initFrontend = k => {
    setNodeBySelector("#Prerequisites", document.createTextNode(
        `Running on ${config.networkName} \
with a ${config.timeoutInBlocks}-block timeout (${config.timeoutString}).`));
    gamesNode = document.getElementById("ActiveGames");
    if (config && config.contract) {
        setNodeBySelector("#NoNewGames", emptyNode());
        // setNodeBySelector("#NoOpenGames", emptyNode());
        setNodeBySelector("#NoActiveGames", emptyNode());
        renderNewGame();
        renderActiveGameHook();
    } else {
        setNodeBySelector("#Play", htmlToElement(
            "<b>No contract deployed on this network. <br />" +
            "Please reload this page after connecting to Rinkeby.</b>"));
    };
    return k();
}

registerInit({
    Frontend: {fun: initFrontend, dependsOn: ["Backend"]},
});
