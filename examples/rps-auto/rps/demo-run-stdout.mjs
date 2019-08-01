// vim: filetype=javascript

import { runGameWith } from './demo.mjs';

const wagerInEth  = 1.5;
const escrowInEth = 0.15;

const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';

function makeInteractWith(label) {
  return (name, handf) => (a, cb) => {
  const res = a === 'getHand' ? handf() : ``;
  const msg
        = a === 'params'  ? `${name} publishes parameters of game: wager of ${wagerInEth}ETH and escrow of ${escrowInEth}ETH.`
        : a === 'accepts' ? `${name} accepts the terms.`
        : a === 'getHand' ? `(local: ${name} plays ${res}.)`
        : a === 'commits' ? `${name} commits to play with (hidden) hand.`
        : a === 'shows'   ? `${name} sends hand in clear.`
        : a === 'reveals' ? `${name} reveals salt and hand.`
        : a === 'outcome' ? `${name} agrees that game is over.`
        : null;

  !!msg && console.log(`${label} ${msg}`);

  return cb(res);
}; }

function makeDemo(doWhile, drawFirst) {
    const label = doWhile ? `[Loop]:` : `[Single]:`;
    return new Promise(
        (resolve) =>
            Promise.resolve(console.log(`${label} Alice initiates a new game on the ${uri} Ethereum node.`))
            .then(() => runGameWith(doWhile, drawFirst, makeInteractWith(label), wagerInEth, escrowInEth, uri))
            .then((gs) => console.log(`${label} Alice thinks outcome is ${gs.outcomeAlice}\n${label} Bob thinks outcome is ${gs.outcomeBob}`))
            .then(() => console.log(`${label} Done!`))
            .then(resolve) ); }

makeDemo(true, true)
 .then(() => makeDemo(false, true))
 .then(() => makeDemo(false, false))
 .then(() => process.exit(0))
 .catch(e => console.error(e) || process.exit(1));
