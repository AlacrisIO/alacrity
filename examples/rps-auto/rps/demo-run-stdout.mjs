// vim: filetype=javascript

import { runGameWith } from './demo.mjs';

const wagerInEth  = 1.5;
const escrowInEth = 0.15;

const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';

const interactWith = (name, hand) => (a, cb) => {
  const msg
        = a === 'params'  ? `${name} publishes parameters of game: wager of ${wagerInEth}ETH and escrow of ${escrowInEth}ETH.`
        : a === 'accepts' ? `${name} accepts the terms.`
        : a === 'getHand' ? `(local: ${name} plays ${hand}.)`
        : a === 'commits' ? `${name} commits to play with (hidden) hand.`
        : a === 'shows'   ? `${name} sends hand in clear.`
        : a === 'reveals' ? `${name} reveals salt and hand.`
        : a === 'outcome' ? `${name} agrees that game is over.`
        : null;
  const res = a === 'getHand' ? hand : ``;

  !!msg && console.log(msg);

  return cb(res);
};

Promise.resolve(console.log(`Alice initiates a new game on the ${uri} Ethereum node.`))
  .then(() => runGameWith(interactWith, wagerInEth, escrowInEth, uri))
  .then((gs) => console.log(`Alice thinks outcome is ${gs.outcomeAlice}\nBob thinks outcome is ${gs.outcomeBob}`))
  .then(() => console.log('Done!'))
  .then(() => process.exit(0))
  .catch(e => console.error(e) || process.exit(1));
