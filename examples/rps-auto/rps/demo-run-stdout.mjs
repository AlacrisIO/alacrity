// vim: filetype=javascript

import { runGameWith } from './demo.mjs';

const wagerInEth  = 1.5;
const escrowInEth = 0.15;

const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';

const interactWith = name => (a, cb) => {
  const commits =
    [ `${name} commits to play with (hidden) hand of rock,`
    , `wager of ${wagerInEth}ETH,`
    , `and escrow of ${escrowInEth}ETH.`
    ].join(' ');

  const msg
    = a === 'commits' ? commits
    : a === 'accepts' ? `${name} plays scissors and matches wager.`
    : a === 'reveals' ? `${name} reveals salt and hand.`
    : a === 'outcome' ? `${name} agrees: Alice wins and receives ${wagerInEth}.`
    : null;

  !!msg && console.log(msg);

  return cb();
};

Promise.resolve(console.log(`Alice initiates a new game on ${uri} node`))
  .then(() => runGameWith(interactWith, wagerInEth, escrowInEth, uri))
  .then(() => console.log('Alice\'s escrow has been reimbursed.'))
  .then(() => console.log('Done!'))
  .then(() => process.exit(0))
  .catch(e => console.error(e) || process.exit(1));
