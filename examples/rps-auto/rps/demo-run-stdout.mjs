// vim: filetype=javascript

import * as RPS       from '../build/rps.mjs';
import * as RPSW      from '../build/rps_while.mjs';
import * as D         from './demo.mjs';
import { stdlibNode } from './stdlib/web3/node.mjs';

const wagerInEth  = '1.5';
const escrowInEth = '0.15';

const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';


const mkInteractWith = (label, shouldForceTimeout) => (name, handf) => (a, cb) => {
  if (shouldForceTimeout(name, a)) {
    console.log(`${label} ${name} deliberately forces a timeout!`);
    return;
  }

  const res = a === 'getHand' ? handf() : '';

  const paramsMsg =
    [ `${name} publishes parameters of game:`
    , `wager of ${wagerInEth}ETH`
    , `and escrow of ${escrowInEth}ETH.`
    ].join(' ');

  const msg
    = a === 'params'  ? paramsMsg
    : a === 'accepts' ? `${name} accepts the terms.`
    : a === 'getHand' ? `(local: ${name} plays ${D.stringOfHand(res)}.)`
    : a === 'commits' ? `${name} commits to play with (hidden) hand.`
    : a === 'shows'   ? `${name} sends hand in clear.`
    : a === 'reveals' ? `${name} reveals salt and hand.`
    : a === 'outcome' ? `${name} agrees that game is over.`
    : null;

  !!msg && console.log(`${label} ${msg}`);

  return cb(res);
};


const mkDemo = (doWhile, drawFirst, shouldForceTimeout) => {
  const label = doWhile ? `[Loop]:` : `[Single]:`;

  const introMsg =
    [ `${label} Alice initiates a new game using the \`web3\` stdlib`
    , `on the ${uri} Ethereum node.`
    ].join(' ');

  const outcomeMsgs = gs =>
    [ `${label} Alice thinks outcome is ${D.stringOfOutcome(gs.outcomeAlice)}.`
    , `${label} Bob thinks outcome is ${D.stringOfOutcome(gs.outcomeBob)}.`
    ].join('\n');

  const theRPS = doWhile ? RPSW : RPS;

  const runGameWithTheRPS = s =>
    D.runGameWith(theRPS
                , s
                , doWhile
                , drawFirst
                , mkInteractWith(label, shouldForceTimeout)
                , wagerInEth
                , escrowInEth
                , uri);

  return new Promise(resolve =>
    Promise.resolve(console.log(introMsg))
      .then(() => stdlibNode(theRPS.ABI, theRPS.Bytecode, uri))
      .then(runGameWithTheRPS)
      .then(gs => console.log(outcomeMsgs(gs)))
      .then(() => console.log(`${label} Done!\n`))
      .then(resolve));
};


// TODO re-enable `WHILE` demo
mkDemo(false, true, D.timeoutPred.aliceForces)
  .then(() => mkDemo(false, false, D.timeoutPred.bobForces))
  .then(() => mkDemo(false, true,  D.timeoutPred.never))
  .then(() => mkDemo(false, false, D.timeoutPred.never))
  .then(() => process.exit(0))
  .catch(e => console.error(e) || process.exit(1));
