// vim: filetype=javascript

import * as RPS       from '../build/rps.mjs';
import * as RPSW      from '../build/rps_while.mjs';
import { stdlibNode } from './stdlib/web3/node.mjs';

const init = (theRPS, wagerInEth, escrowInEth, uri) =>
  stdlibNode(theRPS.ABI, theRPS.Bytecode, uri)
    .then(stdlib => {
      const wagerInWei  = stdlib.toBN(stdlib.web3.toWei(wagerInEth,  'ether'));
      const escrowInWei = stdlib.toBN(stdlib.web3.toWei(escrowInEth, 'ether'));

      return { stdlib, gameState: { wagerInWei, escrowInWei }};
    });

const play = (theRPS, drawFirst, interactWith) => ({ stdlib, gameState }) => {
  const { web3, balanceOf, devnet, transfer } = stdlib;
  const { prefundedDevnetAcct               } = devnet;
  const { wagerInWei, escrowInWei           } = gameState;

  const newPlayer = () =>
    devnet.createAndUnlockAcct()
      .then(to => transfer(to, prefundedDevnetAcct, web3.toWei(100, 'ether')))
      .then(stdlib.EthereumNetwork);

  const captureOpeningGameState = ([ a, b ]) =>
    Object.assign(gameState
               , { alice: a
                 , bob:   b
                 , ctors: [ a.userAddress, b.userAddress ]
                 , balanceStartAlice: balanceOf(a)
                 , balanceStartBob:   balanceOf(b)
                 });

  const captureClosingGameState = ([ outcomeBob, outcomeAlice ]) =>
    Promise.resolve(Object.assign(gameState, { outcomeAlice, outcomeBob }));

  const randomArray = (a) => { return a[Math.floor(Math.random() * a.length)]; };
  const randomHand = () => randomArray(['ROCK', 'PAPER', 'SCISSORS']);
  const makeDrawFirstHand = (first) => {
      var called = false;
      return () => {
          if ( called ) {
              return randomHand(); }
          else {
              called = true;
              return first; } }; };
  const shared = randomHand();
  const makeWhichHand = drawFirst ? (() => makeDrawFirstHand(shared)) : (() => randomHand);

  const txn0 = { balance: 0, value: 0 };

  const bobShoot = ctcAlice =>
    new Promise(resolve =>
      gameState.bob.attach(gameState.ctors, ctcAlice.address)
      .then(ctcBob => theRPS.B(stdlib, ctcBob, txn0, interactWith('Bob',makeWhichHand()), resolve)));

  const aliceShoot = ctc =>
    new Promise(resolve =>
      theRPS.A(stdlib, ctc, txn0, interactWith('Alice',makeWhichHand()), wagerInWei, escrowInWei, resolve));

  return Promise.all([ newPlayer(), newPlayer() ])
    .then(captureOpeningGameState)
    .then(()  => gameState.alice.deploy(gameState.ctors))
    .then(ctc => Promise.all([ bobShoot(ctc), aliceShoot(ctc) ]))
    .then(captureClosingGameState);
};

export const runGameWith = (doWhile, drawFirst, interactWith, wagerInEth, escrowInEth, uri) =>
    { const theRPS = doWhile ? RPSW : RPS;
      return init(theRPS, wagerInEth, escrowInEth, uri)
      .then(play(theRPS, drawFirst, interactWith)); };
