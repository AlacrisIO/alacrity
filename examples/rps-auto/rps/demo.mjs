// vim: filetype=javascript

import * as RPS       from '../build/rps.mjs';
import { stdlibNode } from './stdlib/web3/node.mjs';

const init = (wagerInEth, escrowInEth, uri) =>
  stdlibNode(RPS.ABI, RPS.Bytecode, uri)
    .then(stdlib => {
      const wagerInWei  = stdlib.toBN(stdlib.web3.toWei(wagerInEth,  'ether'));
      const escrowInWei = stdlib.toBN(stdlib.web3.toWei(escrowInEth, 'ether'));

      return { stdlib, gameState: { wagerInWei, escrowInWei }};
    });

const play = interactWith => ({ stdlib, gameState }) => {
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

  const bobShootScissors = ctcAlice =>
    new Promise(resolve =>
      gameState.bob.attach(gameState.ctors, ctcAlice.address)
     .then(ctcBob => RPS.B(stdlib, ctcBob, interactWith('Bob','SCISSORS'), resolve)));

  const aliceShootRock = ctc =>
    new Promise(resolve =>
      RPS.A(stdlib, ctc, interactWith('Alice','ROCK'), wagerInWei, escrowInWei, resolve));

  return Promise.all([ newPlayer(), newPlayer() ])
    .then(captureOpeningGameState)
    .then(()  => gameState.alice.deploy(gameState.ctors))
    .then(ctc => Promise.all([ bobShootScissors(ctc), aliceShootRock(ctc) ]))
    .then(captureClosingGameState);
};

export const runGameWith = (interactWith, wagerInEth, escrowInEth, uri) =>
  init(wagerInEth, escrowInEth, uri)
    .then(play(interactWith));
