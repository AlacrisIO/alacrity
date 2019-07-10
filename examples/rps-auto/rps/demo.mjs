// vim: filetype=javascript

import * as RPS       from '../build/rps.mjs';
import { stdlibNode } from './alacrity-runtime.mjs';


export const runGameWith = (interactWith, wagerInEth, escrowInEth) => {
  const stdlib = stdlibNode(RPS.ABI, RPS.Bytecode);
  const { EthereumNetwork, web3, panic, toBN, balanceOf } = stdlib;

  // This matches the logic in legicash-facts'
  // src/legilogic_ethereum/ethereum_transaction.ml:get_first_account function
  // (which is what the prefunder script uses)
  const prefundedPrivateNetAcct =
    web3.personal.listAccounts[0] || panic('Cannot infer prefunded account!');

  const wagerInWei  = toBN(web3.toWei(wagerInEth,  'ether'));
  const escrowInWei = toBN(web3.toWei(escrowInEth, 'ether'));
  const gameState   = { wagerInWei, escrowInWei };

  const captureOpeningGameState = ([ a, b ]) =>
    Object.assign(gameState
               , { alice: a
                 , bob:   b
                 , ctors: [ a.userAddress, b.userAddress ]
                 , balanceStartAlice: balanceOf(a)
                 , balanceStartBob:   balanceOf(b)
                 });

  const createAndUnlock = () =>
    new Promise(resolve =>
      web3.personal.newAccount((z, i) =>
        web3.personal.unlockAccount(i, () => resolve(i))));

  // https://github.com/ethereum/wiki/wiki/JavaScript-API#web3ethsendtransaction
  const fund = (to, from, value) =>
    new Promise((resolve, reject) =>
      web3.eth.sendTransaction({ to, from, value }, e =>
        !!e ? reject(e)
            : resolve(to)));

  const newPlayer = () =>
    createAndUnlock()
      .then(to => fund(to, prefundedPrivateNetAcct, web3.toWei(100, 'ether')))
      .then(EthereumNetwork);

  const bobShootScissors = ctcAlice =>
    new Promise(resolve =>
      gameState.bob.attach(gameState.ctors, ctcAlice.address)
        .then(ctcBob => RPS.B(stdlib, ctcBob, interactWith('Bob'), 2, resolve)));

  const aliceShootRock = ctc =>
    new Promise(resolve =>
      RPS.A(stdlib, ctc, interactWith('Alice'), wagerInWei, escrowInWei, 0, resolve));

  const captureClosingGameState = ([ outcomeBob, outcomeAlice ]) =>
    Promise.resolve(Object.assign(gameState, { outcomeAlice, outcomeBob }));

  return Promise.all([ newPlayer(), newPlayer() ])
    .then(captureOpeningGameState)
    .then(()  => gameState.alice.deploy(gameState.ctors))
    .then(ctc => Promise.all([ bobShootScissors(ctc), aliceShootRock(ctc) ]))
    .then(captureClosingGameState);
};
