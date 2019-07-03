// vim: filetype=javascript
jasmine.DEFAULT_TIMEOUT_INTERVAL = 60 * 1000 * 10;

import * as RPS       from '../../build/rps.mjs';
import { stdlibNode } from '../alacrity-runtime.mjs';

const stdlib = stdlibNode(RPS.ABI, RPS.Bytecode);
const { EthereumNetwork, web3, panic, toBN, balanceOf } = stdlib;


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


const runGame = () => {
  // This matches the logic in legicash-facts'
  // src/legilogic_ethereum/ethereum_transaction.ml:get_first_account function
  // (which is what the prefunder script uses)
  const prefundedPrivateNetAcct =
    web3.personal.listAccounts[0] || panic('Cannot infer prefunded account!');

  const wagerInWei  = toBN(web3.toWei(1.5, 'ether'));
  const escrowInWei = wagerInWei.div(10);
  const interact    = () => null;
  const gameState   = { wagerInWei, escrowInWei, interact };

  const newPlayer = () =>
    createAndUnlock()
      .then(to => fund(to, prefundedPrivateNetAcct, web3.toWei(100, 'ether')))
      .then(EthereumNetwork);

  const captureOpeningGameState = ([ a, b ]) =>
    Object.assign(gameState
               , { alice: a
                 , bob:   b
                 , ctors: [ a.userAddress, b.userAddress ]
                 , balanceStartAlice: balanceOf(a)
                 , balanceStartBob:   balanceOf(b)
                 });

  const bobShootScissors = ctcAlice =>
    new Promise(resolve =>
      gameState.bob.attach(gameState.ctors, ctcAlice.address)
        .then(ctcBob => RPS.B(stdlib, ctcBob, interact, 2, resolve)));

  const aliceShootRock = ctc =>
    new Promise(resolve =>
      RPS.A(stdlib, ctc, interact, wagerInWei, escrowInWei, 0, resolve));

  const captureClosingGameState = ([ outcomeBob, outcomeAlice ]) =>
    Promise.resolve(Object.assign(gameState, { outcomeAlice, outcomeBob }));

  return Promise.all([ newPlayer(), newPlayer() ])
    .then(captureOpeningGameState)
    .then(()  => gameState.alice.deploy(gameState.ctors))
    .then(ctc => Promise.all([ bobShootScissors(ctc), aliceShootRock(ctc) ]))
    .then(captureClosingGameState);
};


describe('A rock/paper/scissors game', () => {
  describe('results in', () => {

    it('both participants agreeing on who won', done =>
      runGame()
        .then(({ outcomeAlice, outcomeBob }) => expect(outcomeAlice.eq(outcomeBob)).toBe(true))
        .then(done));

    it('the winner\'s balance being increased + loser\'s balance being reduced by wager', done =>
      runGame()
        .then(({ alice, bob, balanceStartAlice, balanceStartBob, wagerInWei }) => {
          // "The Man" always gets his cut regardless - this is just a rough
          // guesstimate of processing fees
          const estimatedGas = toBN(web3.toWei('5000000', 'wei'));

          const aliceGte = balanceOf(alice).gte(
            balanceStartAlice.plus(wagerInWei)
                             .minus(estimatedGas));

          const bobLt = balanceOf(bob).lt(
            balanceStartBob.minus(wagerInWei));

          expect(aliceGte).toBe(true);
          expect(bobLt   ).toBe(true);
          done();
        }));
  });
});
