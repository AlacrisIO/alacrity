// vim: filetype=javascript
jasmine.DEFAULT_TIMEOUT_INTERVAL = 60 * 1000 * 10;

import * as RPS        from '../../../../build/rps.mjs';
import { stdlibNode  } from '../../../stdlib/web3/node.mjs';
import { runGameWith } from '../../../demo.mjs';


const uri = process.env.ETH_NODE_URI || 'http://localhost:8545';


describe('A rock/paper/scissors game using the `web3` stdlib', () => {
  let web3, toBN, balanceOf;

  beforeAll(done => stdlibNode(RPS.ABI, RPS.Bytecode, uri)
    .then(l => { web3      = l.web3;
                 toBN      = l.toBN;
                 balanceOf = l.balanceOf; })
    .then(done));

  describe('results in', () => {
    const interactWith = (name, hand) => (a, cb) =>
          { const res = a === 'getHand' ? hand
                  : null;
              return cb(res); };

    const wagerInEth  = 1.5;
    const escrowInEth = 0.15;

    it('both participants agreeing on who won', done =>
      runGameWith(interactWith, wagerInEth, escrowInEth)
        .then(({ outcomeAlice, outcomeBob }) => expect(outcomeAlice.eq(outcomeBob)).toBe(true))
        .then(done));

    it('the winner\'s balance being increased + loser\'s balance being reduced by wager', done =>
      runGameWith(interactWith, wagerInEth, escrowInEth)
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
