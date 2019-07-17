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

    it('both participants agreeing on who won and the winner\'s balance being increased + loser\'s balance being reduced by wager', done =>
      runGameWith(interactWith, wagerInEth, escrowInEth)
       .then(({ alice, bob, balanceStartAlice, balanceStartBob, wagerInWei, outcomeAlice, outcomeBob }) => {
           expect(outcomeAlice === outcomeBob).toBe(true);

           // "The Man" always gets his cut regardless - this is just a
           // rough guesstimate of processing fees
           const estimatedGas = toBN(web3.toWei('5000000', 'wei'));
           console.log(`before bs`);
           const balanceStart = {[alice]: balanceStartAlice,
                                 [bob]: balanceStartBob};

           const winner = outcomeAlice === 'Alice wins' ? alice : bob;
           const loser = outcomeAlice === 'Alice wins' ? bob : alice;
           const drawp = outcomeAlice === 'Draw';

           if ( !drawp ) {
               const winnerGte = balanceOf(winner).gte(
                   balanceStart[winner].plus(wagerInWei)
                       .minus(estimatedGas));

               const loserLt = balanceOf(loser).lt(
                   balanceStart[loser].minus(wagerInWei));

               expect(winnerGte).toBe(true);
               expect(loserLt   ).toBe(true); }
           else {
               const check = (who) => {
                   expect(balanceOf(who).gte(balanceStart[who].minus(estimatedGas))).toBe(true); };
               check(alice);
               check(bob);
           }
       })
       .then(done));
  });
});
