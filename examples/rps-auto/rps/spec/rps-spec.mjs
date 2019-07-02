// vim: filetype=javascript
jasmine.DEFAULT_TIMEOUT_INTERVAL = 60 * 1000 * 10;

import * as RPS       from '../../build/rps.mjs';
import { stdlibNode } from '../alacrity-runtime.mjs';

const stdlib = stdlibNode(RPS.ABI, RPS.Bytecode);
const { EthereumNetwork, balanceOf, web3, panic, toBN } = stdlib;


const interact = () => null;


// This matches the logic in legicash-facts'
// src/legilogic_ethereum/ethereum_transaction.ml:get_first_account function
// (which is what the prefunder script uses)
const PREFUNDED_PRIVATE_NET_ACCT =
  web3.personal.listAccounts[0] || panic('Cannot infer prefunded account!');


const accts = {};

// Upserts (nickname, hex ID) record
const createAndUnlock = acctNames => {
  const created = u =>
    new Promise(resolve =>
      web3.personal.newAccount((z, i) => {
        accts[u] = i;
        return web3.personal.unlockAccount(i, resolve);
      }));

  return Promise.all(acctNames.map(created));
};


const prefundTestAccounts = () => {
  // https://github.com/ethereum/wiki/wiki/JavaScript-API#web3ethsendtransaction
  const newTxFor = to =>
    ({ to
     , from:  PREFUNDED_PRIVATE_NET_ACCT
     , value: web3.toWei(100, 'ether')
     });

  const prefunded = acct =>
    new Promise((resolve, reject) =>
      web3.eth.sendTransaction(newTxFor(acct), e =>
        !!e ? reject(e)
            : resolve()));

  return Promise.all(Object.values(accts).map(prefunded));
};


const runPrep = done => {
  // Web3's internals will break without this:
  web3.eth.defaultAccount = PREFUNDED_PRIVATE_NET_ACCT;

  return createAndUnlock([ 'alice', 'bob' ])
    .then(prefundTestAccounts)
    .then(done)
    .catch(panic);
};

const wagerInWei  = toBN(web3.toWei(1.5, 'ether'));
const escrowInWei = wagerInWei.div(10);

const runGameWith = (alice, bob) => {
  const ctors = [ accts.alice, accts.bob ];

  const bobShootScissors = ctcAlice =>
    new Promise(resolve =>
      bob.attach(ctcAlice.abi, ctcAlice.bytecode, ctors, ctcAlice.address)
        .then(ctcBob => RPS.B(stdlib, ctcBob, interact, 2, resolve)));

  const aliceShootRock = ctc =>
    new Promise(resolve =>
      RPS.A(stdlib, ctc, interact, wagerInWei, escrowInWei, 0, resolve));

  return alice
    .deploy(ctors)
    .then(ctc => Promise.all([ bobShootScissors(ctc), aliceShootRock(ctc) ]));
};


describe('A rock/paper/scissors game', () => {
  beforeAll(runPrep);

  describe('results in', () => {

    it('both participants agreeing on who won', done =>
      runGameWith(EthereumNetwork(accts.alice), EthereumNetwork(accts.bob))
        .then(([ bobOutcome, aliceOutcome ]) => expect(bobOutcome).toEqual(aliceOutcome))
        .then(done));

    it('the winner\'s balance being increased + loser\'s balance being reduced by wager', done => {
      const alice = EthereumNetwork(accts.alice);
      const bob   = EthereumNetwork(accts.bob);

      const balanceStartAlice = balanceOf(alice);
      const balanceStartBob   = balanceOf(bob);

      return runGameWith(alice, bob)
        .then(() => expect(balanceOf(alice)).toBeGreaterThan(balanceStartAlice + wagerInWei))
        .then(() => expect(balanceOf(bob  )).toBeLessThan(   balanceStartBob   - wagerInWei))
        .then(done);
    });
  });
});
