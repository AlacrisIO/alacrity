// vim: filetype=javascript

import '../monkey-patch-require.js';
import * as A                  from '../alacrity-runtime.mjs';
import { web3 }                from '../web3-prelude.mjs';
import { contractFactoryCode } from '../build/contract-manual.mjs';


jasmine.DEFAULT_TIMEOUT_INTERVAL = 60 * 1000;


const panic = m =>
  console.error(m) || process.exit(1);

const balanceOf = a =>
  web3.eth.getBalance(a);


// This matches the logic in legicash-facts'
// src/legilogic_ethereum/ethereum_transaction.ml:get_first_account function
// (which is what the prefunder script uses)
const PREFUNDED_PRIVATE_NET_ACCT =
  web3.personal.listAccounts[0] || panic('Cannot infer prefunded account!');

const accts    = {};
const contract = {};


// Upserts (nickname, hex ID) record
const createAndUnlock = acctNames => {
  const created = u =>
    new Promise((resolve, reject) =>
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
     , value: A.ethToWei(100)
     });

  const prefunded = acct =>
    new Promise((resolve, reject) =>
      web3.eth.sendTransaction(newTxFor(acct), e =>
        !!e ? reject(e)
            : resolve()));

  return Promise.all(Object.values(accts).map(prefunded));
};


const deployContractFactory = () =>
  new Promise((resolve, reject) => {

    // TODO Fare allowed this to be configurable before; reimplement?
    const BLOCK_POLLING_PERIOD_IN_SECONDS = 1;

    const o =
      { from: PREFUNDED_PRIVATE_NET_ACCT
      , data: contractFactoryCode
      , gas:  web3.eth.estimateGas({ data: contractFactoryCode })
      };

    const k = f => (err, ...d) =>
      !!err ? reject(err)
            : f(...d);

    const gatherContractInfo = txHash =>
      web3.eth.getTransactionReceipt(txHash, k(receipt => {
        if (receipt.transactionHash !== txHash)
          return reject(`Bad txHash; ${txHash} !== ${receipt.transactionHash}`);

        contract.address       = receipt.contractAddress;
        contract.codeHash      = A.digestHex(contractFactoryCode);
        contract.creationBlock = receipt.blockNumber;
        contract.creationHash  = receipt.transactionHash;

        return resolve(contract);
      }));

    const awaitConfirmation = txHash => {
      const clearAndReject = err =>
        clearInterval(i) || reject(err);

      const clearAndGather = () =>
        clearInterval(i) || gatherContractInfo(txHash);

      // A null `t` or `t.blockNumber` means the tx hasn't been confirmed yet
      const query = () => web3.eth.getTransaction(txHash, (err, t) =>
          !!err                  ? clearAndReject(err)
        : !!t && !!t.blockNumber ? clearAndGather()
        : null);

      const i = setInterval(query, BLOCK_POLLING_PERIOD_IN_SECONDS * 1000);
    };

    return web3.eth.sendTransaction(o, k(awaitConfirmation));
  });


const runPrep = done => {
  // Web3's internals will break without this:
  web3.eth.defaultAccount = PREFUNDED_PRIVATE_NET_ACCT;

  return createAndUnlock([ 'alice', 'bob' ])
    .then(prefundTestAccounts)
    .then(deployContractFactory)
    .then(done)
    .catch(panic)
};


describe('The test suite can easily', () => {
  beforeAll(runPrep);

  describe('create + prefund new test accounts', () => {

    it('which can exchange funds', done => {
      const balanceStartAlice = balanceOf(accts.alice);
      const balanceStartBob   = balanceOf(accts.bob);

      const value = 50000;
      const tx    = { from: accts.alice, to: accts.bob, value };

      web3.eth.sendTransaction(tx, () => {
        expect(balanceOf(accts.alice)).toBeLessThan(balanceStartAlice - value);
        expect(balanceOf(accts.bob)).toBeGreaterThan(balanceStartBob + value);

        done();
      });
    });
  });
});


xdescribe('A rock/paper/scissors game', () => {
  beforeAll(runPrep);

  describe('results in', () => {

    it('both participants agreeing on who won', () => {
    });

    it('the winner\'s balance being increased + loser\'s balance being reduced by wager', done => {
      // TODO
      // Switch to Alice account before creating new game
      // Infer game ID
      // Switch to Bob account and accept
      // Await confirmation of game completion on chain
      // Look up Alice's new balance
      // Look up Bob's new balance
      // Compare and assert balances
      // Salt generation must be fixed for `node`
      const balanceStartAlice = balanceOf(accts.alice);
      const balanceStartBob   = balanceOf(accts.bob);
      const wagerInWei        = A.ethToWei(1.5);
      const escrowInWei       = wagerInWei.div(10);

      done();

    });
  });
});
