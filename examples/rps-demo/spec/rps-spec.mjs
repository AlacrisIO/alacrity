// vim: filetype=javascript

import '../monkey-patch-require.js';
import '../rps-demo.mjs'; // Trigger effectful module registration
import { web3          } from '../web3-prelude.mjs';
import { ethToWei      } from '../common-runtime.mjs';
import { createNewGame } from '../dapp-backend.mjs';

const panic = m =>
  console.error(m) || process.exit(1);

// This matches the logic in legicash-facts'
// src/legilogic_ethereum/ethereum_transaction.ml:get_first_account function
// (which is what the prefunder script uses)
const PREFUNDED_PRIVATE_NET_ACCT =
  web3.personal.listAccounts[0] || panic('Cannot infer prefunded account!');

// See config/demo-keys-small.json in legicash-facts
const ALICE = '0xc54e86dffb87b9736e2e35dd85c775358f1c31ce';
const BOB   = '0x9ccaed210ce8c0cb49c5ad1c4f583406c264ba69';


const prefundAliceAndBob = () => {
  // https://github.com/ethereum/wiki/wiki/JavaScript-API#web3ethsendtransaction
  const newTxFor = to =>
    ({ to
     , from:  PREFUNDED_PRIVATE_NET_ACCT
     , value: ethToWei(100)
     });

  const prefunded = acct =>
    new Promise((resolve, reject) =>
      web3.eth.sendTransaction(newTxFor(acct), e =>
        !!e ? reject(e)
            : resolve()));

  return Promise.all([ prefunded(ALICE), prefunded(BOB) ]);
};

describe('A rock/paper/scissors game', () => {

  beforeAll(done => {
    // Web3's internals will break without this:
    web3.eth.defaultAccount = PREFUNDED_PRIVATE_NET_ACCT;

    prefundAliceAndBob()
      .then(done)
      .catch(panic)
  });

  describe('results in', () => {

    it('both participants agreeing on who won', () => {
    });

    it('the winner\'s balance being increased + loser\'s balance being reduced by wager', () => {
      const wagerInWei  = ethToWei(1.5);
      const escrowInWei = wagerInWei.div(10);
      createNewGame(wagerInWei, escrowInWei, BOB, 1);
    });
  });
});
