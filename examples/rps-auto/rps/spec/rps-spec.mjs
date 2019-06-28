// vim: filetype=javascript

import Web3       from 'web3';
import ethers     from 'ethers';
import * as RPS   from '../../build/rps.mjs';
import { stdlib } from '../alacrity-runtime.mjs';

jasmine.DEFAULT_TIMEOUT_INTERVAL = 60 * 1000 * 10;

const k = (reject, f) => (err, ...d) =>
  !!err ? reject(err)
        : f(...d);

const balanceOf = a =>
  a.web3.eth.getBalance(a.userAddress);

const interact = () => null;

// `t` is a type name in string form; `v` is the value to cast
const encode = (t, v) =>
  ethers.utils.defaultAbiCoder.encode([t], [v]);

// Left-padded w/ zeros to length of 64, no `0x` prefix
const encodePlayer = a =>
  encode('address', a).substring(2);


// This matches the logic in legicash-facts'
// src/legilogic_ethereum/ethereum_transaction.ml:get_first_account function
// (which is what the prefunder script uses)
const PREFUNDED_PRIVATE_NET_ACCT =
  stdlib.web3.personal.listAccounts[0] || stdlib.panic('Cannot infer prefunded account!');


const contractCodeWithCtors = (player1Addr, player2Addr) =>
  [ RPS.Bytecode
  , encodePlayer(player1Addr)
  , encodePlayer(player2Addr)
  ].join('');


const accts = {};

// Upserts (nickname, hex ID) record
const createAndUnlock = acctNames => {
  const created = u =>
    new Promise(resolve =>
      stdlib.web3.personal.newAccount((z, i) => {
        accts[u] = i;
        return stdlib.web3.personal.unlockAccount(i, resolve);
      }));

  return Promise.all(acctNames.map(created));
};


const prefundTestAccounts = () => {
  // https://github.com/ethereum/wiki/wiki/JavaScript-API#web3ethsendtransaction
  const newTxFor = to =>
    ({ to
     , from:  PREFUNDED_PRIVATE_NET_ACCT
     , value: stdlib.web3.toWei(100, 'ether')
     });

  const prefunded = acct =>
    new Promise((resolve, reject) =>
      stdlib.web3.eth.sendTransaction(newTxFor(acct), e =>
        !!e ? reject(e)
            : resolve()));

  return Promise.all(Object.values(accts).map(prefunded));
};


const mkSend = (web3, contractAbi, address, from, player1Addr, player2Addr) =>
  (funcName, args, value, cb) => {
    const txObj = { from, value };

    const afterward = k(stdlib.panic, txHash =>
      awaitConfirmation(web3, txHash)
        .then(() => txReceiptFor(web3, txHash))
        .then(cb)
        .catch(stdlib.panic));

    // https://github.com/ethereum/wiki/wiki/JavaScript-API#contract-methods
    return web3.eth
      .contract(contractAbi)
      .at(address)
      [funcName]
      .sendTransaction(player1Addr
                     , player2Addr
                     , ...args
                     , txObj
                     , afterward);
  };


// https://docs.ethers.io/ethers.js/html/api-contract.html#configuring-events
const mkRecv = (web3, contractAbi, address) => (eventName, cb) =>
  new ethers
    .Contract(address, contractAbi, new ethers.providers.Web3Provider(web3.currentProvider))
    .on(eventName, (...a) => {
      const b = a.map(b => b); // Preserve `a` w/ copy
      const e = b.pop();       // The final element represents an `ethers` event object

      // Swap ethers' BigNumber wrapping for web3's
      const bns = b.map(x => stdlib.toBN(x.toString()));

      // TODO FIXME replace arbitrary delay with something more intelligent to
      // mitigate mystery race condition
      web3.eth.getTransaction(e.transactionHash, k(stdlib.panic, t =>
        setTimeout(() => cb(...bns, t.value), 2000)));
    });

const Contract = (web3, userAddress) => (abi, code, player1Addr, player2Addr, address) =>
  ({ abi
   , code
   , player1Addr
   , player2Addr
   , address
   , send: mkSend(web3, abi, address, userAddress, player1Addr, player2Addr)
   , recv: mkRecv(web3, abi, address)
   });


const awaitConfirmation = (web3, txHash, blockPollingPeriodInSeconds = 1) =>
  new Promise((resolve, reject) => {
    // A null `t` or `t.blockNumber` means the tx hasn't been confirmed yet
    const query = () => web3.eth.getTransaction(txHash, (err, t) =>
        !!err                  ? clearInterval(i) || reject(err)
      : !!t && !!t.blockNumber ? clearInterval(i) || resolve(txHash)
      : null);

    const i = setInterval(query, blockPollingPeriodInSeconds * 1000);
  });


const txReceiptFor = (web3, txHash) =>
  new Promise((resolve, reject) =>
    web3.eth.getTransactionReceipt(txHash, (err, r) =>
        !!err                        ? reject(err)
      : r.transactionHash !== txHash ? reject(`Bad txHash; ${txHash} !== ${receipt.transactionHash}`)
      : r.status          !== '0x1'  ? reject(`Transaction: ${txHash} failed for unspecified reason`)
      : resolve(r)));


const deployContractWith = (web3, userAddress) =>
    ( contractAbi
    , contractCode
    , player1Addr
    , player2Addr
    , blockPollingPeriodInSeconds = 1
    ) =>
  new Promise((resolve, reject) => {

    const o =
      { data: contractCode
      , from: userAddress
      , gas:  web3.eth.estimateGas({ data: contractCode })
      };

    const gatherContractInfo = txHash =>
      txReceiptFor(web3, txHash)
        .then(r => resolve(Contract(web3, userAddress)
          ( contractAbi
          , contractCode
          , player1Addr
          , player2Addr
          , r.contractAddress
          )))
        .catch(reject);

    return web3.eth.sendTransaction(o, k(reject, txHash =>
      awaitConfirmation(web3, txHash, blockPollingPeriodInSeconds)
        .then(gatherContractInfo)));
  });


const EthereumNetwork = (web3, userAddress) =>
  ({ deploy: deployContractWith(web3, userAddress)
   , attach: (...a) => Promise.resolve(Contract(web3, userAddress)(...a))
   , web3
   , userAddress
   });


const runPrep = done => {
  // Web3's internals will break without this:
  stdlib.web3.eth.defaultAccount = PREFUNDED_PRIVATE_NET_ACCT;

  return createAndUnlock([ 'alice', 'bob' ])
    .then(prefundTestAccounts)
    .then(done)
    .catch(stdlib.panic);
};

const wagerInWei  = stdlib.toBN(stdlib.web3.toWei(1.5, 'ether'));
const escrowInWei = wagerInWei.div(10);

const runGameWith = (alice, bob) => {
  const contractCode = contractCodeWithCtors(accts.alice, accts.bob);

  const bobShootScissors = ctcAlice =>
    new Promise(resolve =>
      bob.attach(ctcAlice.abi, ctcAlice.code, accts.alice, accts.bob, ctcAlice.address)
        .then(ctcBob => RPS.B(stdlib, ctcBob, interact, 2, resolve)));

  const aliceShootRock = ctc =>
    new Promise(resolve =>
      RPS.A(stdlib, ctc, interact, wagerInWei, escrowInWei, 0, resolve));

  return alice
    .deploy(RPS.ABI, contractCode, accts.alice, accts.bob)
    .then(ctc => Promise.all([ bobShootScissors(ctc), aliceShootRock(ctc) ]));
};


describe('A rock/paper/scissors game', () => {
  beforeAll(runPrep);

  describe('results in', () => {

    it('both participants agreeing on who won', done =>
      runGameWith(EthereumNetwork(stdlib.web3, accts.alice), EthereumNetwork(stdlib.web3, accts.bob))
        .then(([ bobOutcome, aliceOutcome ]) => expect(bobOutcome).toEqual(aliceOutcome))
        .then(done));

    it('the winner\'s balance being increased + loser\'s balance being reduced by wager', done => {
      const alice = EthereumNetwork(stdlib.web3, accts.alice);
      const bob   = EthereumNetwork(stdlib.web3, accts.bob);

      const balanceStartAlice = balanceOf(alice);
      const balanceStartBob   = balanceOf(bob);

      return runGameWith(alice, bob)
        .then(() => expect(balanceOf(alice)).toBeGreaterThan(balanceStartAlice + wagerInWei))
        .then(() => expect(balanceOf(bob  )).toBeLessThan(   balanceStartBob   - wagerInWei))
        .then(done);
    });
  });
});
