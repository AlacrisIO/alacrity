// vim: filetype=javascript

import Web3       from 'web3';
import * as C     from '../../build/contract.mjs';
import * as RPS   from '../rps.ala.mjs';
import { stdlib } from '../alacrity-runtime.mjs';

jasmine.DEFAULT_TIMEOUT_INTERVAL = 60 * 1000 * 10;

const panic = m =>
  console.error(m) || process.exit(1);

const balanceOf = a =>
  a.web3.eth.getBalance(a.userAddress);

const interact = () => null;


// This matches the logic in legicash-facts'
// src/legilogic_ethereum/ethereum_transaction.ml:get_first_account function
// (which is what the prefunder script uses)
const PREFUNDED_PRIVATE_NET_ACCT =
  stdlib.web3.personal.listAccounts[0] || panic('Cannot infer prefunded account!');

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
  (funcName, args, gas, cb) =>
    // https://github.com/ethereum/wiki/wiki/JavaScript-API#contract-methods
    web3.eth.contract(contractAbi)
            .at(address)
            [funcName]
            .sendTransaction(player1Addr, player2Addr, ...args, { from, gas }, (err, ...s) =>
              !!err ? panic(err)
                    : cb(...s));

// TODO FIXME
// TODO stop watching for new events once the first one has been handled
const mkRecv = (web3, contractAbi, address, creationBlock) =>
  (eventName, cb) =>
    // https://github.com/ethereum/wiki/wiki/JavaScript-API#contract-events
    web3.eth.contract(contractAbi)
            .at(address)
            [eventName]
            ({}, { fromBlock: creationBlock, toBlock: 'latest' })
            .get((err, ...s) =>
              !!err ? panic(err)
                    : cb(...s));

// TODO FIXME
const Contract = (web3, userAddress) =>
    (address, codeHash, creationBlock, creationHash, player1Addr, player2Addr, contractAbi) =>
  ({ address
   , codeHash
   , creationBlock
   , creationHash
   , player1Addr
   , player2Addr
   , contractAbi
   , send: mkSend(web3, contractAbi, address, userAddress, player1Addr, player2Addr)
   , recv: mkRecv(web3, contractAbi, address, creationBlock)
   });


const deployContractWith = (web3, userAddress) =>
    ( contractAbi
    , contractCode
    , player1Addr
    , player2Addr
    , blockPollingPeriodInSeconds = 1
    ) =>

  new Promise((resolve, reject) => {

    const o =
      { from: userAddress
      , data: contractCode
      , gas:  web3.eth.estimateGas({ data: contractCode })
      };

    const k = f => (err, ...d) =>
      !!err ? reject(err)
            : f(...d);

    const gatherContractInfo = txHash =>
      web3.eth.getTransactionReceipt(txHash, k(receipt => {
        if (receipt.transactionHash !== txHash)
          return reject(`Bad txHash; ${txHash} !== ${receipt.transactionHash}`);

        return resolve(Contract(web3, userAddress)
          ( receipt.contractAddress        // address
          , stdlib.digestHex(contractCode) // codeHash
          , receipt.blockNumber            // creationBlock
          , receipt.transactionHash        // creationHash
          , player1Addr
          , player2Addr
          , contractAbi
          ));
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

      const i = setInterval(query, blockPollingPeriodInSeconds * 1000);
    };

    return web3.eth.sendTransaction(o, k(awaitConfirmation));
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
    .catch(panic);
};

const runGameWith = (alice, bob) => {
  // TODO FIXME Larger values result in `Error: invalid argument 0: json:
  // cannot unmarshal hex number > 64 bits into Go value of type
  // hexutil.Uint64`
  const wagerInWei = stdlib.web3.toWei(1000, 'wei');

  const escrowInWei = stdlib.web3
    .toBigNumber(wagerInWei)
    .div(10)
    .toString();

  const bobShootScissors = ctc =>
    new Promise(resolve =>
      bob.attach(C.contractAbi, C.contractCode, accts.alice, accts.bob, ctc.addr, c =>
        RPS.B(ctc, interact, 2, resolve)));

  const aliceShootRock = ctc =>
    new Promise(resolve =>
      RPS.A(ctc, interact, wagerInWei, escrowInWei, 0, resolve));

  return alice
    .deploy(C.contractAbi, C.contractCode, accts.alice, accts.bob)
    .then(ctc => [ true, true ]);
    // TODO FIXME
    // .then(ctc => Promise.all([ bobShootScissors(ctc), aliceShootRock(ctc) ]));
};


describe('A rock/paper/scissors game', () => {
  beforeAll(runPrep);

  describe('results in', () => {

    it('both participants agreeing on who won', done =>
      runGameWith(EthereumNetwork(stdlib.web3, accts.alice), EthereumNetwork(stdlib.web3, accts.bob))
        .then(([ bobOutcome, aliceOutcome ]) => expect(bobOutcome).toEqual(aliceOutcome))
        .then(done));

    xit('the winner\'s balance being increased + loser\'s balance being reduced by wager', done => {
      const alice = EthereumNetwork(stdlib.web3, accts.alice);
      const bob   = EthereumNetwork(stdlib.web3, accts.bob);

      const balanceStartAlice = balanceOf(alice);
      const balanceStartBob   = balanceOf(bob);

      return runGameWith(alice, bob)
        .then(() => expect(balanceOf(alice)).toBeLessThan(balanceStartAlice))
        .then(() => expect(balanceOf(bob)).toBeGreaterThan(balanceStartBob))
        .then(done);
    });
  });
});
