// vim: filetype=javascript




const panic = e => {
    console.log('panic with e=', e);
//    process.abort();
    throw Error(e);
    };
//    throw Error(e); };

const k = (reject, f) => (err, ...d) =>
  !!err ? reject(err)
        : f(...d);

const flip = (f, a, b) =>
  f(b, a);

const un0x           = h => h.replace(/^0x/, '');
const hexTo0x        = h => '0x' + h.replace(/^0x/, '');
const byteToHex      = b => (b & 0xFF).toString(16).padStart(2, '0');
const byteArrayToHex = b => Array.from(b, byteToHex).join('');


const nat_to_fixed_size_hex = size => n => {
  const err = m => panic(`nat_to_fixed_size_hex: ${m}`);

  const notNat = !(Number.isInteger(n) && 0 <= n);
  const tooBig = !(Math.ceil(Math.log2(n + 1) / 8) <= size);

  return notNat ? err(`expected a nat`)
       : tooBig ? err(`expected a nat that fits into ${size} bytes`)
       : n.toString(16).padStart((2 * size), '0');
};

// Encodes a 16-bit unsigned integer as 2 hex bytes or 4 hex characters
const nat16_to_fixed_size_hex =
  nat_to_fixed_size_hex(2);


// Parameterized ///////////////////////////////////////////////////////////////

const balanceOf = A => a =>
  a.web3.eth.getBalance(a.userAddress)
    .then(toBN(A));

const assert = ({ asserter }) => d => asserter(d);

const toWei     = ({ web3 }) => web3.utils.toWei;
const toBN      = ({ web3 }) => web3.utils.toBN;
const isBN      = ({ web3 }) => web3.utils.isBN;
const keccak256 = ({ web3 }) => web3.utils.keccak256;

const hexToBN          = A => h => toBN(A)(hexTo0x(h));
const uint256_to_bytes = A => i => bnToHex(A)(i);


const bnToHex = A => (u, size = 32) =>
  toBN(A)(u)
    .toTwos(8 * size)
    .toString(16, 2 * size);


const hexOf = ({ web3 }) => x =>
  typeof x === 'string' && x.slice(0, 2) === '0x'
    ? un0x(web3.utils.toHex(x))
    : un0x(web3.utils.toHex(`0x${x}`));


const bytes_eq = A => (x, y) =>
  hexOf(A)(x) === hexOf(A)(y);


// TODO `Number.isInteger` probably isn't sufficient on its own here
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isInteger#Description
const bytes_len = A => b => {
  const bh = hexOf(A)(b);
  const n  = bh.length / 2;

  return !Number.isInteger(n)
    ? panic(`Invalid byte string: ${bh}`)
    : n;
};


// ∀ a b, msg_left (msg_cat(a, b)) = a
// ∀ a b, msg_right(msg_cat(a, b)) = b
const bytes_cat = A => (a, b) => {
  const ah = hexOf(A)(a);
  const bh = hexOf(A)(b);
  const n  = nat16_to_fixed_size_hex(bytes_len(A)(ah));

  return '0x' +  n + ah + bh;
};


const random_uint256 = A => () =>
  hexToBN(A)(byteArrayToHex(A.random32Bytes()));


const equal = A => (a, b) => toBN(A)(a).eq( toBN(A)(b));
const add   = A => (a, b) => toBN(A)(a).add(toBN(A)(b));
const sub   = A => (a, b) => toBN(A)(a).sub(toBN(A)(b));
const mod   = A => (a, b) => toBN(A)(a).mod(toBN(A)(b));
const mul   = A => (a, b) => toBN(A)(a).mul(toBN(A)(b));
const ge    = A => (a, b) => toBN(A)(a).gte(toBN(A)(b));
const gt    = A => (a, b) => toBN(A)(a).gt( toBN(A)(b));
const le    = A => (a, b) => toBN(A)(a).lte(toBN(A)(b));
const lt    = A => (a, b) => toBN(A)(a).lt( toBN(A)(b));


function sleep(milliseconds) {
  var start = new Date().getTime();
  for (var i = 0; i < 1e7; i++) {
    if ((new Date().getTime() - start) > milliseconds){
      break;
    }
  }
}



// `t` is a type name in string form; `v` is the value to cast
const encode = ({ ethers }) => (t, v) =>
  ethers.utils.defaultAbiCoder.encode([t], [v]);


const rejectInvalidReceiptFor = txHash => r =>
    !r                           ? Promise.reject(`No receipt for txHash: ${txHash}`)
  : r.transactionHash !== txHash ? Promise.reject(`Bad txHash; ${txHash} !== ${r.transactionHash}`)
  : !r.status                    ? Promise.reject(`Transaction: ${txHash} was reverted by EVM\n${r}`) // eslint-disable-line max-len
  : Promise.resolve(r);


const fetchAndRejectInvalidReceiptFor = ({ web3 }) => txHash =>
  web3.eth.getTransactionReceipt(txHash)
    .then(rejectInvalidReceiptFor(txHash));


// https://web3js.readthedocs.io/en/v1.2.0/web3-eth.html#sendtransaction
const transfer = ({ web3 }) => (to, from, value) =>
  web3.eth.sendTransaction({ to, from, value });


// https://web3js.readthedocs.io/en/v1.2.0/web3-eth.html#getblocknumber
const now = ({ web3 }) =>
  web3.eth.getBlockNumber;


// https://web3js.readthedocs.io/en/v1.2.0/web3-eth-contract.html#web3-eth-contract
const mkSendRecv = A => (contractaddress, from, ctors) =>
  (label, funcName, args, value, eventName, cb) => {
  // https://github.com/ethereum/web3.js/issues/2077
  console.log('mkSendRecv: step 1, funcName=', funcName, ' from=', from);
  const munged = [ ...ctors, ...args ]
    .map(m => isBN(A)(m) ? m.toString() : m);
  console.log('mkSendRecv: step 2');

  return new A.web3.eth.Contract(A.abi, contractaddress)
    .methods[funcName](...munged)
    .send({ from, value })
    .then(r  => fetchAndRejectInvalidReceiptFor(A)(r.transactionHash))
    // XXX We may need to actually see the event. I don't know if the
    //     transaction confirmation is enough.
    // XXX Replace 0 below with the contract's balance
    .then(() => {
      console.log('mkSendRecv: Before the callback');
      cb({ value: value, balance: 0 });
    });


};


const consumedEventKeyOf = (name, e) =>
  `${name}:${e.blockNumber}:${e.transactionHash}`;


// https://docs.ethers.io/ethers.js/html/api-contract.html#configuring-events
const mkRecv = ({ web3, ethers }) => (c, w) => (label, eventName, cb) => {
    console.log('mkRecv step 1');
  const consume = (e, bns, resolve, reject) =>
    fetchAndRejectInvalidReceiptFor({ web3 })(e.transactionHash)
      .then(() => web3.eth.getTransaction(e.transactionHash, k(reject, t => {
        const key = consumedEventKeyOf(eventName, e);
          console.log('consume, step 1');
        if (w.alreadyConsumed || (c.consumedEvents[key] !== undefined))
          return reject(`${label} has already consumed ${key}!`);
          console.log('consume, step 2');

        // Sanity check: events ought to be consumed monotonically
        const latestPrevious = Object.values(c.consumedEvents)
          .filter(x => x.eventName === eventName)
          .sort((x, y) => x.blockNumber - y.blockNumber)
          .pop();
          console.log('consume, step 3');

        if (!!latestPrevious && latestPrevious.blockNumber >= e.blockNumber) {
          reject(`${label} attempted to consume ${eventName} out of sequential block # order!`);
        }
          console.log('consume, step 4');

        w.alreadyConsumed = true;
        Object.assign(c.consumedEvents, { [key]: Object.assign({}, e, { eventName }) });
          console.log('consume, step 5');

        resolve();
          console.log('consume, step 6');
        // XXX Replace 0 below with the contract's balance
        cb(...bns, { value: t.value, balance: 0 });
      })));
    console.log('mkRecv step 2');


  const past = () => new Promise((resolve, reject) =>
    new web3.eth.Contract(c.abi, c.contractaddress)
      .getPastEvents(eventName, { toBlock: 999999999 }) // TODO `toBlock`
      .then(es => {
          console.log('past, step 1 es=', es);
        const e = es
          .find(x => c.consumedEvents[consumedEventKeyOf(eventName, x)] === undefined);
          console.log('past, step 2 e=', e, ' eventName=', eventName);
          console.log('|c.consumedEvents|=', Object.keys(c.consumedEvents).length);

        if (!e) {
          console.log('Before reject statement');
          return reject();
          }
          console.log('past, step 3');

        const argsAbi = c.abi
          .find(a => a.name === eventName)
          .inputs;
          console.log('past, step 4');

        const decoded = web3.eth.abi.decodeLog(argsAbi, e.raw.data, e.raw.topics);
          console.log('past, step 5');

        const bns = argsAbi
          .map(a => a.name)
          .map(n => decoded[n]);
          console.log('past, step 6');

        return consume(e, bns, resolve, reject);
      }));
    console.log('mkRecv step 3');


  const pollPast = () => new Promise(resolve => {
      console.log('pollpast, step 1');
    const attempt = () => past()
      .then(resolve)
      .catch(() => flip(setTimeout, 500, () => !w.alreadyConsumed && attempt()));
      console.log('pollpast, step 2');
    sleep(1000);
    return attempt();
  });
    console.log('mkRecv step 4');


  const next = () => new Promise((resolve, reject) => new ethers
    .Contract(c.contractaddress, c.abi, new ethers.providers.Web3Provider(web3.currentProvider))
    .once(eventName, (...a) => {
      console.log('next, step 1');
      const b = a.map(b => b); // Preserve `a` w/ copy
      console.log('next, step 2');
      const e = b.pop();       // The final element represents an `ethers` event object
      console.log('next, step 3');

      // Swap ethers' BigNumber wrapping for web3's
      const bns = b.map(x => toBN({ web3 })(x.toString()));
      console.log('next, step 4');

      return consume(e, bns, resolve, reject);
    }));
    console.log('mkRecv step 5');


  return past()
    .catch(() =>
        Promise.race([ pollPast(), next() ])
    .catch(e => panic(`Error with mkRecv e=${e} End of error`)));
};


const mkRecvWithin = A => c => (label, eventName, blocks, tcb, cb) => {
  console.log('mkRecvWithin, step 1 blocks=', blocks);
  if (blocks < 0)
    return Promise.reject('`blocks` cannot be less than 0!');

  console.log('mkRecvWithin, step 2');
  const w = { alreadyConsumed: false };
  console.log('mkRecvWithin, step 3');

  const runAlternativeAfter = timeoutBlock => new Promise((resolve, reject) => {
    console.log('runAlternativelyAfter, step 1');
    const subs = new A.ethers.providers.Web3Provider(A.web3.currentProvider);
    console.log('runAlternativelyAfter, step 2');

    subs.on('error', e => {
      subs.removeAllListeners('block');
      reject(e);
    });
    console.log('runAlternativelyAfter, step 3');

    subs.on('block', n => {
      console.log('runAlternativelyAfter, step 1');
      if (w.alreadyConsumed)
        return subs.removeAllListeners('block');

      console.log('runAlternativelyAfter, step 2');
      if (n < timeoutBlock)
        return;

      console.log('runAlternativelyAfter, step 3');
      w.alreadyConsumed = true;
      console.log('runAlternativelyAfter, step 4');
      subs.removeAllListeners('block');
      console.log('runAlternativelyAfter, step 5');
      resolve();
      console.log('runAlternativelyAfter, step 6');
      tcb();
    });
    console.log('runAlternativelyAfter, step 4');
  });
  console.log('mkRecvWithin, step 4, eventName=', eventName);

  const raceUntil = timeoutBlock =>
    Promise.race([ mkRecv(A)(c, w)(label, eventName, cb)
                 , runAlternativeAfter(timeoutBlock) ]);
  console.log('mkRecvWithin, step 5');

  return now(A)()
    .then(currentBlock => raceUntil(currentBlock + blocks))
    .catch(e => panic(`Error with mkRecvWithin e=${e} End of error`));
};


const mkTimeoutTerminate = A => (contractaddress, userAddress) => () => {
  return new A.web3.eth.Contract(A.abi, contractaddress)
    .methods['timeout']()
    .send({ from: userAddress, value: 0 })
    .then(r  => fetchAndRejectInvalidReceiptFor(A)(r.transactionHash))
    .then(() => 'Termination by timeout of the program');

};



const Contract = A => userAddress => (ctors, contractaddress) => {
  const recv = (label, eventName, cb) =>
    mkRecv(A)(c, { alreadyConsumed: false })(label, eventName, cb);

  const recvWithin = (label, eventName, blocks, tcb, cb) =>
    mkRecvWithin(A)(c)(label, eventName, blocks, tcb, cb);

  const c =
    { abi:            A.abi
    , bytecode:       A.bytecode
    , sendrecv:       mkSendRecv(A)(contractaddress, userAddress, ctors)
    , timeoutterminate:  mkTimeoutTerminate(A)(contractaddress, userAddress)
    , consumedEvents: {}
    , ctors
    , contractaddress
    };

  Object.assign(c, { recv, recvWithin });

  return c;
};


// https://web3js.readthedocs.io/en/v1.2.0/web3-eth.html#sendtransaction
const mkDeploy = A => userAddress => ctors => {

  // TODO track down solid docs RE: why the ABI would have extra constructor
  // fields and when/how/why dropping leading `0x`s is necessary
  const ctorTypes = A.abi
    .find(a => a.constructor)
    .inputs
    .map(i => i.type)
    .slice(0, ctors.length);

  const encodedCtors = ctors
    .map(c => encode(A.ethers)(ctorTypes[ctors.indexOf(c)], c))
    .map(un0x);

  const data = [ A.bytecode, ...encodedCtors ].join('');

  const contractFromReceipt = r =>
    Contract(A)(userAddress)(ctors, r.contractAddress);

  return A.web3.eth.estimateGas({ data })
    .then(gas => A.web3.eth.sendTransaction({ data, gas, from: userAddress }))
    .then(r => rejectInvalidReceiptFor(r.transactionHash)(r))
    .then(contractFromReceipt);
};


const EthereumNetwork = A => userAddress =>
  ({ deploy: mkDeploy(A)(userAddress)
   , attach: (ctors, address) => Promise.resolve(Contract(A)(userAddress)(ctors, address))
   , web3:   A.web3
   , userAddress
   });


// devnet-specific /////////////////////////////////////////////////////////////

// This matches the logic in legicash-facts'
// src/legilogic_ethereum/ethereum_transaction.ml:get_first_account
// function (which is also what its prefunder script uses)
const prefundedDevnetAcct = ({ web3 }) => () =>
  web3.eth.personal.getAccounts()
    .then(a => a[0])
    .catch(e => panic(`Cannot infer prefunded account! e=${e} End of error`));


const createAndUnlockAcct = ({ web3 }) => () =>
  web3.eth.personal.newAccount('')
    .then(i => web3.eth.personal.unlockAccount(i, '', 999999999)
                 .then(u => u ? Promise.resolve(i)
                              : Promise.reject(`Couldn't unlock account ${i}!`)));


////////////////////////////////////////////////////////////////////////////////


export const mkStdlib = A =>
 ({ hexTo0x
  , un0x
  , k
  , flip
  , web3:             A.web3
  , ethers:           A.ethers
  , balanceOf:        balanceOf(A)
  , random_uint256:   random_uint256(A)
  , uint256_to_bytes: uint256_to_bytes(A)
  , bytes_cat:        bytes_cat(A)
  , bytes_len:        bytes_len(A)
  , bytes_eq:         bytes_eq(A)
  , keccak256:        keccak256(A)
  , assert:           assert(A)
  , equal:            equal(A)
  , eq:               equal(A)
  , add:              add(A)
  , sub:              sub(A)
  , mod:              mod(A)
  , mul:              mul(A)
  , ge:               ge(A)
  , gt:               gt(A)
  , le:               le(A)
  , lt:               lt(A)
  , encode:           encode(A)
  , toWei:            toWei(A)
  , toBN:             toBN(A)
  , bnToHex:          bnToHex(A)
  , isBN:             isBN(A)
  , transfer:         transfer(A)
  , now:              now(A)
  , Contract:         Contract(A)
  , EthereumNetwork:  EthereumNetwork(A)

  , devnet: { prefundedDevnetAcct: prefundedDevnetAcct(A)
            , createAndUnlockAcct: createAndUnlockAcct(A)
            }
  });
