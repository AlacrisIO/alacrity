// vim: filetype=javascript

const panic = e => { throw Error(e); };

const k = (reject, f) => (err, ...d) =>
  !!err ? reject(err)
        : f(...d);

// TODO this should be async
const balanceOf = a =>
  a.web3.eth.getBalance(a.userAddress);

const hexTo0x        = h => '0x' + h;
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

const assert = ({ asserter }) => d => asserter(d);

const toBN      = ({ web3 }) =>      web3.toBigNumber;
const digestHex = ({ web3 }) => x => web3.sha3(x, { encoding: 'hex' });

const hexToBN          = A => h => toBN(A)(hexTo0x(h));
const keccak256        = A => b => digestHex(A)(hexOf(A)(b));
const uint256_to_bytes = A => i => bnToHex(A)(i);

// https://github.com/ethereum/web3.js/blob/0.20.7/lib/utils/utils.js#L495
const isBN = ({ web3 }) => n =>
  n && (n instanceof web3.BigNumber || (n.constructor && n.constructor.name === 'BigNumber'));


const bnToHex = A => (u, size = 32) => {
  const n = toBN(A)(u);
  // TODO: if/when we switch to web3 v1.0:
  //       return n.toTwos(8 * size).toString(16, 2 * size);

  if (n.isNegative()) {
    const top   = toBN(A)(256).pow(size);
    const nTwos = n.modulo(top).add(top);
    return nTwos.toString(16).padStart(2 * size, 'f');

  } else {
    return n.toString(16).padStart(2 * size, '0');
  }
};


// Gets the hex bytes of a number or byte-string, without the 0x prefix
const hexOf = A => x =>
    typeof x === 'number'  ? bnToHex(A)(toBN(A)(x))
  : isBN(A)(x)             ? bnToHex(A)(x)
  : typeof x !== 'string'  ? panic(`Cannot convert to hex: ${x}`)
  : x.slice(0, 2) === '0x' ? x.slice(2)
  : x; // Assume `x` is already in hexadecimal form


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

  return n + ah + bh;
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


// `t` is a type name in string form; `v` is the value to cast
const encode = ({ ethers }) => (t, v) =>
  ethers.utils.defaultAbiCoder.encode([t], [v]);


const awaitConfirmationOf = ({ web3 }) => (txHash, blockPollingPeriodInSeconds = 1) =>
  new Promise((resolve, reject) => {
    // A null `t` or `t.blockNumber` means the tx hasn't been confirmed yet
    const query = () => web3.eth.getTransaction(txHash, (err, t) =>
        !!err                  ? clearInterval(i) || reject(err)
      : !!t && !!t.blockNumber ? clearInterval(i) || resolve(txHash)
      : null);

    const i = setInterval(query, blockPollingPeriodInSeconds * 1000);
  });


const txReceiptFor = ({ web3 }) => txHash =>
  new Promise((resolve, reject) =>
    web3.eth.getTransactionReceipt(txHash, (err, r) =>
        !!err                        ? reject(err)
      : r.transactionHash !== txHash ? reject(`Bad txHash; ${txHash} !== ${r.transactionHash}`)
      : r.status          !== '0x1'  ? reject(`Transaction: ${txHash} failed for unspecified reason`)
      : resolve(r)));


// https://github.com/ethereum/wiki/wiki/JavaScript-API#web3ethsendtransaction
const transfer = ({ web3 }) => (to, from, value) =>
  new Promise((resolve, reject) =>
    web3.eth.sendTransaction({ to, from, value }, e =>
      !!e ? reject(e)
          : resolve(to)));


// https://github.com/ethereum/wiki/wiki/JavaScript-API#contract-methods
const mkSend = A => (address, from, ctors) => (funcName, args, value, cb) =>
  A.web3.eth
    .contract(A.abi)
    .at(address)[funcName]
    .sendTransaction(...ctors, ...args, { from, value }, k(panic, txHash =>
      awaitConfirmationOf(A)(txHash)
        .then(() => txReceiptFor(A)(txHash))
        .then(cb)
        .catch(panic)));


// https://docs.ethers.io/ethers.js/html/api-contract.html#configuring-events
const mkRecv = ({ web3, ethers, abi }) => address => (eventName, cb) =>
  new ethers
    .Contract(address, abi, new ethers.providers.Web3Provider(web3.currentProvider))
    .on(eventName, (...a) => {
      const b = a.map(b => b); // Preserve `a` w/ copy
      const e = b.pop();       // The final element represents an `ethers` event object

      // Swap ethers' BigNumber wrapping for web3's
      const bns = b.map(x => toBN({ web3 })(x.toString()));

      // TODO FIXME replace arbitrary delay with something more intelligent to
      // mitigate mystery race condition
      web3.eth.getTransaction(e.transactionHash, k(panic, t =>
        setTimeout(() => cb(...bns, t.value), 2000)));
    });


const Contract = A => userAddress => (ctors, address) =>
  ({ abi:      A.abi
   , bytecode: A.bytecode
   , send:     mkSend(A)(address, userAddress, ctors)
   , recv:     mkRecv(A)(address)
   , ctors
   , address
   });


const mkDeploy = A => userAddress => (ctors, blockPollingPeriodInSeconds = 1) =>
  new Promise((resolve, reject) => {
    // TODO track down solid docs RE: why the ABI would have extra constructor
    // fields and when/how/why dropping leading `0x`s is necessary
    const ctorTypes = A.abi
      .find(a => a.constructor)
      .inputs
      .map(i => i.type)
      .slice(0, ctors.length);

    const encodedCtors = ctors
      .map(c => encode(A.ethers)(ctorTypes[ctors.indexOf(c)], c))
      .map(c => c.replace(/^0x/, ''));

    const data = [ A.bytecode, ...encodedCtors ].join('');

    const o = { data, from: userAddress, gas: A.web3.eth.estimateGas({ data }) };

    const gatherContractInfo = txHash =>
      txReceiptFor(A)(txHash)
        .then(r => Contract(A)(userAddress)(ctors, r.contractAddress))
        .then(resolve)
        .catch(reject);

    return A.web3.eth.sendTransaction(o, k(reject, txHash =>
      awaitConfirmationOf(A)(txHash, blockPollingPeriodInSeconds)
        .then(gatherContractInfo)));
  });


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
const prefundedDevnetAcct = ({ web3 }) =>
     web3.personal.listAccounts[0]
  || panic('Cannot infer prefunded account!');


const createAndUnlockAcct = ({ web3 }) => () =>
  new Promise(resolve =>
    web3.personal.newAccount((z, i) =>
      web3.personal.unlockAccount(i, () => resolve(i))));


////////////////////////////////////////////////////////////////////////////////


export const mkStdlib = A =>
 ({ hexTo0x
  , k
  , balanceOf
  , web3:                A.web3
  , ethers:              A.ethers
  , random_uint256:      random_uint256(A)
  , uint256_to_bytes:    uint256_to_bytes(A)
  , bytes_cat:           bytes_cat(A)
  , bytes_len:           bytes_len(A)
  , bytes_eq:            bytes_eq(A)
  , keccak256:           keccak256(A)
  , digestHex:           digestHex(A)
  , assert:              assert(A)
  , equal:               equal(A)
  , eq:                  equal(A)
  , add:                 add(A)
  , sub:                 sub(A)
  , mod:                 mod(A)
  , mul:                 mul(A)
  , ge:                  ge(A)
  , gt:                  gt(A)
  , le:                  le(A)
  , lt:                  lt(A)
  , encode:              encode(A)
  , toBN:                toBN(A)
  , bnToHex:             bnToHex(A)
  , isBN:                isBN(A)
  , awaitConfirmationOf: awaitConfirmationOf(A)
  , txReceiptFor:        txReceiptFor(A)
  , transfer:            transfer(A)
  , Contract:            Contract(A)
  , EthereumNetwork:     EthereumNetwork(A)

  , devnet: { prefundedDevnetAcct: prefundedDevnetAcct(A)
            , createAndUnlockAcct: createAndUnlockAcct(A)
            }
  });
