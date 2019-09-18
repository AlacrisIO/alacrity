// vim: filetype=javascript
import abiDecoder      from 'abi-decoder';

const panic = e => { throw Error(e); };

const k = (reject, f) => (err, ...d) =>
  !!err ? reject(err)
        : f(...d);

const un0x           = h => h.replace(/^0x/, '');
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

const ABI_StateChannel = [{"constant":false,"inputs":[{"name":"session","type":"bytes32"},{"name":"clock","type":"uint256"},{"name":"participants","type":"address[]"},{"name":"balancedState","type":"bytes32"},{"name":"newClock","type":"uint256"},{"name":"newBalancedState","type":"bytes32"},{"name":"signatures_v","type":"bytes"},{"name":"signatures_r","type":"bytes32[]"},{"name":"signatures_s","type":"bytes32[]"}],"name":"updateState","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"clock","type":"uint256"},{"name":"participants","type":"address[]"}],"name":"nextClock","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":false,"inputs":[{"name":"session","type":"bytes32"},{"name":"clock","type":"uint256"},{"name":"participants","type":"address[]"},{"name":"data","type":"bytes32"},{"name":"deposit","type":"uint256"},{"name":"withdrawals","type":"uint256[]"},{"name":"newState","type":"bytes32"},{"name":"signatures_v","type":"bytes"},{"name":"signatures_r","type":"bytes32[]"},{"name":"signatures_s","type":"bytes32[]"}],"name":"settle","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"_session","type":"bytes32"},{"name":"_clock","type":"uint256"},{"name":"_participants","type":"address[]"},{"name":"_processor","type":"address"},{"name":"_stateRoot","type":"bytes32"},{"name":"_balances","type":"bytes32"},{"name":"_message","type":"bytes"},{"name":"_evidence","type":"bytes"}],"name":"sendMessage","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"session","type":"bytes32"},{"name":"clock","type":"uint256"},{"name":"_participants","type":"address[]"},{"name":"processState","type":"bytes32"},{"name":"_owned","type":"uint256[]"},{"name":"_collaterals","type":"uint256[]"},{"name":"_failures","type":"uint256[]"},{"name":"_deadlines","type":"uint256[]"},{"name":"failedParticipant","type":"uint256"}],"name":"timeOut","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":true,"inputs":[],"name":"timeoutInBlocks","outputs":[{"name":"timeout","type":"uint256"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":false,"inputs":[{"name":"session","type":"bytes32"},{"name":"clock","type":"uint256"},{"name":"participants","type":"address[]"},{"name":"data","type":"bytes32"},{"name":"withdrawals","type":"uint256[]"},{"name":"beneficiary","type":"address"},{"name":"signatures_v","type":"bytes"},{"name":"signatures_r","type":"bytes32[]"},{"name":"signatures_s","type":"bytes32[]"}],"name":"close","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"session","type":"bytes32"},{"name":"clock","type":"uint256"},{"name":"participants","type":"address[]"},{"name":"processState","type":"bytes32"},{"name":"owned","type":"uint256[]"},{"name":"collaterals","type":"uint256[]"},{"name":"failures","type":"uint256[]"},{"name":"_deadlines","type":"uint256[]"},{"name":"challengingParticipant","type":"uint256"},{"name":"challengedParticipant","type":"uint256"}],"name":"challenge","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"digest","type":"bytes32"},{"name":"participants","type":"address[]"},{"name":"signatures_v","type":"bytes"},{"name":"signatures_r","type":"bytes32[]"},{"name":"signatures_s","type":"bytes32[]"}],"name":"checkSignatures","outputs":[],"payable":false,"stateMutability":"pure","type":"function"},{"inputs":[{"name":"state","type":"bytes32"}],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"name":"","type":"bytes32"}],"name":"Unanimously","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"challengedParticipant","type":"uint256"}],"name":"Challenge","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"clock","type":"uint256"},{"indexed":false,"name":"failedParticipant","type":"uint256"}],"name":"TimeOut","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"clock","type":"uint256"},{"indexed":false,"name":"message","type":"bytes"}],"name":"Message","type":"event"}];

abiDecoder.addABI(ABI_StateChannel);

const use_state_channels = true;


// Encodes a 16-bit unsigned integer as 2 hex bytes or 4 hex characters
const nat16_to_fixed_size_hex =
  nat_to_fixed_size_hex(2);


// Parameterized ///////////////////////////////////////////////////////////////

const balanceOf = A => a =>
  A.web3.eth.getBalance(a.userAddress[0])
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


// `t` is a type name in string form; `v` is the value to cast
const encode = ({ ethers }) => (t, v) =>
  ethers.utils.defaultAbiCoder.encode([t], [v]);

const SC_mkCreateIdentity = A => () =>
  new Promise((resolve, reject) => {
    A.web3.shh.newIdentity((err_id, result_id) =>
    !!err_id ? reject(err_id) : resolve(result_id));
  });


const SC_nullparticipant = () => [0, 0]



//
// Functionality for signing
//
const SC_ProvideVRS = A => Aidentity => (iter) =>
  new Promise((resolve, reject) => {
    const myIdentity = Aidentity.userAddress[1];
    const fctSendVRS = (result_sign) => {
      A.web3.shh.post({'from':myIdentity, 'to':result_sign.from, 'payload':result_sign.XXXX},
        (err_post, result_post) => !!err_post ? reject('error shh.pos') : resolve('success shh.post' + result_post));
    };
    const fctProcess = (data_to_sign) => {
      A.web3.eth.sign(myIdentity, data_to_sign,
        (err_sign, result_sign) => !!err_sign ? reject('error signature') : fctSendVRS(result_sign));
    };
    const fctComputeHash = (data_to_sign) => {
      // Need to check that the operation is valid.
      //
      const state_to_sign = keccak256(A)(data_to_sign);
      return fctProcess(state_to_sign);
    };
    A.web3.filter({'topics':'signatureVRS', 'to':Aidentity.userAddress[1]},
    (err_filt, result_filt) => !!err_filt ? reject('error web3.filter') : fctComputeHash(result_filt));
  });

async function SC_InfiniteProvideVRS(A,Aidentity) {
  let iter = 0;
  while (iter >= 0)
  {
    let var_reply = await SC_ProvideVRS(A)(Aidentity)(iter);
    iter = iter + 1;
    console.log('SC_InfiniteProvideVRS, iter=' + iter + ' var_reply=' + var_reply);
  }
}




//
// Functionality for signing
//
const SC_ProvideListParticipant = A => Aidentity => (iter) =>
  new Promise((resolve, reject) => {
    const myIdentity = Aidentity.userAddress[1];
    const fctSendListParticipant = (result_filt) => {
      A.web3.shh.post({'from':myIdentity, 'to':result_list.from, 'payload':Aidentity.sc_participants},
        (err_post, result_post) => !!err_post ? reject('error shh.pos') : resolve('success shh.post' + result_post));
    };
    A.web3.filter({'topics':'listparticipant', 'to':Aidentity.userAddress[1]},
    (err_filt, result_filt) => !!err_filt ? reject('error web3.filter') : fctSendListParticipant(result_filt));
  });

async function SC_InfiniteProvideListParticipant(A,Aidentity) {
  let iter = 0;
  while (iter >= 0)
  {
    let var_reply = await SC_ProvideListParticipant(A)(Aidentity)(iter);
    iter = iter + 1;
    console.log('SC_InfiniteProvideVRS, iter=' + iter + ' var_reply=' + var_reply);
  }
}





const SC_ProcessUnamimous = A => (trans) =>
  new Promise((resolve, reject) => {

  });


const SC_ScanUnanimously = A => Aidentity => contractAddress =>
  new A.ethers
    .Contract(contractAddress, A.abi, new A.ethers.providers.Web3Provider(A.web3.currentProvider))
    .on('Unanimously', (...a) => {
      console.log('Code needs to be written to handle unanimous events');
      process.exit();
    });

const SC_ScanChallenge = A => Aidentity => contractAddress =>
  new A.ethers
    .Contract(contractAddress, A.abi, new A.ethers.providers.Web3Provider(A.web3.currentProvider))
    .on('Challenge', (challengedParticipant) => {
      console.log('challengedParticipant=', challengedParticipant);
      console.log('myIdentity=', Aidentity.userAddress[1]);
      console.log('Code needs to be written to handle challenge events');
      process.exit();
    });

const SC_ScanTimeOut = A => Aidentity => contractAddress =>
  new A.ethers
    .Contract(contractAddress, A.abi, new A.ethers.providers.Web3Provider(A.web3.currentProvider))
    .on('TimeOut', (clock, failedParticipant) => {
      console.log('clock=', clock, ' failedParticipant=', failedParticipant);
      console.log('myIdentity=', Aidentity.userAddress[1]);
      console.log('Code needs to be written to handle challenge events');
      process.exit();
    });

const SC_ScanMessage = A => contractAddress =>
  new A.ethers
    .Contract(contractAddress, A.abi, new A.ethers.providers.Web3Provider(A.web3.currentProvider))
    .on('Message', (clock, message) => {
      console.log('clock=', clock, ' message=', message);
      console.log('myIdentity=', Aidentity.userAddress[1]);
      console.log('Code needs to be written to handle challenge events');
      process.exit();
    });


const SC_WaitEvents = A => (contractAddress) =>
  new Promise.race([SC_ScanUnanimously(A)(contractAddress),
  SC_ScanChallenge(A)(contractAddress),
  SC_ScanTimeOut(A)(contractAddress),
  SC_ScanMessage(A)(contractAddress)]);



const SC_mkSpanThreads = A => B => () =>
  new Promise.race([SC_InfiniteProvideVRS(A)(B) , SC_WaitEvents(A)(B)]);




const SC_mkSendTransaction = A => B => (to, payload) =>
  new Promise((resolve, reject) => {
    const myIdentity = Aidentity.userAddress[1];
    const pSend = new Promise((resolve_loc, reject_loc) =>
      A.web3.shh.post({'from': myIdentity, 'to': to, 'payload': payload},
          (err, result) =>
          !!err ? reject_loc(err) : resolve_loc(result)));
    pSend.catch((message) => reject(message))
    .then((result) => // eslint-disable-line no-unused-vars
    new Promise((resolve, reject) => {
      const pTimeOut = new Promise((resolve_time, reject_time) => {
        setTimeout(reject_time, 500, 'timeout');
      });
      const pWait = new Promise((resolve_wait, reject_wait) => { // eslint-disable-line no-unused-vars
        const fct = (payload_input, result_filt) => {
          A.SC.state = A.SC.state + payload_input;
          resolve_wait(result_filt);
        };
        A.web3.shh.filter({'topics':'state-channel', 'to':myIdentity},
          (err_filt, result_filt) => !!err_filt ? reject(err_filt) : fct(payload, result_filt));
        });
      return Promise.race([pTimeOut,pWait]);
    }));
  });



const rejectInvalidReceiptFor = txHash => r =>
    !r                           ? Promise.reject(`No receipt for txHash: ${txHash}`)
  : r.transactionHash !== txHash ? Promise.reject(`Bad txHash; ${txHash} !== ${r.transactionHash}`)
  : !r.status                    ? Promise.reject(`Transaction: ${txHash} was reverted by EVM\n${r}`)
  : Promise.resolve(r);


const fetchAndRejectInvalidReceiptFor = ({ web3 }) => txHash =>
  web3.eth.getTransactionReceipt(txHash)
    .then(rejectInvalidReceiptFor(txHash));


// https://web3js.readthedocs.io/en/v1.2.0/web3-eth.html#sendtransaction
const transfer = ({ web3 }) => (to, from, value) =>
  web3.eth.sendTransaction({ to, from, value });


// https://github.com/ethereum/wiki/wiki/JavaScript-API#contract-methods
// https://web3js.readthedocs.io/en/v1.2.0/web3-eth-contract.html#web3-eth-contract
const mkSendRecvETH = A => B => (label, funcName, args, value, eventName, cb) => {
  // https://github.com/ethereum/web3.js/issues/2077
  const munged = [ ...B.ctors, ...args ]
    .map(m => isBN(A)(m) ? m.toString() : m);

  return new A.web3.eth.Contract(A.abi, B.contractAddress)
    .methods[funcName](...munged)
    .send({ from: B.userpairaddress[0], value })
    .then(r  => fetchAndRejectInvalidReceiptFor(A)(r.transactionHash))
    // XXX We may need to actually see the event. I don't know if the
    //     transaction confirmation is enough.
    // XXX Replace 0 below with the contract's balance
    .then(() => cb({ value: value, balance: 0 }));
};

const mkSendRecv = mkSendRecvETH;


const SC_mkGetListParticipant = A => (contractAddress) =>
  new Promise.resolve(

const SC_mkJoinStateChannel = A => Aidentity =>
  new Promise((resolve, reject) => {

  });



// https://docs.ethers.io/ethers.js/html/api-contract.html#configuring-events
const mkRecv = A => B => (label, eventName, cb) =>
  new ethers
    .Contract(B.contractAddress, abi, new ethers.providers.Web3Provider(web3.currentProvider))
    .once(eventName, (...a) => {
      const b = a.map(b => b); // Preserve `a` w/ copy
      const e = b.pop();       // The final element represents an `ethers` event object

      // Swap ethers' BigNumber wrapping for web3's
      const bns = b.map(x => toBN(A)(x.toString()));

      // TODO FIXME replace arbitrary delay with something more intelligent to
      // mitigate mystery race condition
      return A.web3.eth.getTransaction(e.transactionHash, k(panic, t =>
        // XXX Replace 0 below with the contract's balance
        setTimeout(() => cb(...bns, { value: t.value, balance: 0 }), 2000)));
    });


// https://web3js.readthedocs.io/en/v1.2.0/web3-eth.html#sendtransaction
// Change of code. We no longer return a full contract, just the contract address.
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

//  const contractFromReceipt = r =>
//    Contract(A)(userAddress)(ctors, r.contractAddress);

  return A.web3.eth.estimateGas({ data })
    .then(gas => A.web3.eth.sendTransaction({ data, gas, from: userAddress }))
    .then(r => rejectInvalidReceiptFor(r.transactionHash)(r))
    .then(r => r.contractAddress);
};




// This array should contain all the data
// of the process.
const MutableState = (contractAddress,ctors,userpairaddress,initiatorpairaddress) =>
   ({sc_list_address: [] // This should contain the ethereum addresses as well as shh ones.
                         // Not everyone in this list has joined the SC. It is just a lookup
                         // table
    , sc_participants: [] // The list of participants of the SC (list of index of sc_list_address)
    , sc_status_sequence: []
    , userpairaddress
    , initiatorpairaddress
    , contractAddress
    , ctors
    });




const SC_mkCreateSC = A => B => () => {
  if initiatorpairAddress[0] == 0 {



    new A.web3.eth.Contract(A.abi, contractaddress)
        .methods['constructor']()
        .send({ from: mypairAddress[0], value: 0 })
        .then(r  => fetchAndRejectInvalidReceiptFor(A)(r.transactionHash))
        .then(() => 'Successful construction of contract');
  }
  else {
    new A.
  }
};



const mkSpanCTC = A => B =>
  ({ abi:      A.abi
   , bytecode: A.bytecode
   , sendrecv: mkSendRecv(A)(B)
   , recv:     mkRecv(A)(B)
   , SC_sendTransaction: SC_mkSendTransaction(A)
   , SC_SpanThreads: SC_mkSpanThreads(A)(B)
   , SC_CreateSC: SC_mkCreateSC(A)(B)
   , B
   });






const EthereumNetwork = A => (userAddress, sc_identity) =>
  ({ deploy: mkDeploy(A)(userAddress)
   , joinSC: mkCreateSC(A)(myAddress, initiatorAddress)
   , userAddress: [userAddress, sc_identity]
   });


// devnet-specific /////////////////////////////////////////////////////////////

// This matches the logic in legicash-facts'
// src/legilogic_ethereum/ethereum_transaction.ml:get_first_account
// function (which is also what its prefunder script uses)
const prefundedDevnetAcct = ({ web3 }) => () =>
  web3.eth.personal.getAccounts()
    .then(a => a[0])
    .catch(e => panic(`Cannot infer prefunded account!\n${e}`));


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
  , web3:             A.web3
  , SC_createIdentity: SC_mkCreateIdentity(A)
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
  , Contract:         Contract(A)
  , SpanCTC:          mkSpanCTC(A)
  , EthereumNetwork:  EthereumNetwork(A)

  , devnet: { prefundedDevnetAcct: prefundedDevnetAcct(A)
            , createAndUnlockAcct: createAndUnlockAcct(A)
            }
  });
