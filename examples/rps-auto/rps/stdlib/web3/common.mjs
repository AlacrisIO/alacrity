// vim: filetype=javascript
import abiDecoder      from 'abi-decoder';

const use_state_channel = true;

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

const ABI_StateChannel = [{'constant':false,'inputs':[{'name':'session','type':'bytes32'},{'name':'clock','type':'uint256'},{'name':'participants','type':'address[]'},{'name':'balancedState','type':'bytes32'},{'name':'newClock','type':'uint256'},{'name':'newBalancedState','type':'bytes32'},{'name':'signatures_v','type':'bytes'},{'name':'signatures_r','type':'bytes32[]'},{'name':'signatures_s','type':'bytes32[]'}],'name':'updateState','outputs':[],'payable':false,'stateMutability':'nonpayable','type':'function'},{'constant':true,'inputs':[{'name':'clock','type':'uint256'},{'name':'participants','type':'address[]'}],'name':'nextClock','outputs':[{'name':'','type':'uint256'}],'payable':false,'stateMutability':'pure','type':'function'},{'constant':false,'inputs':[{'name':'session','type':'bytes32'},{'name':'clock','type':'uint256'},{'name':'participants','type':'address[]'},{'name':'data','type':'bytes32'},{'name':'deposit','type':'uint256'},{'name':'withdrawals','type':'uint256[]'},{'name':'newState','type':'bytes32'},{'name':'signatures_v','type':'bytes'},{'name':'signatures_r','type':'bytes32[]'},{'name':'signatures_s','type':'bytes32[]'}],'name':'settle','outputs':[],'payable':true,'stateMutability':'payable','type':'function'},{'constant':false,'inputs':[{'name':'_session','type':'bytes32'},{'name':'_clock','type':'uint256'},{'name':'_participants','type':'address[]'},{'name':'_processor','type':'address'},{'name':'_stateRoot','type':'bytes32'},{'name':'_balances','type':'bytes32'},{'name':'_message','type':'bytes'},{'name':'_evidence','type':'bytes'}],'name':'sendMessage','outputs':[],'payable':true,'stateMutability':'payable','type':'function'},{'constant':false,'inputs':[{'name':'session','type':'bytes32'},{'name':'clock','type':'uint256'},{'name':'_participants','type':'address[]'},{'name':'processState','type':'bytes32'},{'name':'_owned','type':'uint256[]'},{'name':'_collaterals','type':'uint256[]'},{'name':'_failures','type':'uint256[]'},{'name':'_deadlines','type':'uint256[]'},{'name':'failedParticipant','type':'uint256'}],'name':'timeOut','outputs':[],'payable':true,'stateMutability':'payable','type':'function'},{'constant':true,'inputs':[],'name':'timeoutInBlocks','outputs':[{'name':'timeout','type':'uint256'}],'payable':false,'stateMutability':'pure','type':'function'},{'constant':false,'inputs':[{'name':'session','type':'bytes32'},{'name':'clock','type':'uint256'},{'name':'participants','type':'address[]'},{'name':'data','type':'bytes32'},{'name':'withdrawals','type':'uint256[]'},{'name':'beneficiary','type':'address'},{'name':'signatures_v','type':'bytes'},{'name':'signatures_r','type':'bytes32[]'},{'name':'signatures_s','type':'bytes32[]'}],'name':'close','outputs':[],'payable':true,'stateMutability':'payable','type':'function'},{'constant':false,'inputs':[{'name':'session','type':'bytes32'},{'name':'clock','type':'uint256'},{'name':'participants','type':'address[]'},{'name':'processState','type':'bytes32'},{'name':'owned','type':'uint256[]'},{'name':'collaterals','type':'uint256[]'},{'name':'failures','type':'uint256[]'},{'name':'_deadlines','type':'uint256[]'},{'name':'challengingParticipant','type':'uint256'},{'name':'challengedParticipant','type':'uint256'}],'name':'challenge','outputs':[],'payable':false,'stateMutability':'nonpayable','type':'function'},{'constant':true,'inputs':[{'name':'digest','type':'bytes32'},{'name':'participants','type':'address[]'},{'name':'signatures_v','type':'bytes'},{'name':'signatures_r','type':'bytes32[]'},{'name':'signatures_s','type':'bytes32[]'}],'name':'checkSignatures','outputs':[],'payable':false,'stateMutability':'pure','type':'function'},{'inputs':[{'name':'state','type':'bytes32'}],'payable':true,'stateMutability':'payable','type':'constructor'},{'anonymous':false,'inputs':[{'indexed':false,'name':'','type':'bytes32'}],'name':'Unanimously','type':'event'},{'anonymous':false,'inputs':[{'indexed':false,'name':'challengedParticipant','type':'uint256'}],'name':'Challenge','type':'event'},{'anonymous':false,'inputs':[{'indexed':false,'name':'clock','type':'uint256'},{'indexed':false,'name':'failedParticipant','type':'uint256'}],'name':'TimeOut','type':'event'},{'anonymous':false,'inputs':[{'indexed':false,'name':'clock','type':'uint256'},{'indexed':false,'name':'message','type':'bytes'}],'name':'Message','type':'event'}];

abiDecoder.addABI(ABI_StateChannel);

//const use_state_channels = true;


// Encodes a 16-bit unsigned integer as 2 hex bytes or 4 hex characters
const nat16_to_fixed_size_hex =
  nat_to_fixed_size_hex(2);


// Parameterized ///////////////////////////////////////////////////////////////

const balanceOf = A => a =>
  A.web3.eth.getBalance(a[0])
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
const encode = ({ ethers }) => (t, v) => {
  console.log('t=', t, ' v=', v);
  return ethers.utils.defaultAbiCoder.encode([t], [v]);
};

const SC_mkCreateIdentity = A => () =>
      new Promise(resolve => {
          var identities = [];
          A.web3.shh.newSymKey().then((id) => {identities.push(id);}),
          A.web3.shh.newKeyPair().then((id) => {identities.push(id);});
          resolve(identities);});

// The cases to cover for digest computations are
// 1: close
//   digest = keccak256(encode(A)(session, UnanimousAction.Closing, withdrawals, beneficiary))
// 2: settle
//   digest = keccak256(encode(A)(session, UnanimousAction.Settling, deposit, withdrawals, newState)
// 3: updateState
//   digest = keccak256(encode(A)(session, UnanimousAction.Updating, newClock, newBalancedState))
// The signature
//
const get_digest = A => (oper) => {
    if (oper.nature == 0)
        return keccak256(A)(encode(A)(oper.session, 'closing', oper.withdrawals, oper.beneficiary));
    if (oper.nature == 1)
        return keccak256(A)(encode(A)(oper.session, 'settling', oper.deposit, oper.withdrawals, oper.newState));
    if (oper.nature == 2)
        return keccak256(A)(encode(A)(oper.session, 'updating', oper.clock, oper.data));
};

let CheckCorrectnessOperation = A => B => (oper) => {
    // We need to insert code for checking correctness of operation
    console.log('Code need to be inserted here');
    console.log('CheckCorrectnessOperation A=', A);
    console.log('CheckCorrectnessOperation B=', B);
    console.log('CheckCorrectnessOperation oper=', oper);
    return true;
};


let SC_SignState = A => B => (oper) => {
    if (CheckCorrectnessOperation(A)(B)(oper)) {
        const digest = get_digest(A)(oper);
        B.pending_unanimous_operations[B.pending_unanimous_operations.length] = {digest, oper};
        const myId_eth = B.userpairaddress[0];
        return A.web3.eth.sign(myId_eth, digest);
    }
};

//
// SHH: Functionality for signing messages from the computing side.
//
const SC_Send_VRSsignature = A => B =>
      new Promise((resolve, reject) => {
          console.log('SC_Send_VRSsignature, step 1');
          const myId_shh = B.userpairaddress[1];
          const fctSendVRS = (result_sign) => {
              A.web3.shh.post({
                  symKeyID: myId_shh[0],
                  sig: myId_shh[1],
                  ttl: 10,
                  topic: 'listparticipant2',
                  payload: result_sign,
                  powTime: 3,
                  powTarget: 0.5})
                  .then(h => {
                      console.log('Message with hash was successfully sent h=', h);
                      resolve(h);
                  })
                  .catch(err => {
                      console.log('Error: ', err);
                      reject(err);
                  });
          };
          console.log('SC_Send_VRSsignature, step 2');
          const CompHash_and_send = (data_to_sign) => {
              const state_to_sign = keccak256(A)(data_to_sign);
              SC_SignState(A)(B)(state_to_sign).then(fctSendVRS);
          };
          console.log('SC_Send_VRSsignature, step 3');
//          var subscription = null;
//          console.log('myId_shh=', myId_shh);
          var subscription = A.web3.shh.subscribe('messages', {
              symKeyID: myId_shh[0],
              topics: ['VRSsignature1']});
          console.log('SC_Send_VRSsignature, step 4');
          subscription.on('error', e => {
              console.log('Error e=', e);
              reject(e);
          });
          console.log('SC_Send_VRSsignature, step 5');
          subscription.on('data', CompHash_and_send);
          console.log('SC_Send_VRSsignature, step 6');
//          resolve('leaving');
      });


async function SC_Inf_Send_VRSsignature(A,B) {
  let iter = 0;
  while (iter >= 0)
  {
    let var_reply = await SC_Send_VRSsignature(A)(B);
    iter = iter + 1;
    console.log('SC_Inf_Send_VRSsignature, iter=' + iter + ' var_reply=' + var_reply);
  }
}






const SC_GetSingle_VRSsignature = A => B => (requestpair, state) =>
      new Promise((resolve, reject) => {
          console.log('Beginning of SC_GetSingle_VRSsignature');
          const myId_shh = B.userpairaddress[1];
          const request_shh = requestpair[1];
          const waitForSignature = () => {
              A.web3.shh.filter(
                  {'topics':['VRSsignature2'], 'to':myId_shh},
                  (err_filt, result_filt) => !!err_filt ? reject('error web3.filter') : resolve(result_filt));
          };
          A.web3.shh.post(
              {'from':myId_shh, 'to':request_shh, 'topics':['VRSsignature1'], 'payload':state},
              (err_post, _result_post) => !!err_post ? reject('error shh.pos') : waitForSignature());
      });



const digestState = A => full_state => {
//    console.log('A=', A);
//    console.log('full_state=', full_state);
    return keccak256(A)(full_state.session + A.web3.utils.toHex(full_state.clock) + A.web3.utils.toHex(full_state.participant) + full_state.data);
};


const SC_SubmitSettleOperation = A => B => (prev_state, new_state, deposit, withdrawals, list_signature) => {
    const session = prev_state.session;
    const clock = prev_state.clock;
    const participants = prev_state.participants.map(x => x[0]);
    const data = prev_state.data;
    //
    const signatures_v = list_signature.map(x => x.v);
    const signatures_r = list_signature.map(x => x.r);
    const signatures_s = list_signature.map(x => x.s);
    //
    const newState = digestState(A)(new_state);
    return new A.web3.eth.Contract(A.abi, B.contractAddress)
        .methods['settle'](session, clock, participants, data, deposit, withdrawals,
                           newState, signatures_v, signatures_r, signatures_s)
        .send({ from: B.userpairaddress[0], value: deposit })
        .then(r => fetchAndRejectInvalidReceiptFor(A)(r.transactionHash));
};







async function SC_GetAll_VRSsignatures(A,B, state) {
    console.log('SC_GetAll_VRSsignatures, begin');
    const nb_part = B.sc_idx_participants.length;
    console.log('SC_GetAll_VRSsignatures, nb_part=', nb_part);
    const ListSignatures = [];
    for (var i=0; i<nb_part; i++) {
        if (i == B.sc_my_idx) {
            // my signature is needed
            let the_sign = await SC_SignState(A)(B)(state);
            ListSignatures[i] = the_sign;
        }
        else {
            // others participants
            let requestpair = B.sc_list_address[B.sc_idx_participants[i]];
            let the_sign = await SC_GetSingle_VRSsignature(A)(B)(requestpair, state);
            ListSignatures[i] = the_sign;
        }
    }
    return ListSignatures;
}






//
// SHH: Functionality for obtaining list of participants
//
const SC_Send_ListParticipant = A => B =>
      new Promise((resolve, reject) => {
      console.log('SC_Send_ListParticipant, step 1');
      const myId_shh = B.userpairaddress[1];
      console.log('SC_Send_ListParticipant, step 2');

      const fctSendListParticipant = (result_filt) => {
          console.log('SC_Send_ListParticipant, step 3');
          const list_part = B.sc_idx_participants.map(x => B.sc_list_address[x]);
          console.log('SC_Send_ListParticipant, step 4');
          A.web3.shh.post({
            symKeyID: myId_shh[0],
            sig: myId_shh[1],
            ttl: 10,
            topic: 'listparticipant2',
            payload: list_part,
            powTime: 3,
            powTarget: 0.5})
              .then(h => {
                  console.log('Message with hash was successfully sent h=', h);
                  resolve(h);
              })
              .catch(err => {
                  console.log('Error: ', err);
                  reject(err);
              });
      };

      console.log('SC_Send_ListParticipant, step 5');
      var subscription = null;
      console.log('SC_Send_ListParticipant, step 6');
      subscription = A.web3.shh.subscribe('messages', {
          symKeyID: myId_shh[0],
          topics: ['listparticipant1']})
          .on('data', fctSendListParticipant);
      console.log('SC_Send_ListParticipant, step 7');
//      console.log('subscription=', subscription);
//      resolve(iter);
  });


const SC_Test = (iter) => new Promise(resolve => {
    console.log('SC_Test, iter=', iter);
    setTimeout(function() {
        resolve('foo');
    }, 300);
});


async function SC_Inf_Send_ListParticipant(A,B) {
  let iter = 0;
  while (iter >= 0)
  {
      let var_reply = await SC_Send_ListParticipant(A)(B);
//      let var_reply = await SC_Test(iter);
      iter = iter + 1;
      console.log('SC_Inf_Send_ListParticipant, iter=' + iter + ' var_reply=' + var_reply);
  }
}







const SC_Get_ListParticipant = A => B =>
  new Promise((resolve, reject) => {
      console.log('SC_Get_ListParticipant, step 1');
      const myId_shh = B.userpairaddress[1];
//      const initiatorId = B.initiatorpairaddress[1];
      console.log('SC_Get_ListParticipant, step 2');
      const updateListPart = (listpart) => {
          console.log('listpart');
          B.sc_list_address = B.sc_list_address.concat(listpart);
          resolve('successful update of list_address');
      };
      console.log('SC_Get_ListParticipant, step 3');
      const fctRecvListParticipant = () => {
          const subscription = A.web3.shh.subscribe('messages', {
              symKeyID: myId_shh[0],
              topics: ['listparticipant2']})
                .on('data', updateListPart);
      };
      console.log('SC_Get_ListParticipant, step 4');
      A.web3.shh.post({
          symKeyID: myId_shh[0],
          sig: myId_shh[1],
          ttl: 10,
          topic: 'listparticipant1',
          payload: '',
          powTime: 3,
          powTarget: 0.5})
          .then(h => {
              console.log('Message with hash was successfully sent h=', h);
              resolve(h);
              })
          .catch(err => {
              console.log('Error: ', err);
              reject(err);
          });
  });






const SC_UpdateCurrentState = A => B => (oper) => {
    console.log('Code needs to be written to update state from the operation');
//    console.log('SC_UpdateCurrentState A=', A);
//    console.log('SC_UpdateCurrentState B=', B);
//    console.log('SC_UpdateCurrentState oper=', oper);
//    process.exit();
};


const SC_WaitEvents = A => B =>
      new Promise(resolve => {
          console.log('Beginning of SC_WaitEvents');
          const subs = new A.ethers.Contract(B.contractAddress,
                 A.abi, new A.ethers.providers.Web3Provider(A.web3.currentProvider));
          subs.on('Unanimously', (digest) => {
              console.log('SC_ScanUnanimously, on operation digest=', digest);
              const oper = B.pending_unanimous_operations.find(x => x.digest === digest);
              console.log('SC_ScanUnanimously, oper=', oper);
              SC_UpdateCurrentState(A)(B)(oper);
              resolve('successful completion of  Unanimously');
          });
          subs.on('Challenge', (challengedParticipant) => {
              console.log('challengedParticipant=', challengedParticipant);
              resolve('Successful completion of ScanChallenge');
          });
          subs.on('TimeOut', (clock, failedParticipant) => {
              console.log('clock=', clock, ' failedParticipant=', failedParticipant);
              console.log('myIdentity=', B.userpairaddress[1]);
              console.log('Code needs to be written to handle challenge events');
              resolve('Successful completion of TimeOut');
          });
          subs.on('Message', (clock, message) => {
              console.log('clock=', clock, ' message=', message);
              console.log('myIdentity=', B.userpairaddress[1]);
              console.log('Code needs to be written to handle challenge events');
              process.exit();
          });
      });


// We can refactor these out (before merging the PR) once new features have
// been stabilized (definitely keep using them in the meantime if it helps you
// make progress) but `async` + `await` are almost always the wrong choice
// IMHO.
//
// They're also wholly unnecessary once you're familiar enough with plugging
// `Promise` pipelines together. Don't worry about TCO, for instance; I think
// what you alluded to before didn't actually need a TCO solution anyway and
// it'll become clear why given a little more practice.
//
// `async` + `await`'s inclusion into the language has way more to do with user
// adoption/appeasement than facilitating good code composition or removing
// technical barriers, and they're kind of an anti-pattern, especially when
// favoring a functional style like we do.
//
// Projects that use them tend to look more like C# or Java over time, and
// consequently they grow increasingly more difficult to extend or maintain +
// suffer from all the usual issues inherent to OOP/imperative-styled programs.

async function SC_Inf_WaitEvents(A,B) {
    let iter = 0;
    while (iter >= 0)
    {
        let var_reply = await SC_WaitEvents(A)(B);
        iter = iter + 1;
        console.log('SC_Inf_WaitEvents, iter=' + iter + ' var_reply=' + var_reply);
    }
}





// I suspect you might be using `Promise.race` in some places where what you
// really want instead is `Promise.all`
const SC_mkSpanThreads = A => B => () =>
      Promise.race([SC_Inf_Send_ListParticipant(A,B),
                    SC_Inf_Send_VRSsignature(A,B),
                    SC_Inf_WaitEvents(A,B)]);



const SC_mkSendTransaction = A => B => (to, payload) =>
  new Promise((resolve, reject) => {
    const myId_shh = B.userpairaddress[1];
    const pSend = new Promise((resolve_loc, reject_loc) =>
      A.web3.shh.post({'from': myId_shh, 'to': to, 'payload': payload},
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
        A.web3.shh.filter({'topics':['state-channel'], 'to':myId_shh},
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


// https://docs.ethers.io/ethers.js/html/api-contract.html#configuring-events
const mkRecv = A => B => (label, eventName, cb) =>
  new A.ethers
    .Contract(B.contractAddress, A.abi, new A.ethers.providers.Web3Provider(A.web3.currentProvider))
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
const mkDeploy = A => userAddress => (full_state, ctors) => {
  // TODO track down solid docs RE: why the ABI would have extra constructor
  // fields and when/how/why dropping leading `0x`s is necessary
  console.log('mkDeploy : full_state=', full_state);
//    console.log('A.abi=', A.abi);
  var data;
  if (use_state_channel) {
    const newState = digestState(A)(full_state);
    const ctor_state = un0x(encode(A.ethers)('bytes32', newState));
//    console.log('ctor_state=', ctor_state);
    data = [ A.bytecode, ctor_state ].join('');
  }
  else {
    const ctorTypes = A.abi
    .find(a => a.constructor)
    .inputs
    .map(i => i.type)
    .slice(0, ctors.length);

    const ctor1 = A.abi.find(a =>a.constructor);
    const ctor2 = A.abi.find(a =>a.constructor).inputs;
    const ctor3 = A.abi.find(a =>a.constructor).inputs.map(i => i.type);

    console.log('ctor1=', ctor1);
    console.log('ctor2=', ctor2);
    console.log('ctor3=', ctor3);
    console.log('ctorsTypes=', ctorTypes);
    console.log('ctors=', ctors);
    const encodedCtors = ctors
    .map(c => encode(A.ethers)(ctorTypes[ctors.indexOf(c)], c))
    .map(un0x);
    data = [ A.bytecode, ...encodedCtors ].join('');
  }

//  const contractFromReceipt = r =>
//    Contract(A)(userAddress)(ctors, r.contractAddress);
//  console.log('data=', data);
  return A.web3.eth.estimateGas({ data })
    .then(gas => A.web3.eth.sendTransaction({ data, gas, from: userAddress }))
    .then(r => rejectInvalidReceiptFor(r.transactionHash)(r))
    .then(r => r.contractAddress);
};




// This array should contain all the data
// of the process.
const MutableState = (contractAddress,ctors,userpairaddress,initiatorpairaddress,deposit,full_state) =>
   ({sc_list_address: [] // This should contain the ethereum addresses as well as shh ones.
                         // Not everyone in this list has joined the SC. It is just a lookup
                         // table
     , sc_idx_participants: [] // The list of participants of the SC (list of index of sc_list_address)
     , sc_my_idx: 0
     , sc_status_sequence: [full_state]
     , pending_unanimous_operations: []
     , deposit
     , userpairaddress
     , initiatorpairaddress
     , contractAddress
     , ctors
    });



// See note in `demo.mjs` RE: Problem #2 - the `full_state => { .. }` function
// produced below never returns a `Promise` (or anything else), so execution
// fails due to the absence of a `.then()` method to execute on the
// (non-)result of its invocation.
const SC_mkCreateSC = A => B => (full_state) => {
    console.log('SC_mkCreateSC, step 1');
    const state = digestState(A)(full_state);
    console.log('SC_mkCreateSC, step 2, state=', state);
    const ctor_state = un0x(encode(A.ethers)('bytes32', state));
    console.log('SC_mkCreateSC, step 3, ctor_state=', ctor_state);
    if (B.initiatorpairaddress[0] == 0) {
        console.log('SC_mkCreateSC, step 4');

      // Problem #1 based on our Slack discussion:
      //
      // The issue here is confusion between `sendRecv` vs. `deploy`. If you're
      // trying to replicate how we instantiate contracts on the chain you'll
      // need to model your code after the latter. See:
      // https://github.com/AlacrisIO/alacrity/blob/37b4db2d0f27f8f563551b6eebeeca6949a024f4/examples/rps-auto/rps/stdlib/web3/common.mjs#L302
      //
      // However... It's not clear to me why you'd try to call a contract's
      // constructor more than once for what (presumably?) would be a shared
      // instance of a contract, and keep in mind that a contract that hasn't
      // yet been deployed will - by definition - lack any available methods :-)
        new A.web3.eth.Contract(A.abi, B.contractaddress)
            .methods['constructor'](state)
            .send({ from: B.userpairaddress[0], value: 0 })
            .then(r  => fetchAndRejectInvalidReceiptFor(A)(r.transactionHash))
            .then(() => {
                console.log('Successful construction of contract');
                Promise.resolve();
            });
    }
    else {
        console.log('SC_mkCreateSC, step 5');
        SC_Get_ListParticipant(A)(B)
            .then(mesg => SC_GetAll_VRSsignatures(A,B, state)
                  .then(list_signature => {
                      console.log('SC_mkCreateSC, step 6');
                      const prev_state = B.list_full_state[0];
                      const participants = prev_state.participants;
                      participants.push(B.userpairaddress[0]);
                      const new_state = {session: prev_state.session, data: prev_state.data,
                                         participants, clock: prev_state.clock };
                      const deposit = B.deposit;
                      const withdrawals = [];
                      for (var i=0; i<mesg.length; i++) {
                          withdrawals.push(0);
                      }
                      console.log('SC_mkCreateSC, step 7');
                      SC_SubmitSettleOperation(A)(B)(prev_state, new_state, deposit, withdrawals, list_signature);
                  }));
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
   , SC_Inf_Send_ListParticipant
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
  , MutableState:     MutableState
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
  , SpanCTC:          mkSpanCTC(A)
  , deploy:           mkDeploy(A)
  , devnet: { prefundedDevnetAcct: prefundedDevnetAcct(A)
            , createAndUnlockAcct: createAndUnlockAcct(A)
            }
  });
