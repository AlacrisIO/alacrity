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



const ascii_to_hexa = str =>
{
    var arr1 = ['0x'];
    for (var n = 0, l = str.length; n < l; n ++)
    {
	var hex = Number(str.charCodeAt(n)).toString(16);
	arr1.push(hex);
    }
    return arr1.join('');
};

const hexa_to_ascii = str1 =>
{
    var hex  = un0x(str1.toString());
    var str = '';
    for (var n = 0; n < hex.length; n += 2) {
	str += String.fromCharCode(parseInt(hex.substr(n, 2), 16));
    }
    return str;
};

const array_to_hexa = arr =>
{
    return ascii_to_hexa(JSON.stringify(arr));
};

const hexa_to_array = recvmesg =>
{
    return JSON.parse(hexa_to_ascii(recvmesg.payload));
};





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
    console.log('oper=', oper);
    console.log('oper.nature=', oper.nature);
    if (oper.nature == 0)
        return keccak256(A)(encode(A)(oper.session, 'closing', oper.withdrawals, oper.beneficiary));
    if (oper.nature == 1)
        return keccak256(A)(encode(A)(oper.session, 'settling', oper.deposit, oper.withdrawals, oper.newState));
    if (oper.nature == 2)
        return keccak256(A)(encode(A)(oper.session, 'updating', oper.clock, oper.data));
};

const digestState = A => full_state => {
//    console.log('A=', A);
//    console.log('full_state=', full_state);
    return keccak256(A)(full_state.session + A.web3.utils.toHex(full_state.clock) + A.web3.utils.toHex(full_state.participant) + full_state.data);
};



let CheckCorrectnessOperation = A => B => (oper) => {
    // We need to insert code for checking correctness of operation
    console.log('The code of CheckCorrectnessOperation needs to be inserted');
//    console.log('CheckCorrectnessOperation A=', A);
//    console.log('CheckCorrectnessOperation B=', B);
//    console.log('CheckCorrectnessOperation oper=', oper);
    return true;
};


let SC_SignState = A => B => (state_to_sign) => {
    console.log('SC_SignState: operation is correct state_to_sign=', state_to_sign);
    const digest = get_digest(A)(state_to_sign);
    console.log('SC_SignState: digest=', digest);
    B.pending_unanimous_operations[B.pending_unanimous_operations.length] = {digest, state_to_sign};
    const myId_eth = B.userpairaddress[0];
    console.log('SC_SignState: myId_eth=', myId_eth);
    return A.web3.eth.sign(digest, myId_eth);
};


const topic_vrs_step1 = '0xeeaadd11';
const topic_vrs_step2 = '0xeeaadd22';
const topic_listparticipant_step1 = '0xffaadd11';
const topic_listparticipant_step2 = '0xffaadd22';




const SC_post = A => (Id_shh, topic, payload) =>
      A.web3_shh.shh.post({
          symKeyID: Id_shh[0],
          sig: Id_shh[1],
          ttl: 10,
          topic,
          payload: array_to_hexa(payload),
          powTime: 3,
          powTarget: 0.5});

const SC_receive = A => (Id_shh, topic) =>
      A.web3_shh.shh.subscribe('messages', {
        symKeyID: Id_shh[0],
        topics: [topic]
      });


//
// SHH: Functionality for signing messages from the computing side.
//
const SC_Send_VRSsignature = A => B =>
      new Promise((resolve, reject) => {
          console.log('SC_Send_VRSsignature, step 1');
          const myId_shh = B.userpairaddress[1];
          const fctSendVRS = (result_sign) => {
              console.log('fctSendVRS: Before call to SC_post');
              SC_post(A)(myId_shh, topic_vrs_step2, result_sign)
                  .then(h => {
                      console.log('Successfully send required VRS signature h=', h);
                      resolve(h);
                  })
                  .catch(err => {
                      console.log('Error: ', err);
                      reject(err);
                  });
          };
          console.log('SC_Send_VRSsignature, step 2');
          const CompHash_and_send = (data_to_sign) => {
              if (CheckCorrectnessOperation(A)(B)(data_to_sign)) {
                  console.log('data_to_sign=', data_to_sign);
                  const state_to_sign = keccak256(A)(data_to_sign);
                  console.log('state_to_sign=', state_to_sign);
                  SC_SignState(A)(B)(state_to_sign).then(fctSendVRS);
              }
              else {
                  resolve('The state did not pass sanity check');
              }
          };
          console.log('SC_Send_VRSsignature, step 3');
          SC_receive(A)(myId_shh, topic_vrs_step1)
              .once('data', x => {
                  console.log('Receiving a request for VRS signature');
                  const mesg = hexa_to_array(x);
                  if (mesg.to === B.userpairaddress[0]) {
                      console.log('Send_VRS: Message was for me, computing hash and sending it');
                      console.log('mesg.state=', mesg.state);
                      CompHash_and_send(mesg.state);
                  }
                  else {
                      resolve('Send_VRS: message was not for me');
                  }
              });
      });


async function SC_Inf_Send_VRSsignature(A,B) {
    let iter = 0;
    while (B.run_threads)
    {
        let var_reply = await SC_Send_VRSsignature(A)(B);
        iter = iter + 1;
        console.log('SC_Inf_Send_VRSsignature, iter=' + iter + ' var_reply=' + var_reply);
    }
}






const SC_GetSingle_VRSsignature = A => B => (requestpair, state) =>
      new Promise((resolve, reject) => {
          console.log('Beginning of SC_GetSingle_VRSsignature, state=', state);
          const init_shh = B.initiatorpairaddress[1];
          const waitForSignature = () => {
              SC_receive(A)(init_shh, topic_vrs_step2)
                  .once('data', x => {
                      console.log('Receive message');
                      resolve(hexa_to_array(x));
                  });
          };
          SC_post(A)(init_shh, topic_vrs_step1, {state, to: requestpair[0]})
              .then(h => {
                  console.log('Successful send of request for VRS signature h=', h);
                  waitForSignature();
              })
              .catch(err => {
                  console.log('Error: ', err);
                  reject(err);
              });
      });





const SC_SubmitSettleOperation = A => B => (prev_state, new_state, deposit, withdrawals, list_signature) => {
    console.log('SC_SubmitSettleOperation, step 1');
    const session = typeConversionToWeb3(A)(prev_state.session);
    const clock = typeConversionToWeb3(A)(prev_state.clock);
    const participants = prev_state.participants.map(x => typeConversionToWeb3(A)(x[0]));
    const data = typeConversionToWeb3(A)(prev_state.data);
    const deposit_web3 = typeConversionToWeb3(A)(deposit);
    const withdrawals_web3 = withdrawals.map(typeConversionToWeb3(A));
    console.log('withdrawals=', withdrawals);
    console.log('SC_SubmitSettleOperation, step 2');
    //
    const signatures_v = list_signature.map(x => x.v);
    const signatures_r = list_signature.map(x => x.r);
    const signatures_s = list_signature.map(x => x.s);
    console.log('list_signature=', list_signature);
    console.log('SC_SubmitSettleOperation, step 3');
    //
    const newState = digestState(A)(new_state);
    console.log('SC_SubmitSettleOperation, step 4');
    console.log('session=', session);
    console.log('clock=', clock);
    console.log('participants=', participants);
    console.log('data=', data);
    console.log('signatures_v=', signatures_v);
    console.log('signatures_r=', signatures_r);
    console.log('signatures_s=', signatures_s);
    console.log('newState=', newState);
//        .methods['settle'](session, clock, participants, data, deposit, withdrawals,
//                           newState, signatures_v, signatures_r, signatures_s)
    return new A.web3.eth.Contract(A.abi, B.contractAddress)
        .methods['settle_test1'](session, clock, participants, data, deposit_web3, withdrawals_web3,
                                 newState)
        .send({ from: B.userpairaddress[0], value: deposit })
        .then(r => fetchAndRejectInvalidReceiptFor(A)(r.transactionHash));
};







async function SC_GetAll_VRSsignatures(A,B, state) {
    console.log('SC_GetAll_VRSsignatures, begin state=', state);
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
          console.log('SC_Send_ListParticipant, step 1, B.userpairaddress[0]=', B.userpairaddress[0]);
          const myId_shh = B.userpairaddress[1];
          console.log('SC_Send_ListParticipant, step 2');
          const fctSendListParticipant = () => {
              console.log('SC_Send_ListParticipant, step 3');
              const list_part = B.sc_idx_participants.map(x => B.sc_list_address[x]);
              console.log('SC_Send_ListParticipant, step 4, list_part=', list_part);
              SC_post(A)(myId_shh, topic_listparticipant_step2, list_part)
                  .then(h => {
                      console.log('Successful send of listparticipant h=', h);
                      resolve(h);
                  })
                  .catch(err => {
                      console.log('Error: ', err);
                      reject(err);
                  });
          };
          console.log('SC_Send_ListParticipant, step 5');
          SC_receive(A)(myId_shh, topic_listparticipant_step1)
              .once('data', x => {
                  console.log('data: Receiving of topic B.userpairaddress[0]=', B.userpairaddress[0]);
                  const mesg = hexa_to_array(x);
                  console.log('data: mesg.to=', mesg.to);
                  if (mesg.to === B.userpairaddress[0]) {
                      console.log('Send_ListPart: Message was for me, sending list participant');
                      fctSendListParticipant();
                  }
                  else {
                      resolve('Send_ListPart: Message was not for me');
                  }
              });
  });



async function SC_Inf_Send_ListParticipant(A,B) {
    let iter = 0;
    while (B.run_threads)
    {
        let var_reply = await SC_Send_ListParticipant(A)(B);
        iter = iter + 1;
        console.log('SC_Inf_Send_ListParticipant, iter=' + iter + ' var_reply=' + var_reply);
    }
}





async function SC_Get_ListParticipant(A,B)
{
    console.log('SC_Get_ListParticipant, step 1');
    const init_shh = B.initiatorpairaddress[1];
    //      const initiatorId = B.initiatorpairaddress[1];
    console.log('SC_Get_ListParticipant, step 2');
    var received_listpart = false;
    const updateListPart = (listpart) => {
        console.log('listpart=', listpart);
        const oldlen = B.sc_list_address.length;
        B.sc_list_address = B.sc_list_address.concat(listpart);
        const lenappend=listpart.length;
        for (var u=0; u<lenappend; u++)
        {
            B.sc_idx_participants.push(oldlen + u);
        }
        console.log('updateListPart : B.sc_list_address=', B.sc_list_address);
        console.log('updateListPart : B.sc_idx_participants=', B.sc_idx_participants);
        received_listpart = true;
    };
    console.log('SC_Get_ListParticipant, step 3');
    SC_receive(A)(init_shh, topic_listparticipant_step2)
        .once('data', x => updateListPart(hexa_to_array(x)));
    console.log('SC_Get_ListParticipant, step 4');
    console.log('SC_Get_ListParticipant, init_shh[0]=', init_shh[0]);
    console.log('SC_Get_ListParticipant, init_shh[1]=', init_shh[1]);
    console.log('SC_Get_ListParticipant, B.initiatorpairaddress[0]=', B.initiatorpairaddress[0]);
    var iter=0;
    while (received_listpart === false)
    {
        console.log('Before the SC_Wait event iter=', iter);
        await SC_Wait(1500);
        SC_post(A)(init_shh, topic_listparticipant_step1, {to: B.initiatorpairaddress[0]})
            .then(h => {
                console.log('Successful send of request for listparticipant h=', h);
            });
          iter = iter + 1;
    }
    console.log('successful update of listparticipant');
}






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
              console.log('Unanimously, on digest=', digest);
              const oper = B.pending_unanimous_operations.find(x => x.digest === digest);
              console.log('Unanimously, oper=', oper);
              SC_UpdateCurrentState(A)(B)(oper);
              resolve('successful processing of event Unanimously');
          });
          subs.on('Challenge', (challengedParticipant) => {
              console.log('challengedParticipant=', challengedParticipant);
              resolve('Successful processing of event Challenge');
          });
          subs.on('TimeOut', (clock, failedParticipant) => {
              console.log('clock=', clock, ' failedParticipant=', failedParticipant);
              console.log('myIdentity=', B.userpairaddress[1]);
              console.log('Code needs to be written to handle challenge events');
              resolve('Successful processing of event TimeOut');
          });
          subs.on('Message', (clock, message) => {
              console.log('clock=', clock, ' message=', message);
              console.log('myIdentity=', B.userpairaddress[1]);
              console.log('Code needs to be written to handle challenge events');
              resolve('Successful processing of event Message');
          });
      });


const SC_Wait = (delay) => new Promise(resolve => {
    console.log('SC_Wait, delay=', delay);
    setTimeout(function() {
        resolve();
    }, delay);
});




async function SC_Inf_WaitEvents(A,B) {
    let iter = 0;
    while (B.run_threads)
    {
        console.log('Before SC_WaitEvents');
        let var_reply = await SC_WaitEvents(A)(B);
//        let var_reply = await SC_Test(iter);
        iter = iter + 1;
        console.log('SC_Inf_WaitEvents, iter=' + iter + ' var_reply=' + var_reply);
    }
    console.log('After exiting while loop. Need to know which promises are still pending');
    debugger;
}





const SC_mkSpanThreads = A => B => () =>
      Promise.race([SC_Inf_Send_ListParticipant(A,B),
                    SC_Inf_Send_VRSsignature(A,B)]);
//                    SC_Inf_WaitEvents(A,B)]);





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

const typeConversionToWeb3 = A => m =>
      isBN(A)(m) ? m.toString() : m;



// https://github.com/ethereum/wiki/wiki/JavaScript-API#contract-methods
// https://web3js.readthedocs.io/en/v1.2.0/web3-eth-contract.html#web3-eth-contract
const mkSendRecvETH = A => B => (label, funcName, args, value, eventName, cb) => {
  // https://github.com/ethereum/web3.js/issues/2077
  const munged = [ ...B.ctors, ...args ]
    .map(m => isBN(A)(m) ? m.toString() : m);
  //
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

  return A.web3.eth.estimateGas({ data })
    .then(gas => A.web3.eth.sendTransaction({ data, gas, from: userAddress }))
    .then(r => rejectInvalidReceiptFor(r.transactionHash)(r))
    .then(r => r.contractAddress);
};




// This array should contain all the data
// of the process.
const MutableState = (contractAddress,ctors,userpairaddress,initiatorpairaddress,deposit,full_state) => {
    var sc_list_address;
    var sc_idx_participants;
    var sc_my_idx;
    if (initiatorpairaddress[0] === 0) {
        sc_list_address = [userpairaddress];
        sc_idx_participants = [0];
        sc_my_idx = 0;
    }
    else {
        sc_list_address = [];
        sc_idx_participants = [];
        sc_my_idx = -1;
    }
    return {sc_list_address // This should contain the ethereum addresses as well as shh ones.
            // Not everyone in this list has joined the SC. It is just a lookup
            // table
            , sc_idx_participants // The list of participants of the SC (list of index of sc_list_address)
            , sc_my_idx
            , list_full_state: [full_state]
            , pending_unanimous_operations: []
            , run_threads: true
            , deposit
            , userpairaddress
            , initiatorpairaddress
            , contractAddress
            , ctors
           };
};



const SC_mkCreateSC = A => B => (full_state) => {
    console.log('SC_mkCreateSC, step 1');
    const state = digestState(A)(full_state);
    console.log('SC_mkCreateSC, step 2, state=', state);
    const ctor_state = un0x(encode(A.ethers)('bytes32', state));
    console.log('SC_mkCreateSC, step 3, ctor_state=', ctor_state);
    // Nothing to be done if you are the initiator.
    if (B.initiatorpairaddress[0] !== 0) {
        console.log('SC_mkCreateSC, step 5');
        SC_Get_ListParticipant(A,B)
            .then(mesg => SC_GetAll_VRSsignatures(A,B, state)
                  .then(list_signature => {
                      console.log('SC_mkCreateSC, step 6');
                      console.log('mesg=', mesg);
                      const prev_state = B.list_full_state[0];
//                      console.log('1: prev_state=', prev_state);
                      const participants = prev_state.participants.map(x => x);
                      participants.push(B.userpairaddress);
                      const new_state = {session: prev_state.session, data: prev_state.data,
                                         participants, clock: prev_state.clock };
//                      console.log('participants=', participants);
                      const deposit = B.deposit;
                      const withdrawals = [];
                      for (var i=0; i<prev_state.participants.length; i++) {
                          withdrawals.push(0);
                      }
                      console.log('SC_mkCreateSC, step 7');
                      SC_SubmitSettleOperation(A)(B)(prev_state, new_state, deposit, withdrawals, list_signature);
                      console.log('SC_mkCreateSC, step 8');
                      B.run_threads=false;
                      Promise.resolve();
                  }));
    }
    else {
        Promise.resolve();
    }
};



const mkSpanCTC = A => B =>
  ({ abi:      A.abi
   , bytecode: A.bytecode
   , sendrecv: mkSendRecv(A)(B)
   , recv:     mkRecv(A)(B)
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
  , web3_shh:         A.web3_shh
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
