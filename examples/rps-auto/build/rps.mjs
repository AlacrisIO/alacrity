import { stdlib } from '../rps/alacrity-runtime.mjs';

export function A(ctc, interact, v0, v1, v2, kTop) {
  const v4 = stdlib.le(0, v2);
  const v5 = stdlib.lt(v2, 3);
  const v6 = v4 ? v5 : false;
  stdlib.assert(v6);
  const v10 = stdlib.random_uint256();
  const v11 = stdlib.uint256_to_bytes(v10);
  const v12 = stdlib.uint256_to_bytes(v2);
  const v13 = stdlib.bytes_cat(v11, v12);
  const v14 = stdlib.keccak256(v13);
  const v15 = v0;
  const v16 = v1;
  const v17 = stdlib.add(v15, v16);
  ctc.send("m0", [v15, v16, v14], v17, () => {
    ctc.recv("e0", (p15, p16, p14, v18) => {
      stdlib.assert(stdlib.equal(v15, p15));
      stdlib.assert(stdlib.equal(v16, p16));
      stdlib.assert(stdlib.equal(v14, p14));
      const v19 = stdlib.add(v15, v16);
      const v20 = stdlib.eq(v18, v19);
      stdlib.assert(v20);
      ctc.recv("e1", (v21, v22) => {
        const v23 = stdlib.eq(v22, v15);
        stdlib.assert(v23);
        const v24 = stdlib.le(0, v21);
        const v25 = stdlib.lt(v21, 3);
        const v26 = v24 ? v25 : false;
        stdlib.assert(v26);
        const v27 = v10;
        const v28 = v2;
        ctc.send("m2", [v14, v15, v16, v21, v27, v28], 0, () => {
          ctc.recv("e2", (p27, p28, v29) => {
            stdlib.assert(stdlib.equal(v27, p27));
            stdlib.assert(stdlib.equal(v28, p28));
            const v30 = stdlib.eq(v29, 0);
            stdlib.assert(v30);
            const v31 = stdlib.uint256_to_bytes(v27);
            const v32 = stdlib.uint256_to_bytes(v28);
            const v33 = stdlib.bytes_cat(v31, v32);
            const v34 = stdlib.keccak256(v33);
            const v35 = stdlib.eq(v14, v34);
            stdlib.assert(v35);
            const v36 = stdlib.le(0, v28);
            const v37 = stdlib.lt(v28, 3);
            const v38 = v36 ? v37 : false;
            stdlib.assert(v38);
            const v39 = stdlib.le(0, v28);
            const v40 = stdlib.lt(v28, 3);
            const v41 = v39 ? v40 : false;
            const v42 = stdlib.le(0, v21);
            const v43 = stdlib.lt(v21, 3);
            const v44 = v42 ? v43 : false;
            const v45 = v41 ? v44 : false;
            const v46 = stdlib.sub(4, v21);
            const v47 = stdlib.add(v28, v46);
            const v48 = stdlib.mod(v47, 3);
            const v49 = v44 ? 0 : 1;
            const v50 = v41 ? 2 : v49;
            const v51 = v45 ? v48 : v50;
            const v52 = stdlib.le(0, v51);
            const v53 = stdlib.lt(v51, 3);
            const v54 = v52 ? v53 : false;
            stdlib.assert(v54);
            const v55 = stdlib.eq(v51, 2);
            const v56 = stdlib.le(0, v28);
            const v57 = stdlib.lt(v28, 3);
            const v58 = v56 ? v57 : false;
            const v59 = v55 ? false : true;
            const v60 = v59 ? true : v58;
            stdlib.assert(v60);
            const v61 = stdlib.eq(v51, 0);
            const v62 = stdlib.le(0, v21);
            const v63 = stdlib.lt(v21, 3);
            const v64 = v62 ? v63 : false;
            const v65 = v61 ? false : true;
            const v66 = v65 ? true : v64;
            stdlib.assert(v66);
            const v67 = stdlib.eq(v51, 2);
            const v68 = stdlib.mul(2, v15);
            const v69 = stdlib.add(v68, v16);
            const v70 = stdlib.eq(v51, 0);
            const v71 = stdlib.mul(2, v15);
            const v72 = stdlib.add(v15, v16);
            const v73 = v70 ? v16 : v72;
            const v74 = v70 ? v71 : v15;
            const v75 = v67 ? v69 : v73;
            const v76 = v67 ? 0 : v74;
            const v77 = stdlib.eq(v51, 2);
            const v78 = stdlib.eq(v51, 0);
            kTop(v51); }); }); }); }); }); }

export function B(ctc, interact, v3, kTop) {
  const v7 = stdlib.le(0, v3);
  const v8 = stdlib.lt(v3, 3);
  const v9 = v7 ? v8 : false;
  stdlib.assert(v9);
  ctc.recv("e0", (v15, v16, v14, v18) => {
    const v19 = stdlib.add(v15, v16);
    const v20 = stdlib.eq(v18, v19);
    stdlib.assert(v20);
    const v21 = v3;
    ctc.send("m1", [v14, v15, v16, v21], v15, () => {
      ctc.recv("e1", (p21, v22) => {
        stdlib.assert(stdlib.equal(v21, p21));
        const v23 = stdlib.eq(v22, v15);
        stdlib.assert(v23);
        const v24 = stdlib.le(0, v21);
        const v25 = stdlib.lt(v21, 3);
        const v26 = v24 ? v25 : false;
        stdlib.assert(v26);
        ctc.recv("e2", (v27, v28, v29) => {
          const v30 = stdlib.eq(v29, 0);
          stdlib.assert(v30);
          const v31 = stdlib.uint256_to_bytes(v27);
          const v32 = stdlib.uint256_to_bytes(v28);
          const v33 = stdlib.bytes_cat(v31, v32);
          const v34 = stdlib.keccak256(v33);
          const v35 = stdlib.eq(v14, v34);
          stdlib.assert(v35);
          const v36 = stdlib.le(0, v28);
          const v37 = stdlib.lt(v28, 3);
          const v38 = v36 ? v37 : false;
          stdlib.assert(v38);
          const v39 = stdlib.le(0, v28);
          const v40 = stdlib.lt(v28, 3);
          const v41 = v39 ? v40 : false;
          const v42 = stdlib.le(0, v21);
          const v43 = stdlib.lt(v21, 3);
          const v44 = v42 ? v43 : false;
          const v45 = v41 ? v44 : false;
          const v46 = stdlib.sub(4, v21);
          const v47 = stdlib.add(v28, v46);
          const v48 = stdlib.mod(v47, 3);
          const v49 = v44 ? 0 : 1;
          const v50 = v41 ? 2 : v49;
          const v51 = v45 ? v48 : v50;
          const v52 = stdlib.le(0, v51);
          const v53 = stdlib.lt(v51, 3);
          const v54 = v52 ? v53 : false;
          stdlib.assert(v54);
          const v55 = stdlib.eq(v51, 2);
          const v56 = stdlib.le(0, v28);
          const v57 = stdlib.lt(v28, 3);
          const v58 = v56 ? v57 : false;
          const v59 = v55 ? false : true;
          const v60 = v59 ? true : v58;
          stdlib.assert(v60);
          const v61 = stdlib.eq(v51, 0);
          const v62 = stdlib.le(0, v21);
          const v63 = stdlib.lt(v21, 3);
          const v64 = v62 ? v63 : false;
          const v65 = v61 ? false : true;
          const v66 = v65 ? true : v64;
          stdlib.assert(v66);
          const v67 = stdlib.eq(v51, 2);
          const v68 = stdlib.mul(2, v15);
          const v69 = stdlib.add(v68, v16);
          const v70 = stdlib.eq(v51, 0);
          const v71 = stdlib.mul(2, v15);
          const v72 = stdlib.add(v15, v16);
          const v73 = v70 ? v16 : v72;
          const v74 = v70 ? v71 : v15;
          const v75 = v67 ? v69 : v73;
          const v76 = v67 ? 0 : v74;
          const v77 = stdlib.eq(v51, 2);
          const v78 = stdlib.eq(v51, 0);
          kTop(v51); }); }); }); }); }

export const ABI = [{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v15","type":"uint256"},{"name":"v16","type":"uint256"},{"name":"v14","type":"uint256"}],"name":"m0","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v14","type":"uint256"},{"name":"v15","type":"uint256"},{"name":"v16","type":"uint256"},{"name":"v21","type":"uint256"},{"name":"v27","type":"uint256"},{"name":"v28","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v14","type":"uint256"},{"name":"v15","type":"uint256"},{"name":"v16","type":"uint256"},{"name":"v21","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"}],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v15","type":"uint256"},{"indexed":false,"name":"v16","type":"uint256"},{"indexed":false,"name":"v14","type":"uint256"}],"name":"e0","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v21","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v27","type":"uint256"},{"indexed":false,"name":"v28","type":"uint256"}],"name":"e2","type":"event"}];

export const Bytecode = "0x6080604052604051610c44380380610c448339818101604052604081101561002657600080fd5b81019080805190602001909291908051906020019092919050505060008282604051602001808481526020018373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b815260140193505050506040516020818303038152906040528051906020012060001c6000819055505050610b53806100f16000396000f3fe6080604052600436106100345760003560e01c8063865ca4e7146100395780639f54e22e146100bb578063b00416101461015c575b600080fd5b6100b9600480360360a081101561004f57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff1690602001909291908035906020019092919080359060200190929190803590602001909291905050506101e8565b005b61015a60048036036101008110156100d257600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff1690602001909291908035906020019092919080359060200190929190803590602001909291908035906020019092919080359060200190929190803590602001909291905050506103d9565b005b6101e6600480360360c081101561017257600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190803590602001909291908035906020019092919080359060200190929190505050610824565b005b60008585604051602001808481526020018373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b815260140193505050506040516020818303038152906040528051906020012060001c6000541461028e57600080fd5b8473ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16146102c657600080fd5b81830134146102d457600080fd5b7fe97939a5c3b22414f653aea9f18c5859841d0af50ba6ede8dd8f1937b8da453883838360405180848152602001838152602001828152602001935050505060405180910390a160018585838686604051602001808781526020018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b815260140184815260200183815260200182815260200196505050505050506040516020818303038152906040528051906020012060001c6000819055505050505050565b6002888888888888604051602001808881526020018773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018581526020018481526020018381526020018281526020019750505050505050506040516020818303038152906040528051906020012060001c6000541461049f57600080fd5b8773ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16146104d757600080fd5b600034146104e457600080fd5b61052c82604051602001808281526020019150506040516020818303038152906040528260405160200180828152602001915050604051602081830303815290604052610a42565b6040516020018082805190602001908083835b60208310610562578051825260208201915060208101905060208303925061053f565b6001836020036101000a0380198251168184511680821785525050505050509050019150506040516020818303038152906040528051906020012060001c86146105ab57600080fd5b80600011156105bb5760006105c0565b600381105b6105c957600080fd5b600081600011156105db5760006105e0565b600382105b9050600084600011156105f45760006105f9565b600385105b905060008261060957600061060b565b815b61063057826106285781610620576001610623565b60005b61062b565b60025b610641565b60038660040385018161063f57fe5b065b90508060001115610653576000610658565b600381105b61066157600080fd5b60028114610670576001610673565b60005b61069157836000111561068757600061068c565b600384105b610694565b60015b61069d57600080fd5b600081146106ac5760016106af565b60005b6106cd5785600011156106c35760006106c8565b600386105b6106d0565b60015b6106d957600080fd5b6000600282149050600080831490508c73ffffffffffffffffffffffffffffffffffffffff166108fc8361071b5782610714578a8c01610716565b8a5b610722565b8a8c600202015b9081150290604051600060405180830381858888f1935050505015801561074d573d6000803e3d6000fd5b508b73ffffffffffffffffffffffffffffffffffffffff166108fc836107825782610778578b61077d565b8b6002025b610785565b60005b9081150290604051600060405180830381858888f193505050501580156107b0573d6000803e3d6000fd5b507f3a074aaea2d51955da8b38c6383720deafa2c498a47a8b8312010f91a80d49d38787604051808381526020018281526020019250505060405180910390a1600080819055507302b463784bc1a49f1647b47a19452ac420dfc65a73ffffffffffffffffffffffffffffffffffffffff16ff5b60018686868686604051602001808781526020018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b815260140184815260200183815260200182815260200196505050505050506040516020818303038152906040528051906020012060001c600054146108e257600080fd5b8473ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff161461091a57600080fd5b82341461092657600080fd5b806000111561093657600061093b565b600381105b61094457600080fd5b7f3680e78b6fdf571695c81f108d81181ea63f50c100e6375e765b14bd7ac0adbb816040518082815260200191505060405180910390a16002868686868686604051602001808881526020018773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018581526020018481526020018381526020018281526020019750505050505050506040516020818303038152906040528051906020012060001c600081905550505050505050565b606082518383604051602001808461ffff1661ffff1660f01b815260020183805190602001908083835b60208310610a8f5780518252602082019150602081019050602083039250610a6c565b6001836020036101000a03801982511681845116808217855250505050505090500182805190602001908083835b60208310610ae05780518252602082019150602081019050602083039250610abd565b6001836020036101000a038019825116818451168082178552505050505050905001935050505060405160208183030381529060405290509291505056fea265627a7a723058205fd1bd6e037dc0d968fe84f45ee9912e80c74114271f54e560d19b0078f018e064736f6c63430005090032";