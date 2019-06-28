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
  const v17 = v14;
  const v18 = stdlib.add(v15, v16);
  ctc.send("m0", [v15, v16, v17], v18, () => {
    ctc.recv("e0", (p15, p16, p17, v19) => {
      stdlib.assert(stdlib.equal(v15, p15));
      stdlib.assert(stdlib.equal(v16, p16));
      stdlib.assert(stdlib.equal(v17, p17));
      const v20 = stdlib.add(v15, v16);
      const v21 = stdlib.eq(v19, v20);
      stdlib.assert(v21);
      ctc.recv("e1", (v22, v23) => {
        const v24 = stdlib.eq(v23, v15);
        stdlib.assert(v24);
        const v25 = stdlib.le(0, v22);
        const v26 = stdlib.lt(v22, 3);
        const v27 = v25 ? v26 : false;
        stdlib.assert(v27);
        const v28 = v10;
        const v29 = v2;
        ctc.send("m2", [v15, v16, v17, v22, v28, v29], 0, () => {
          ctc.recv("e2", (p28, p29, v30) => {
            stdlib.assert(stdlib.equal(v28, p28));
            stdlib.assert(stdlib.equal(v29, p29));
            const v31 = stdlib.eq(v30, 0);
            stdlib.assert(v31);
            const v32 = stdlib.uint256_to_bytes(v28);
            const v33 = stdlib.uint256_to_bytes(v29);
            const v34 = stdlib.bytes_cat(v32, v33);
            const v35 = stdlib.keccak256(v34);
            const v36 = stdlib.eq(v17, v35);
            stdlib.assert(v36);
            const v37 = stdlib.le(0, v29);
            const v38 = stdlib.lt(v29, 3);
            const v39 = v37 ? v38 : false;
            stdlib.assert(v39);
            const v40 = stdlib.le(0, v29);
            const v41 = stdlib.lt(v29, 3);
            const v42 = v40 ? v41 : false;
            const v43 = stdlib.le(0, v22);
            const v44 = stdlib.lt(v22, 3);
            const v45 = v43 ? v44 : false;
            const v46 = v42 ? v45 : false;
            const v47 = stdlib.sub(4, v22);
            const v48 = stdlib.add(v29, v47);
            const v49 = stdlib.mod(v48, 3);
            const v50 = v45 ? 0 : 1;
            const v51 = v42 ? 2 : v50;
            const v52 = v46 ? v49 : v51;
            const v53 = stdlib.le(0, v52);
            const v54 = stdlib.lt(v52, 3);
            const v55 = v53 ? v54 : false;
            stdlib.assert(v55);
            const v56 = stdlib.eq(v52, 2);
            const v57 = stdlib.le(0, v29);
            const v58 = stdlib.lt(v29, 3);
            const v59 = v57 ? v58 : false;
            const v60 = v56 ? false : true;
            const v61 = v60 ? true : v59;
            stdlib.assert(v61);
            const v62 = stdlib.eq(v52, 0);
            const v63 = stdlib.le(0, v22);
            const v64 = stdlib.lt(v22, 3);
            const v65 = v63 ? v64 : false;
            const v66 = v62 ? false : true;
            const v67 = v66 ? true : v65;
            stdlib.assert(v67);
            const v68 = stdlib.eq(v52, 2);
            const v69 = stdlib.mul(2, v15);
            const v70 = stdlib.add(v69, v16);
            const v71 = stdlib.eq(v52, 0);
            const v72 = stdlib.mul(2, v15);
            const v73 = stdlib.add(v15, v16);
            const v74 = v71 ? v16 : v73;
            const v75 = v71 ? v72 : v15;
            const v76 = v68 ? v70 : v74;
            const v77 = v68 ? 0 : v75;
            const v78 = stdlib.eq(v52, 2);
            const v79 = stdlib.eq(v52, 0);
            kTop(v52); }); }); }); }); }); }

export function B(ctc, interact, v3, kTop) {
  const v7 = stdlib.le(0, v3);
  const v8 = stdlib.lt(v3, 3);
  const v9 = v7 ? v8 : false;
  stdlib.assert(v9);
  ctc.recv("e0", (v15, v16, v17, v19) => {
    const v20 = stdlib.add(v15, v16);
    const v21 = stdlib.eq(v19, v20);
    stdlib.assert(v21);
    const v22 = v3;
    ctc.send("m1", [v15, v16, v17, v22], v15, () => {
      ctc.recv("e1", (p22, v23) => {
        stdlib.assert(stdlib.equal(v22, p22));
        const v24 = stdlib.eq(v23, v15);
        stdlib.assert(v24);
        const v25 = stdlib.le(0, v22);
        const v26 = stdlib.lt(v22, 3);
        const v27 = v25 ? v26 : false;
        stdlib.assert(v27);
        ctc.recv("e2", (v28, v29, v30) => {
          const v31 = stdlib.eq(v30, 0);
          stdlib.assert(v31);
          const v32 = stdlib.uint256_to_bytes(v28);
          const v33 = stdlib.uint256_to_bytes(v29);
          const v34 = stdlib.bytes_cat(v32, v33);
          const v35 = stdlib.keccak256(v34);
          const v36 = stdlib.eq(v17, v35);
          stdlib.assert(v36);
          const v37 = stdlib.le(0, v29);
          const v38 = stdlib.lt(v29, 3);
          const v39 = v37 ? v38 : false;
          stdlib.assert(v39);
          const v40 = stdlib.le(0, v29);
          const v41 = stdlib.lt(v29, 3);
          const v42 = v40 ? v41 : false;
          const v43 = stdlib.le(0, v22);
          const v44 = stdlib.lt(v22, 3);
          const v45 = v43 ? v44 : false;
          const v46 = v42 ? v45 : false;
          const v47 = stdlib.sub(4, v22);
          const v48 = stdlib.add(v29, v47);
          const v49 = stdlib.mod(v48, 3);
          const v50 = v45 ? 0 : 1;
          const v51 = v42 ? 2 : v50;
          const v52 = v46 ? v49 : v51;
          const v53 = stdlib.le(0, v52);
          const v54 = stdlib.lt(v52, 3);
          const v55 = v53 ? v54 : false;
          stdlib.assert(v55);
          const v56 = stdlib.eq(v52, 2);
          const v57 = stdlib.le(0, v29);
          const v58 = stdlib.lt(v29, 3);
          const v59 = v57 ? v58 : false;
          const v60 = v56 ? false : true;
          const v61 = v60 ? true : v59;
          stdlib.assert(v61);
          const v62 = stdlib.eq(v52, 0);
          const v63 = stdlib.le(0, v22);
          const v64 = stdlib.lt(v22, 3);
          const v65 = v63 ? v64 : false;
          const v66 = v62 ? false : true;
          const v67 = v66 ? true : v65;
          stdlib.assert(v67);
          const v68 = stdlib.eq(v52, 2);
          const v69 = stdlib.mul(2, v15);
          const v70 = stdlib.add(v69, v16);
          const v71 = stdlib.eq(v52, 0);
          const v72 = stdlib.mul(2, v15);
          const v73 = stdlib.add(v15, v16);
          const v74 = v71 ? v16 : v73;
          const v75 = v71 ? v72 : v15;
          const v76 = v68 ? v70 : v74;
          const v77 = v68 ? 0 : v75;
          const v78 = stdlib.eq(v52, 2);
          const v79 = stdlib.eq(v52, 0);
          kTop(v52); }); }); }); }); }

export const ABI = [{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v15","type":"uint256"},{"name":"v16","type":"uint256"},{"name":"v17","type":"uint256"}],"name":"m0","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v15","type":"uint256"},{"name":"v16","type":"uint256"},{"name":"v17","type":"uint256"},{"name":"v22","type":"uint256"},{"name":"v28","type":"uint256"},{"name":"v29","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v15","type":"uint256"},{"name":"v16","type":"uint256"},{"name":"v17","type":"uint256"},{"name":"v22","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"}],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v15","type":"uint256"},{"indexed":false,"name":"v16","type":"uint256"},{"indexed":false,"name":"v17","type":"uint256"}],"name":"e0","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v22","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v28","type":"uint256"},{"indexed":false,"name":"v29","type":"uint256"}],"name":"e2","type":"event"}];

export const Bytecode = "0x6080604052604051610c44380380610c448339818101604052604081101561002657600080fd5b81019080805190602001909291908051906020019092919050505060008282604051602001808481526020018373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b815260140193505050506040516020818303038152906040528051906020012060001c6000819055505050610b53806100f16000396000f3fe6080604052600436106100345760003560e01c8063865ca4e7146100395780639f54e22e146100bb578063b00416101461015c575b600080fd5b6100b9600480360360a081101561004f57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff1690602001909291908035906020019092919080359060200190929190803590602001909291905050506101e8565b005b61015a60048036036101008110156100d257600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff1690602001909291908035906020019092919080359060200190929190803590602001909291908035906020019092919080359060200190929190803590602001909291905050506103d9565b005b6101e6600480360360c081101561017257600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190803590602001909291908035906020019092919080359060200190929190505050610824565b005b60008585604051602001808481526020018373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b815260140193505050506040516020818303038152906040528051906020012060001c6000541461028e57600080fd5b8473ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16146102c657600080fd5b81830134146102d457600080fd5b7fe97939a5c3b22414f653aea9f18c5859841d0af50ba6ede8dd8f1937b8da453883838360405180848152602001838152602001828152602001935050505060405180910390a160018585858585604051602001808781526020018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b815260140184815260200183815260200182815260200196505050505050506040516020818303038152906040528051906020012060001c6000819055505050505050565b6002888888888888604051602001808881526020018773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018581526020018481526020018381526020018281526020019750505050505050506040516020818303038152906040528051906020012060001c6000541461049f57600080fd5b8773ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16146104d757600080fd5b600034146104e457600080fd5b61052c82604051602001808281526020019150506040516020818303038152906040528260405160200180828152602001915050604051602081830303815290604052610a42565b6040516020018082805190602001908083835b60208310610562578051825260208201915060208101905060208303925061053f565b6001836020036101000a0380198251168184511680821785525050505050509050019150506040516020818303038152906040528051906020012060001c84146105ab57600080fd5b80600011156105bb5760006105c0565b600381105b6105c957600080fd5b600081600011156105db5760006105e0565b600382105b9050600084600011156105f45760006105f9565b600385105b905060008261060957600061060b565b815b61063057826106285781610620576001610623565b60005b61062b565b60025b610641565b60038660040385018161063f57fe5b065b90508060001115610653576000610658565b600381105b61066157600080fd5b60028114610670576001610673565b60005b61069157836000111561068757600061068c565b600384105b610694565b60015b61069d57600080fd5b600081146106ac5760016106af565b60005b6106cd5785600011156106c35760006106c8565b600386105b6106d0565b60015b6106d957600080fd5b6000600282149050600080831490508c73ffffffffffffffffffffffffffffffffffffffff166108fc8361071b5782610714578b8d01610716565b8b5b610722565b8b8d600202015b9081150290604051600060405180830381858888f1935050505015801561074d573d6000803e3d6000fd5b508b73ffffffffffffffffffffffffffffffffffffffff166108fc836107825782610778578c61077d565b8c6002025b610785565b60005b9081150290604051600060405180830381858888f193505050501580156107b0573d6000803e3d6000fd5b507f3a074aaea2d51955da8b38c6383720deafa2c498a47a8b8312010f91a80d49d38787604051808381526020018281526020019250505060405180910390a1600080819055507302b463784bc1a49f1647b47a19452ac420dfc65a73ffffffffffffffffffffffffffffffffffffffff16ff5b60018686868686604051602001808781526020018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b815260140184815260200183815260200182815260200196505050505050506040516020818303038152906040528051906020012060001c600054146108e257600080fd5b8473ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff161461091a57600080fd5b83341461092657600080fd5b806000111561093657600061093b565b600381105b61094457600080fd5b7f3680e78b6fdf571695c81f108d81181ea63f50c100e6375e765b14bd7ac0adbb816040518082815260200191505060405180910390a16002868686868686604051602001808881526020018773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1660601b81526014018581526020018481526020018381526020018281526020019750505050505050506040516020818303038152906040528051906020012060001c600081905550505050505050565b606082518383604051602001808461ffff1661ffff1660f01b815260020183805190602001908083835b60208310610a8f5780518252602082019150602081019050602083039250610a6c565b6001836020036101000a03801982511681845116808217855250505050505090500182805190602001908083835b60208310610ae05780518252602082019150602081019050602083039250610abd565b6001836020036101000a038019825116818451168082178552505050505050905001935050505060405160208183030381529060405290509291505056fea265627a7a7230582070a5b5c174e790670d17998e45e5e83998219f397d3392d57399883496cd4aa664736f6c63430005090032";