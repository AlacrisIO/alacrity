export function A(stdlib, ctc, interact, v0, v1, v2, kTop) {
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
            kTop(v52); }); }); }); }); }); }

export function B(stdlib, ctc, interact, v3, kTop) {
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
          kTop(v52); }); }); }); }); }

export const ABI = [{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v15","type":"uint256"},{"name":"v16","type":"uint256"},{"name":"v17","type":"uint256"}],"name":"m0","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v15","type":"uint256"},{"name":"v16","type":"uint256"},{"name":"v17","type":"uint256"},{"name":"v22","type":"uint256"},{"name":"v28","type":"uint256"},{"name":"v29","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v15","type":"uint256"},{"name":"v16","type":"uint256"},{"name":"v17","type":"uint256"},{"name":"v22","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"}],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v15","type":"uint256"},{"indexed":false,"name":"v16","type":"uint256"},{"indexed":false,"name":"v17","type":"uint256"}],"name":"e0","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v22","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v28","type":"uint256"},{"indexed":false,"name":"v29","type":"uint256"}],"name":"e2","type":"event"}];

export const Bytecode = "0x60806040818152806108498339810180604052604081101561002057600080fd5b5080516020918201516040805160008186018190526c01000000000000000000000000600160a060020a0395861681028385015294909316909302605484015280516048818503018152606890930190528151919092012090556107c0806100896000396000f3fe608060405260043610610050577c01000000000000000000000000000000000000000000000000000000006000350463865ca4e781146100555780639f54e22e14610099578063b0041610146100ee575b600080fd5b610097600480360360a081101561006b57600080fd5b50600160a060020a03813581169160208101359091169060408101359060608101359060800135610136565b005b61009760048036036101008110156100b057600080fd5b50600160a060020a03813581169160208101359091169060408101359060608101359060808101359060a08101359060c08101359060e00135610261565b610097600480360360c081101561010457600080fd5b50600160a060020a03813581169160208101359091169060408101359060608101359060808101359060a00135610550565b60408051600060208083018290526c01000000000000000000000000600160a060020a03808b1682028587015289160260548401528351604881850301815260689093019093528151919092012090541461019057600080fd5b33600160a060020a038616146101a557600080fd5b34838301146101b357600080fd5b604080518481526020810184905280820183905290517fe97939a5c3b22414f653aea9f18c5859841d0af50ba6ede8dd8f1937b8da45389181900360600190a16040805160016020808301919091526c01000000000000000000000000600160a060020a039889168102838501529790961690960260548701526068860193909352608885019190915260a8808501919091528151808503909101815260c890930190528151910120600055565b6040805160026020808301919091526c01000000000000000000000000600160a060020a03808d168202848601528b16026054830152606882018990526088820188905260a8820187905260c88083018790528351808403909101815260e89092019092528051910120600054146102d857600080fd5b33600160a060020a038916146102ed57600080fd5b34156102f857600080fd5b604080516020808201859052825180830390910181528183018352606080830185905283518084039091018152608090920190925261033791906106a7565b6040516020018082805190602001908083835b602083106103695780518252601f19909201916020918201910161034a565b6001836020036101000a0380198251168184511680821785525050505050509050019150506040516020818303038152906040528051906020012060019004841415156103b557600080fd5b80600011156103c55760006103ca565b600381105b15156103d557600080fd5b600081600011156103e75760006103ec565b600382105b905060008460001115610400576000610405565b600385105b9050600082610415576000610417565b815b61043c5782610434578161042c57600161042f565b60005b610437565b60025b610447565b600360048790038501065b9050600281148115600160a060020a038d166108fc83610474578261046c578c61046f565b60005b610479565b8c6002025b8c019081150290604051600060405180830381858888f193505050501580156104a6573d6000803e3d6000fd5b508b600160a060020a03166108fc836104ce57826104c4578c6104c9565b8c6002025b6104d1565b60005b6040518115909202916000818181858888f193505050501580156104f9573d6000803e3d6000fd5b50604080518881526020810188905281517f3a074aaea2d51955da8b38c6383720deafa2c498a47a8b8312010f91a80d49d3929181900390910190a1600080557302b463784bc1a49f1647b47a19452ac420dfc65aff5b6040805160016020808301919091526c01000000000000000000000000600160a060020a03808b168202848601528916026054830152606882018790526088820186905260a88083018690528351808403909101815260c89092019092528051910120600054146105c057600080fd5b33600160a060020a038616146105d557600080fd5b3484146105e157600080fd5b80600011156105f15760006105f6565b600381105b151561060157600080fd5b6040805182815290517f3680e78b6fdf571695c81f108d81181ea63f50c100e6375e765b14bd7ac0adbb9181900360200190a16040805160026020808301919091526c01000000000000000000000000600160a060020a03998a168102838501529890971690970260548801526068870194909452608886019290925260a885015260c8808501919091528151808503909101815260e890930190528151910120600055565b606082518383604051602001808461ffff1661ffff167e0100000000000000000000000000000000000000000000000000000000000002815260020183805190602001908083835b6020831061070e5780518252601f1990920191602091820191016106ef565b51815160209384036101000a600019018019909216911617905285519190930192850191508083835b602083106107565780518252601f199092019160209182019101610737565b6001836020036101000a038019825116818451168082178552505050505050905001935050505060405160208183030381529060405290509291505056fea165627a7a72305820856a870c8d0b190925bdb7c4dbaf8fb279b643421475103c4e62c03b3fa055ba0029";