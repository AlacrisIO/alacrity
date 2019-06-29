export function A(stdlib, ctc, interact, v0, v1, v2, kTop) {
  const v4 = stdlib.gt(v1, 0);
  stdlib.assert(v4);
  const v5 = stdlib.gt(v0, 0);
  stdlib.assert(v5);
  const v6 = stdlib.le(0, v2);
  const v7 = stdlib.lt(v2, 3);
  const v8 = v6 ? v7 : false;
  stdlib.assert(v8);
  const v12 = stdlib.random_uint256();
  const v13 = stdlib.uint256_to_bytes(v12);
  const v14 = stdlib.uint256_to_bytes(v2);
  const v15 = stdlib.bytes_cat(v13, v14);
  const v16 = stdlib.keccak256(v15);
  const v17 = v0;
  const v18 = v1;
  const v19 = stdlib.add(v17, v18);
  ctc.send("m0", [v17, v18, v16], v19, () => {
    ctc.recv("e0", (p17, p18, p16, v20) => {
      stdlib.assert(stdlib.equal(v17, p17));
      stdlib.assert(stdlib.equal(v18, p18));
      stdlib.assert(stdlib.equal(v16, p16));
      const v21 = stdlib.add(v17, v18);
      const v22 = stdlib.eq(v20, v21);
      stdlib.assert(v22);
      ctc.recv("e1", (v23, v24) => {
        const v25 = stdlib.eq(v24, v17);
        stdlib.assert(v25);
        const v26 = stdlib.le(0, v23);
        const v27 = stdlib.lt(v23, 3);
        const v28 = v26 ? v27 : false;
        stdlib.assert(v28);
        const v29 = v12;
        const v30 = v2;
        ctc.send("m2", [v16, v17, v18, v23, v29, v30], 0, () => {
          ctc.recv("e2", (p29, p30, v31) => {
            stdlib.assert(stdlib.equal(v29, p29));
            stdlib.assert(stdlib.equal(v30, p30));
            const v32 = stdlib.eq(v31, 0);
            stdlib.assert(v32);
            const v33 = stdlib.uint256_to_bytes(v29);
            const v34 = stdlib.uint256_to_bytes(v30);
            const v35 = stdlib.bytes_cat(v33, v34);
            const v36 = stdlib.keccak256(v35);
            const v37 = stdlib.eq(v16, v36);
            stdlib.assert(v37);
            const v38 = stdlib.le(0, v30);
            const v39 = stdlib.lt(v30, 3);
            const v40 = v38 ? v39 : false;
            stdlib.assert(v40);
            const v41 = stdlib.le(0, v30);
            const v42 = stdlib.lt(v30, 3);
            const v43 = v41 ? v42 : false;
            const v44 = stdlib.le(0, v23);
            const v45 = stdlib.lt(v23, 3);
            const v46 = v44 ? v45 : false;
            const v47 = v43 ? v46 : false;
            const v48 = stdlib.sub(4, v23);
            const v49 = stdlib.add(v30, v48);
            const v50 = stdlib.mod(v49, 3);
            const v51 = v46 ? 0 : 1;
            const v52 = v43 ? 2 : v51;
            const v53 = v47 ? v50 : v52;
            const v54 = stdlib.le(0, v53);
            const v55 = stdlib.lt(v53, 3);
            const v56 = v54 ? v55 : false;
            const v57 = stdlib.eq(v53, 2);
            const v58 = stdlib.le(0, v30);
            const v59 = stdlib.lt(v30, 3);
            const v60 = v58 ? v59 : false;
            const v61 = v57 ? false : true;
            const v62 = v61 ? true : v60;
            const v63 = stdlib.eq(v53, 0);
            const v64 = stdlib.le(0, v23);
            const v65 = stdlib.lt(v23, 3);
            const v66 = v64 ? v65 : false;
            const v67 = v63 ? false : true;
            const v68 = v67 ? true : v66;
            const v69 = stdlib.eq(v53, 2);
            const v70 = stdlib.mul(2, v17);
            const v71 = stdlib.eq(v53, 0);
            const v72 = stdlib.mul(2, v17);
            const v73 = v71 ? 0 : v17;
            const v74 = v71 ? v72 : v17;
            const v75 = v69 ? v70 : v73;
            const v76 = v69 ? 0 : v74;
            const v77 = stdlib.add(v18, v75);
            const v78 = stdlib.eq(v53, 2);
            const v79 = stdlib.eq(v53, 0);
            kTop(v53); }); }); }); }); }); }

export function B(stdlib, ctc, interact, v3, kTop) {
  const v9 = stdlib.le(0, v3);
  const v10 = stdlib.lt(v3, 3);
  const v11 = v9 ? v10 : false;
  stdlib.assert(v11);
  ctc.recv("e0", (v17, v18, v16, v20) => {
    const v21 = stdlib.add(v17, v18);
    const v22 = stdlib.eq(v20, v21);
    stdlib.assert(v22);
    const v23 = v3;
    ctc.send("m1", [v16, v17, v18, v23], v17, () => {
      ctc.recv("e1", (p23, v24) => {
        stdlib.assert(stdlib.equal(v23, p23));
        const v25 = stdlib.eq(v24, v17);
        stdlib.assert(v25);
        const v26 = stdlib.le(0, v23);
        const v27 = stdlib.lt(v23, 3);
        const v28 = v26 ? v27 : false;
        stdlib.assert(v28);
        ctc.recv("e2", (v29, v30, v31) => {
          const v32 = stdlib.eq(v31, 0);
          stdlib.assert(v32);
          const v33 = stdlib.uint256_to_bytes(v29);
          const v34 = stdlib.uint256_to_bytes(v30);
          const v35 = stdlib.bytes_cat(v33, v34);
          const v36 = stdlib.keccak256(v35);
          const v37 = stdlib.eq(v16, v36);
          stdlib.assert(v37);
          const v38 = stdlib.le(0, v30);
          const v39 = stdlib.lt(v30, 3);
          const v40 = v38 ? v39 : false;
          stdlib.assert(v40);
          const v41 = stdlib.le(0, v30);
          const v42 = stdlib.lt(v30, 3);
          const v43 = v41 ? v42 : false;
          const v44 = stdlib.le(0, v23);
          const v45 = stdlib.lt(v23, 3);
          const v46 = v44 ? v45 : false;
          const v47 = v43 ? v46 : false;
          const v48 = stdlib.sub(4, v23);
          const v49 = stdlib.add(v30, v48);
          const v50 = stdlib.mod(v49, 3);
          const v51 = v46 ? 0 : 1;
          const v52 = v43 ? 2 : v51;
          const v53 = v47 ? v50 : v52;
          const v54 = stdlib.le(0, v53);
          const v55 = stdlib.lt(v53, 3);
          const v56 = v54 ? v55 : false;
          const v57 = stdlib.eq(v53, 2);
          const v58 = stdlib.le(0, v30);
          const v59 = stdlib.lt(v30, 3);
          const v60 = v58 ? v59 : false;
          const v61 = v57 ? false : true;
          const v62 = v61 ? true : v60;
          const v63 = stdlib.eq(v53, 0);
          const v64 = stdlib.le(0, v23);
          const v65 = stdlib.lt(v23, 3);
          const v66 = v64 ? v65 : false;
          const v67 = v63 ? false : true;
          const v68 = v67 ? true : v66;
          const v69 = stdlib.eq(v53, 2);
          const v70 = stdlib.mul(2, v17);
          const v71 = stdlib.eq(v53, 0);
          const v72 = stdlib.mul(2, v17);
          const v73 = v71 ? 0 : v17;
          const v74 = v71 ? v72 : v17;
          const v75 = v69 ? v70 : v73;
          const v76 = v69 ? 0 : v74;
          const v77 = stdlib.add(v18, v75);
          const v78 = stdlib.eq(v53, 2);
          const v79 = stdlib.eq(v53, 0);
          kTop(v53); }); }); }); }); }

export const ABI = [{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v17","type":"uint256"},{"name":"v18","type":"uint256"},{"name":"v16","type":"uint256"}],"name":"m0","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v16","type":"uint256"},{"name":"v17","type":"uint256"},{"name":"v18","type":"uint256"},{"name":"v23","type":"uint256"},{"name":"v29","type":"uint256"},{"name":"v30","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v16","type":"uint256"},{"name":"v17","type":"uint256"},{"name":"v18","type":"uint256"},{"name":"v23","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"}],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v17","type":"uint256"},{"indexed":false,"name":"v18","type":"uint256"},{"indexed":false,"name":"v16","type":"uint256"}],"name":"e0","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v23","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v29","type":"uint256"},{"indexed":false,"name":"v30","type":"uint256"}],"name":"e2","type":"event"}];

export const Bytecode = "0x60806040526040516107933803806107938339818101604052604081101561002657600080fd5b5080516020918201516040805160008186018190526001600160601b0319606095861b8116838501529390941b909216605483015280516048818403018152606890920190528051920191909120905561070e806100856000396000f3fe6080604052600436106100345760003560e01c8063865ca4e7146100395780639f54e22e1461007d578063b0041610146100d2575b600080fd5b61007b600480360360a081101561004f57600080fd5b506001600160a01b0381358116916020810135909116906040810135906060810135906080013561011a565b005b61007b600480360361010081101561009457600080fd5b506001600160a01b03813581169160208101359091169060408101359060608101359060808101359060a08101359060c08101359060e00135610230565b61007b600480360360c08110156100e857600080fd5b506001600160a01b03813581169160208101359091169060408101359060608101359060808101359060a001356104db565b60408051600060208083018290526001600160601b031960608a811b82168587015289901b1660548401528351604881850301815260689093019093528151919092012090541461016a57600080fd5b336001600160a01b0386161461017f57600080fd5b818301341461018d57600080fd5b604080518481526020810184905280820183905290517fe97939a5c3b22414f653aea9f18c5859841d0af50ba6ede8dd8f1937b8da45389181900360600190a16040805160016020808301919091526001600160601b0319606098891b8116838501529690971b90951660548601526068850191909152608884019290925260a8808401919091528151808403909101815260c890920190528051910120600055565b6040805160026020808301919091526001600160601b031960608c811b8216848601528b901b166054830152606882018990526088820188905260a8820187905260c88083018790528351808403909101815260e890920190925280519101206000541461029d57600080fd5b336001600160a01b038916146102b257600080fd5b34156102bd57600080fd5b60408051602080820185905282518083039091018152818301835260608083018590528351808403909101815260809092019092526102fc919061060a565b6040516020018082805190602001908083835b6020831061032e5780518252601f19909201916020918201910161030f565b6001836020036101000a0380198251168184511680821785525050505050509050019150506040516020818303038152906040528051906020012060001c861461037757600080fd5b6003811061038457600080fd5b600380821090841060008261039a57600061039c565b815b6103c157826103b957816103b15760016103b4565b60005b6103bc565b60025b6103d2565b6003866004038501816103d057fe5b065b90506002811481156001600160a01b038d166108fc836103ff57826103f7578b6103fa565b60005b610404565b8b6002025b8b019081150290604051600060405180830381858888f19350505050158015610431573d6000803e3d6000fd5b508b6001600160a01b03166108fc83610459578261044f578b610454565b8b6002025b61045c565b60005b6040518115909202916000818181858888f19350505050158015610484573d6000803e3d6000fd5b50604080518881526020810188905281517f3a074aaea2d51955da8b38c6383720deafa2c498a47a8b8312010f91a80d49d3929181900390910190a1600080557302b463784bc1a49f1647b47a19452ac420dfc65aff5b6040805160016020808301919091526001600160601b031960608a811b82168486015289901b166054830152606882018790526088820186905260a88083018690528351808403909101815260c890920190925280519101206000541461054157600080fd5b336001600160a01b0386161461055657600080fd5b82341461056257600080fd5b6003811061056f57600080fd5b6040805182815290517f3680e78b6fdf571695c81f108d81181ea63f50c100e6375e765b14bd7ac0adbb9181900360200190a16040805160026020808301919091526001600160601b03196060998a1b8116838501529790981b90961660548701526068860194909452608885019290925260a884015260c8808401919091528151808403909101815260e890920190528051910120600055565b606082518383604051602001808461ffff1661ffff1660f01b815260020183805190602001908083835b602083106106535780518252601f199092019160209182019101610634565b51815160209384036101000a600019018019909216911617905285519190930192850191508083835b6020831061069b5780518252601f19909201916020918201910161067c565b6001836020036101000a038019825116818451168082178552505050505050905001935050505060405160208183030381529060405290509291505056fea265627a7a723058208d9430a19ec9d1e985a967157b0ea59af3e4754f1c41a77138cc4f0c3634376164736f6c63430005090032";