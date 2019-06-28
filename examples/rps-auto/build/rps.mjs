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

export const Bytecode = "0x60806040526040516107f23803806107f28339818101604052604081101561002657600080fd5b5080516020918201516040805160008186018190526001600160601b0319606095861b8116838501529390941b909216605483015280516048818403018152606890920190528051920191909120905561076d806100856000396000f3fe6080604052600436106100345760003560e01c8063865ca4e7146100395780639f54e22e1461007d578063b0041610146100d2575b600080fd5b61007b600480360360a081101561004f57600080fd5b506001600160a01b0381358116916020810135909116906040810135906060810135906080013561011a565b005b61007b600480360361010081101561009457600080fd5b506001600160a01b03813581169160208101359091169060408101359060608101359060808101359060a08101359060c08101359060e00135610230565b61007b600480360360c08110156100e857600080fd5b506001600160a01b03813581169160208101359091169060408101359060608101359060808101359060a0013561053a565b60408051600060208083018290526001600160601b031960608a811b82168587015289901b1660548401528351604881850301815260689093019093528151919092012090541461016a57600080fd5b336001600160a01b0386161461017f57600080fd5b818301341461018d57600080fd5b604080518481526020810184905280820183905290517fe97939a5c3b22414f653aea9f18c5859841d0af50ba6ede8dd8f1937b8da45389181900360600190a16040805160016020808301919091526001600160601b0319606098891b8116838501529690971b90951660548601526068850191909152608884019290925260a8808401919091528151808403909101815260c890920190528051910120600055565b6040805160026020808301919091526001600160601b031960608c811b8216848601528b901b166054830152606882018990526088820188905260a8820187905260c88083018790528351808403909101815260e890920190925280519101206000541461029d57600080fd5b336001600160a01b038916146102b257600080fd5b34156102bd57600080fd5b60408051602080820185905282518083039091018152818301835260608083018590528351808403909101815260809092019092526102fc9190610669565b6040516020018082805190602001908083835b6020831061032e5780518252601f19909201916020918201910161030f565b6001836020036101000a0380198251168184511680821785525050505050509050019150506040516020818303038152906040528051906020012060001c861461037757600080fd5b6003811061038457600080fd5b600380821090841060008261039a57600061039c565b815b6103c157826103b957816103b15760016103b4565b60005b6103bc565b60025b6103d2565b6003866004038501816103d057fe5b065b9050600381106103e157600080fd5b600281146103f05760016103f3565b60005b6104005760038410610403565b60015b61040c57600080fd5b801561041957600161041c565b60005b610429576003861061042c565b60015b61043557600080fd5b6002811481156001600160a01b038d166108fc83610461578261045a578a8c0161045c565b8a5b610468565b8a8c600202015b6040518115909202916000818181858888f19350505050158015610490573d6000803e3d6000fd5b508b6001600160a01b03166108fc836104b857826104ae578b6104b3565b8b6002025b6104bb565b60005b6040518115909202916000818181858888f193505050501580156104e3573d6000803e3d6000fd5b50604080518881526020810188905281517f3a074aaea2d51955da8b38c6383720deafa2c498a47a8b8312010f91a80d49d3929181900390910190a1600080557302b463784bc1a49f1647b47a19452ac420dfc65aff5b6040805160016020808301919091526001600160601b031960608a811b82168486015289901b166054830152606882018790526088820186905260a88083018690528351808403909101815260c89092019092528051910120600054146105a057600080fd5b336001600160a01b038616146105b557600080fd5b8234146105c157600080fd5b600381106105ce57600080fd5b6040805182815290517f3680e78b6fdf571695c81f108d81181ea63f50c100e6375e765b14bd7ac0adbb9181900360200190a16040805160026020808301919091526001600160601b03196060998a1b8116838501529790981b90961660548701526068860194909452608885019290925260a884015260c8808401919091528151808403909101815260e890920190528051910120600055565b606082518383604051602001808461ffff1661ffff1660f01b815260020183805190602001908083835b602083106106b25780518252601f199092019160209182019101610693565b51815160209384036101000a600019018019909216911617905285519190930192850191508083835b602083106106fa5780518252601f1990920191602091820191016106db565b6001836020036101000a038019825116818451168082178552505050505050905001935050505060405160208183030381529060405290509291505056fea265627a7a723058209acf13b5c7bd3d18a5e511e5226cdc9f3437c7e1c31fdd1c46c3625a47bd1a5c64736f6c63430005090032";