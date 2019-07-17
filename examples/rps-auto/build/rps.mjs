export function A(stdlib, ctc, interact, v0, v1, kTop) {
  interact("getHand", (v2) => {
    const v3 = stdlib.bytes_eq(v2, "ROCK");
    const v4 = stdlib.bytes_eq(v2, "PAPER");
    const v5 = stdlib.bytes_eq(v2, "SCISSORS");
    const v6 = v4 ? true : v5;
    const v7 = v3 ? true : v6;
    stdlib.assert(v7);
    const v8 = v4 ? 1 : 2;
    const v9 = v3 ? 0 : v8;
    const v13 = stdlib.random_uint256();
    const v14 = stdlib.uint256_to_bytes(v13);
    const v15 = stdlib.uint256_to_bytes(v9);
    const v16 = stdlib.bytes_cat(v14, v15);
    const v17 = stdlib.keccak256(v16);
    const v18 = v0;
    const v19 = v1;
    const v20 = v17;
    interact("commits", (v21) => {
      const v22 = stdlib.add(v18, v19);
      ctc.send("m0", [v18, v19, v20], v22, () => {
        ctc.recv("e0", (p18, p19, p20, v23) => {
          stdlib.assert(stdlib.equal(v18, p18));
          stdlib.assert(stdlib.equal(v19, p19));
          stdlib.assert(stdlib.equal(v20, p20));
          const v24 = stdlib.add(v18, v19);
          const v25 = stdlib.eq(v23, v24);
          stdlib.assert(v25);
          ctc.recv("e1", (v37, v39) => {
            const v40 = stdlib.eq(v39, v18);
            stdlib.assert(v40);
            const v41 = stdlib.le(0, v37);
            const v42 = stdlib.lt(v37, 3);
            const v43 = v41 ? v42 : false;
            stdlib.assert(v43);
            const v44 = v13;
            const v45 = v9;
            interact("reveals", (v46) => {
              ctc.send("m2", [v18, v19, v20, v37, v44, v45], 0, () => {
                ctc.recv("e2", (p44, p45, v47) => {
                  stdlib.assert(stdlib.equal(v44, p44));
                  stdlib.assert(stdlib.equal(v45, p45));
                  const v48 = stdlib.eq(v47, 0);
                  stdlib.assert(v48);
                  const v49 = stdlib.uint256_to_bytes(v44);
                  const v50 = stdlib.uint256_to_bytes(v45);
                  const v51 = stdlib.bytes_cat(v49, v50);
                  const v52 = stdlib.keccak256(v51);
                  const v53 = stdlib.eq(v20, v52);
                  stdlib.assert(v53);
                  const v54 = stdlib.le(0, v45);
                  const v55 = stdlib.lt(v45, 3);
                  const v56 = v54 ? v55 : false;
                  stdlib.assert(v56);
                  const v57 = stdlib.le(0, v45);
                  const v58 = stdlib.lt(v45, 3);
                  const v59 = v57 ? v58 : false;
                  const v60 = stdlib.le(0, v37);
                  const v61 = stdlib.lt(v37, 3);
                  const v62 = v60 ? v61 : false;
                  const v63 = v59 ? v62 : false;
                  const v64 = stdlib.sub(4, v37);
                  const v65 = stdlib.add(v45, v64);
                  const v66 = stdlib.mod(v65, 3);
                  const v67 = v62 ? 0 : 1;
                  const v68 = v59 ? 2 : v67;
                  const v69 = v63 ? v66 : v68;
                  interact("outcome", (v108) => {
                    kTop(v69); }); }); }); }); }); }); }); }); }); }

export function B(stdlib, ctc, interact, kTop) {
  ctc.recv("e0", (v18, v19, v20, v23) => {
    const v24 = stdlib.add(v18, v19);
    const v25 = stdlib.eq(v23, v24);
    stdlib.assert(v25);
    interact("getHand", (v26) => {
      const v27 = stdlib.bytes_eq(v26, "ROCK");
      const v28 = stdlib.bytes_eq(v26, "PAPER");
      const v29 = stdlib.bytes_eq(v26, "SCISSORS");
      const v30 = v28 ? true : v29;
      const v31 = v27 ? true : v30;
      stdlib.assert(v31);
      const v32 = v28 ? 1 : 2;
      const v33 = v27 ? 0 : v32;
      const v37 = v33;
      interact("accepts", (v38) => {
        ctc.send("m1", [v18, v19, v20, v37], v18, () => {
          ctc.recv("e1", (p37, v39) => {
            stdlib.assert(stdlib.equal(v37, p37));
            const v40 = stdlib.eq(v39, v18);
            stdlib.assert(v40);
            const v41 = stdlib.le(0, v37);
            const v42 = stdlib.lt(v37, 3);
            const v43 = v41 ? v42 : false;
            stdlib.assert(v43);
            ctc.recv("e2", (v44, v45, v47) => {
              const v48 = stdlib.eq(v47, 0);
              stdlib.assert(v48);
              const v49 = stdlib.uint256_to_bytes(v44);
              const v50 = stdlib.uint256_to_bytes(v45);
              const v51 = stdlib.bytes_cat(v49, v50);
              const v52 = stdlib.keccak256(v51);
              const v53 = stdlib.eq(v20, v52);
              stdlib.assert(v53);
              const v54 = stdlib.le(0, v45);
              const v55 = stdlib.lt(v45, 3);
              const v56 = v54 ? v55 : false;
              stdlib.assert(v56);
              const v57 = stdlib.le(0, v45);
              const v58 = stdlib.lt(v45, 3);
              const v59 = v57 ? v58 : false;
              const v60 = stdlib.le(0, v37);
              const v61 = stdlib.lt(v37, 3);
              const v62 = v60 ? v61 : false;
              const v63 = v59 ? v62 : false;
              const v64 = stdlib.sub(4, v37);
              const v65 = stdlib.add(v45, v64);
              const v66 = stdlib.mod(v65, 3);
              const v67 = v62 ? 0 : 1;
              const v68 = v59 ? 2 : v67;
              const v69 = v63 ? v66 : v68;
              interact("outcome", (v108) => {
                kTop(v69); }); }); }); }); }); }); }); }

export const ABI = [{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v18","type":"uint256"},{"name":"v19","type":"uint256"},{"name":"v20","type":"uint256"}],"name":"m0","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v18","type":"uint256"},{"name":"v19","type":"uint256"},{"name":"v20","type":"uint256"},{"name":"v37","type":"uint256"},{"name":"v44","type":"uint256"},{"name":"v45","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v18","type":"uint256"},{"name":"v19","type":"uint256"},{"name":"v20","type":"uint256"},{"name":"v37","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"}],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v18","type":"uint256"},{"indexed":false,"name":"v19","type":"uint256"},{"indexed":false,"name":"v20","type":"uint256"}],"name":"e0","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v37","type":"uint256"}],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v44","type":"uint256"},{"indexed":false,"name":"v45","type":"uint256"}],"name":"e2","type":"event"}];

export const Bytecode = "0x60806040526040516107933803806107938339818101604052604081101561002657600080fd5b5080516020918201516040805160008186018190526001600160601b0319606095861b8116838501529390941b909216605483015280516048818403018152606890920190528051920191909120905561070e806100856000396000f3fe6080604052600436106100345760003560e01c8063865ca4e7146100395780639f54e22e1461007d578063b0041610146100d2575b600080fd5b61007b600480360360a081101561004f57600080fd5b506001600160a01b0381358116916020810135909116906040810135906060810135906080013561011a565b005b61007b600480360361010081101561009457600080fd5b506001600160a01b03813581169160208101359091169060408101359060608101359060808101359060a08101359060c08101359060e00135610230565b61007b600480360360c08110156100e857600080fd5b506001600160a01b03813581169160208101359091169060408101359060608101359060808101359060a001356104db565b60408051600060208083018290526001600160601b031960608a811b82168587015289901b1660548401528351604881850301815260689093019093528151919092012090541461016a57600080fd5b336001600160a01b0386161461017f57600080fd5b818301341461018d57600080fd5b604080518481526020810184905280820183905290517fe97939a5c3b22414f653aea9f18c5859841d0af50ba6ede8dd8f1937b8da45389181900360600190a16040805160016020808301919091526001600160601b0319606098891b8116838501529690971b90951660548601526068850193909352608884019190915260a8808401919091528151808403909101815260c890920190528051910120600055565b6040805160026020808301919091526001600160601b031960608c811b8216848601528b901b166054830152606882018990526088820188905260a8820187905260c88083018790528351808403909101815260e890920190925280519101206000541461029d57600080fd5b336001600160a01b038916146102b257600080fd5b34156102bd57600080fd5b60408051602080820185905282518083039091018152818301835260608083018590528351808403909101815260809092019092526102fc919061060a565b6040516020018082805190602001908083835b6020831061032e5780518252601f19909201916020918201910161030f565b6001836020036101000a0380198251168184511680821785525050505050509050019150506040516020818303038152906040528051906020012060001c841461037757600080fd5b6003811061038457600080fd5b600380821090841060008261039a57600061039c565b815b6103c157826103b957816103b15760016103b4565b60005b6103bc565b60025b6103d2565b6003866004038501816103d057fe5b065b90506002811481156001600160a01b038d166108fc836103ff57826103f7578c6103fa565b60005b610404565b8c6002025b8c019081150290604051600060405180830381858888f19350505050158015610431573d6000803e3d6000fd5b508b6001600160a01b03166108fc83610459578261044f578c610454565b8c6002025b61045c565b60005b6040518115909202916000818181858888f19350505050158015610484573d6000803e3d6000fd5b50604080518881526020810188905281517f3a074aaea2d51955da8b38c6383720deafa2c498a47a8b8312010f91a80d49d3929181900390910190a1600080557302b463784bc1a49f1647b47a19452ac420dfc65aff5b6040805160016020808301919091526001600160601b031960608a811b82168486015289901b166054830152606882018790526088820186905260a88083018690528351808403909101815260c890920190925280519101206000541461054157600080fd5b336001600160a01b0386161461055657600080fd5b83341461056257600080fd5b6003811061056f57600080fd5b6040805182815290517f3680e78b6fdf571695c81f108d81181ea63f50c100e6375e765b14bd7ac0adbb9181900360200190a16040805160026020808301919091526001600160601b03196060998a1b8116838501529790981b90961660548701526068860194909452608885019290925260a884015260c8808401919091528151808403909101815260e890920190528051910120600055565b606082518383604051602001808461ffff1661ffff1660f01b815260020183805190602001908083835b602083106106535780518252601f199092019160209182019101610634565b51815160209384036101000a600019018019909216911617905285519190930192850191508083835b6020831061069b5780518252601f19909201916020918201910161067c565b6001836020036101000a038019825116818451168082178552505050505050905001935050505060405160208183030381529060405290509291505056fea265627a7a72305820ea19ca950d35050b7e881c602041167b1a1615d1e5d5e29a086fe7dd739a54c264736f6c634300050a0032";