export function A(stdlib, ctc, txn, interact, v0, v1, kTop) {
  interact("params", (v2) => {
    const v3 = stdlib.add(v0, v1);
    ctc.sendrecv("A", "m0", [v0, v1], v3, "e0", (txn) => {
      const v4 = txn.value;
      const v5 = stdlib.add(v0, v1);
      const v6 = stdlib.eq(v4, v5);
      stdlib.assert(v6);
      ctc.recv("A", "e1", (txn) => {
        const v8 = txn.value;
        const v9 = stdlib.eq(v8, v0);
        stdlib.assert(v9);
        interact("getHand", (v10) => {
          const v11 = stdlib.bytes_eq(v10, "ROCK");
          const v12 = stdlib.bytes_eq(v10, "PAPER");
          const v13 = stdlib.bytes_eq(v10, "SCISSORS");
          const v14 = v12 ? true : v13;
          const v15 = v11 ? true : v14;
          stdlib.assert(v15);
          const v16 = v12 ? 1 : 2;
          const v17 = v11 ? 0 : v16;
          const v21 = stdlib.random_uint256();
          const v22 = stdlib.uint256_to_bytes(v21);
          const v23 = stdlib.uint256_to_bytes(v17);
          const v24 = stdlib.bytes_cat(v22, v23);
          const v25 = stdlib.keccak256(v24);
          interact("commits", (v26) => {
            ctc.sendrecv("A", "m2", [v0, v1, v25], 0, "e2", (txn) => {
              const v27 = txn.value;
              const v28 = stdlib.eq(v27, 0);
              stdlib.assert(v28);
              ctc.recv("A", "e3", (v36, txn) => {
                const v41 = txn.value;
                const v42 = stdlib.eq(v41, 0);
                stdlib.assert(v42);
                const v43 = stdlib.le(0, v36);
                const v44 = stdlib.lt(v36, 3);
                const v45 = v43 ? v44 : false;
                stdlib.assert(v45);
                interact("reveals", (v46) => {
                  ctc.sendrecv("A", "m4", [v0, v1, v25, v36, v21, v17], 0, "e4", (txn) => {
                    const v47 = txn.value;
                    const v48 = stdlib.eq(v47, 0);
                    stdlib.assert(v48);
                    const v49 = stdlib.uint256_to_bytes(v21);
                    const v50 = stdlib.uint256_to_bytes(v17);
                    const v51 = stdlib.bytes_cat(v49, v50);
                    const v52 = stdlib.keccak256(v51);
                    const v53 = stdlib.eq(v25, v52);
                    stdlib.assert(v53);
                    const v54 = stdlib.le(0, v17);
                    const v55 = stdlib.lt(v17, 3);
                    const v56 = v54 ? v55 : false;
                    stdlib.assert(v56);
                    const v57 = stdlib.le(0, v17);
                    const v58 = stdlib.lt(v17, 3);
                    const v59 = v57 ? v58 : false;
                    const v60 = stdlib.le(0, v36);
                    const v61 = stdlib.lt(v36, 3);
                    const v62 = v60 ? v61 : false;
                    const v63 = v59 ? v62 : false;
                    const v64 = stdlib.sub(4, v36);
                    const v65 = stdlib.add(v17, v64);
                    const v66 = stdlib.mod(v65, 3);
                    const v67 = v62 ? 0 : 1;
                    const v68 = v59 ? 2 : v67;
                    const v69 = v63 ? v66 : v68;
                    interact("outcome", (v108) => {
                      const v109 = stdlib.le(0, v69);
                      const v110 = stdlib.lt(v69, 3);
                      const v111 = v109 ? v110 : false;
                      stdlib.assert(v111);
                      const v112 = stdlib.eq(v69, 0);
                      const v113 = stdlib.eq(v69, 1);
                      const v114 = v113 ? "Draw" : "Alice wins";
                      const v115 = v112 ? "Bob wins" : v114;
                      kTop(v115); }); }); }); }); }); }); }); }); }); }); }

export function B(stdlib, ctc, txn, interact, kTop) {
  ctc.recv("B", "e0", (v0, v1, txn) => {
    const v4 = txn.value;
    const v5 = stdlib.add(v0, v1);
    const v6 = stdlib.eq(v4, v5);
    stdlib.assert(v6);
    interact("accepts", (v7) => {
      ctc.sendrecv("B", "m1", [v0, v1], v0, "e1", (txn) => {
        const v8 = txn.value;
        const v9 = stdlib.eq(v8, v0);
        stdlib.assert(v9);
        ctc.recv("B", "e2", (v25, txn) => {
          const v27 = txn.value;
          const v28 = stdlib.eq(v27, 0);
          stdlib.assert(v28);
          interact("getHand", (v29) => {
            const v30 = stdlib.bytes_eq(v29, "ROCK");
            const v31 = stdlib.bytes_eq(v29, "PAPER");
            const v32 = stdlib.bytes_eq(v29, "SCISSORS");
            const v33 = v31 ? true : v32;
            const v34 = v30 ? true : v33;
            stdlib.assert(v34);
            const v35 = v31 ? 1 : 2;
            const v36 = v30 ? 0 : v35;
            interact("shows", (v40) => {
              ctc.sendrecv("B", "m3", [v0, v1, v25, v36], 0, "e3", (txn) => {
                const v41 = txn.value;
                const v42 = stdlib.eq(v41, 0);
                stdlib.assert(v42);
                const v43 = stdlib.le(0, v36);
                const v44 = stdlib.lt(v36, 3);
                const v45 = v43 ? v44 : false;
                stdlib.assert(v45);
                ctc.recv("B", "e4", (v21, v17, txn) => {
                  const v47 = txn.value;
                  const v48 = stdlib.eq(v47, 0);
                  stdlib.assert(v48);
                  const v49 = stdlib.uint256_to_bytes(v21);
                  const v50 = stdlib.uint256_to_bytes(v17);
                  const v51 = stdlib.bytes_cat(v49, v50);
                  const v52 = stdlib.keccak256(v51);
                  const v53 = stdlib.eq(v25, v52);
                  stdlib.assert(v53);
                  const v54 = stdlib.le(0, v17);
                  const v55 = stdlib.lt(v17, 3);
                  const v56 = v54 ? v55 : false;
                  stdlib.assert(v56);
                  const v57 = stdlib.le(0, v17);
                  const v58 = stdlib.lt(v17, 3);
                  const v59 = v57 ? v58 : false;
                  const v60 = stdlib.le(0, v36);
                  const v61 = stdlib.lt(v36, 3);
                  const v62 = v60 ? v61 : false;
                  const v63 = v59 ? v62 : false;
                  const v64 = stdlib.sub(4, v36);
                  const v65 = stdlib.add(v17, v64);
                  const v66 = stdlib.mod(v65, 3);
                  const v67 = v62 ? 0 : 1;
                  const v68 = v59 ? 2 : v67;
                  const v69 = v63 ? v66 : v68;
                  interact("outcome", (v108) => {
                    const v109 = stdlib.le(0, v69);
                    const v110 = stdlib.lt(v69, 3);
                    const v111 = v109 ? v110 : false;
                    stdlib.assert(v111);
                    const v112 = stdlib.eq(v69, 0);
                    const v113 = stdlib.eq(v69, 1);
                    const v114 = v113 ? "Draw" : "Alice wins";
                    const v115 = v112 ? "Bob wins" : v114;
                    kTop(v115); }); }); }); }); }); }); }); }); }); }

export const ABI = [{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v0","type":"uint256"},{"name":"v1","type":"uint256"}],"name":"m1","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v0","type":"uint256"},{"name":"v1","type":"uint256"},{"name":"v25","type":"uint256"},{"name":"v36","type":"uint256"}],"name":"m3","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v0","type":"uint256"},{"name":"v1","type":"uint256"}],"name":"m0","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v0","type":"uint256"},{"name":"v1","type":"uint256"},{"name":"v25","type":"uint256"}],"name":"m2","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"},{"name":"v0","type":"uint256"},{"name":"v1","type":"uint256"},{"name":"v25","type":"uint256"},{"name":"v36","type":"uint256"},{"name":"v21","type":"uint256"},{"name":"v17","type":"uint256"}],"name":"m4","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"inputs":[{"name":"pA","type":"address"},{"name":"pB","type":"address"}],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v0","type":"uint256"},{"indexed":false,"name":"v1","type":"uint256"}],"name":"e0","type":"event"},{"anonymous":false,"inputs":[],"name":"e1","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v25","type":"uint256"}],"name":"e2","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v36","type":"uint256"}],"name":"e3","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"v21","type":"uint256"},{"indexed":false,"name":"v17","type":"uint256"}],"name":"e4","type":"event"}];

export const Bytecode = "0x6080604052604051610a2e380380610a2e8339818101604052604081101561002657600080fd5b5080516020918201516040805160008186018190526001600160601b0319606095861b8116838501529390941b90921660548301528051604881840301815260689092019052805192019190912090556109a9806100856000396000f3fe60806040526004361061004a5760003560e01c8063371ff7ad1461004f578063555806701461008d578063bc5d9a1f146100d5578063de25135014610111578063ef7de38514610153575b600080fd5b61008b6004803603608081101561006557600080fd5b506001600160a01b038135811691602081013590911690604081013590606001356101a8565b005b61008b600480360360c08110156100a357600080fd5b506001600160a01b03813581169160208101359091169060408101359060608101359060808101359060a001356102ac565b61008b600480360360808110156100eb57600080fd5b506001600160a01b038135811691602081013590911690604081013590606001356103da565b61008b600480360360a081101561012757600080fd5b506001600160a01b038135811691602081013590911690604081013590606081013590608001356104e3565b61008b600480360361010081101561016a57600080fd5b506001600160a01b03813581169160208101359091169060408101359060608101359060808101359060a08101359060c08101359060e001356105f8565b6040805160016020808301919091526001600160601b0319606088811b82168486015287901b1660548301526068820185905260888083018590528351808403909101815260a890920190925280519101206000541461020757600080fd5b336001600160a01b0384161461021c57600080fd5b81341461022857600080fd5b6040517fa2c2d6664a47f199eff9c374cfc1c62a769ddc60f66cb047bf673f586c017b2390600090a16040805160026020808301919091526001600160601b0319606097881b8116838501529590961b909416605485015260688401929092526088808401919091528151808403909101815260a890920190528051910120600055565b6040805160036020808301919091526001600160601b031960608a811b82168486015289901b166054830152606882018790526088820186905260a88083018690528351808403909101815260c890920190925280519101206000541461031257600080fd5b336001600160a01b0386161461032757600080fd5b341561033257600080fd5b6003811061033f57600080fd5b6040805182815290517f6fbec89a9bad4c7daaf5b053ac2c5ad4e0ff33c287295fe9a98cf7f3a3043f9c9181900360200190a16040805160046020808301919091526001600160601b03196060998a1b8116838501529790981b90961660548701526068860194909452608885019290925260a884015260c8808401919091528151808403909101815260e890920190528051910120600055565b60408051600060208083018290526001600160601b0319606089811b82168587015288901b1660548401528351604881850301815260689093019093528151919092012090541461042a57600080fd5b336001600160a01b0385161461043f57600080fd5b808201341461044d57600080fd5b604080518381526020810183905281517f8117357c96451e4526279f58f4c30adbb462018bb6b7400f1393e45f7db30b7b929181900390910190a16040805160016020808301919091526001600160601b0319606097881b8116838501529590961b909416605485015260688401929092526088808401919091528151808403909101815260a890920190528051910120600055565b6040805160026020808301919091526001600160601b0319606089811b82168486015288901b1660548301526068820186905260888083018690528351808403909101815260a890920190925280519101206000541461054257600080fd5b336001600160a01b0386161461055757600080fd5b341561056257600080fd5b6040805182815290517ff04f5fc87a72102f7c0b228f8bbaf9b9aa7a2b5dc295c86538fdde91e95866e99181900360200190a16040805160036020808301919091526001600160601b0319606098891b8116838501529690971b90951660548601526068850193909352608884019190915260a8808401919091528151808403909101815260c890920190528051910120600055565b6040805160046020808301919091526001600160601b031960608c811b8216848601528b901b166054830152606882018990526088820188905260a8820187905260c88083018790528351808403909101815260e890920190925280519101206000541461066557600080fd5b336001600160a01b0389161461067a57600080fd5b341561068557600080fd5b60408051602080820185905282518083039091018152818301835260608083018590528351808403909101815260809092019092526106c491906108a5565b6040516020018082805190602001908083835b602083106106f65780518252601f1990920191602091820191016106d7565b6001836020036101000a0380198251168184511680821785525050505050509050019150506040516020818303038152906040528051906020012060001c841461073f57600080fd5b6003811061074c57600080fd5b6003808210908410600082610762576000610764565b815b6107895782610781578161077957600161077c565b60005b610784565b60025b61079a565b60038660040385018161079857fe5b065b905060028114811581816001600160a01b038f166108fc836107c957826107c1578e6107c4565b60005b6107ce565b8e6002025b8e019081150290604051600060405180830381858888f193505050501580156107fb573d6000803e3d6000fd5b508d6001600160a01b03166108fc836108235782610819578e61081e565b8e6002025b610826565b60005b6040518115909202916000818181858888f1935050505015801561084e573d6000803e3d6000fd5b50604080518a8152602081018a905281517fb71d350b59ceca5c6544e5367d61ca8cae3e36b25f8d900743d063dff3d6508b929181900390910190a1600080557302b463784bc1a49f1647b47a19452ac420dfc65aff5b606082518383604051602001808461ffff1661ffff1660f01b815260020183805190602001908083835b602083106108ee5780518252601f1990920191602091820191016108cf565b51815160209384036101000a600019018019909216911617905285519190930192850191508083835b602083106109365780518252601f199092019160209182019101610917565b6001836020036101000a038019825116818451168082178552505050505050905001935050505060405160208183030381529060405290509291505056fea265627a7a723058200317904334a3e01eff87b3a199181c70c0b4054fb8dc1946d5260613fa86f19164736f6c634300050a0032";