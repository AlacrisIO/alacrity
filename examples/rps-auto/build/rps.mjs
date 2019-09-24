export function A(stdlib, ctc, txn, interact, v0, v1, kTop) {
  const v2 = v0;
  const v3 = v1;
  interact("params", (v4) => {
    const v5 = stdlib.add(v2, v3);
    ctc.sendrecv("A", "m0", [v2, v3], v5, "e0", (txn) => {
      console.log("A sent/recv 0");
      const v6 = txn.value;
      const v7 = stdlib.add(v2, v3);
      const v8 = stdlib.eq(v6, v7);
      stdlib.assert(v8);
      ctc.recv("A", "e1", (txn) => {
        console.log("A receivedAP 1");
        const v10 = txn.value;
        const v11 = stdlib.eq(v10, v2);
        stdlib.assert(v11);
        interact("getHand", (v12) => {
          const v13 = stdlib.bytes_eq(v12, "ROCK");
          const v14 = stdlib.bytes_eq(v12, "PAPER");
          const v15 = stdlib.bytes_eq(v12, "SCISSORS");
          const v16 = v14 ? true : v15;
          const v17 = v13 ? true : v16;
          stdlib.assert(v17);
          const v18 = v14 ? 1 : 2;
          const v19 = v13 ? 0 : v18;
          const v23 = stdlib.random_uint256();
          const v24 = stdlib.uint256_to_bytes(v23);
          const v25 = stdlib.uint256_to_bytes(v19);
          const v26 = stdlib.bytes_cat(v24, v25);
          const v27 = stdlib.keccak256(v26);
          const v28 = v27;
          interact("commits", (v29) => {
            ctc.sendrecv("A", "m2", [v2, v3, v28], 0, "e2", (txn) => {
              console.log("A sent/recv 2");
              const v30 = txn.value;
              const v31 = stdlib.eq(v30, 0);
              stdlib.assert(v31);
              ctc.recv("A", "e3", (v43, txn) => {
                console.log("A receivedAP 3");
                const v45 = txn.value;
                const v46 = stdlib.eq(v45, 0);
                stdlib.assert(v46);
                const v47 = stdlib.le(0, v43);
                const v48 = stdlib.lt(v43, 3);
                const v49 = v47 ? v48 : false;
                stdlib.assert(v49);
                const v50 = v23;
                const v51 = v19;
                interact("reveals", (v52) => {
                  ctc.sendrecv("A", "m4", [v2, v3, v28, v43, v50, v51], 0, "e4", (txn) => {
                    console.log("A sent/recv 4");
                    const v53 = txn.value;
                    const v54 = stdlib.eq(v53, 0);
                    stdlib.assert(v54);
                    const v55 = stdlib.uint256_to_bytes(v50);
                    const v56 = stdlib.uint256_to_bytes(v51);
                    const v57 = stdlib.bytes_cat(v55, v56);
                    const v58 = stdlib.keccak256(v57);
                    const v59 = stdlib.eq(v28, v58);
                    stdlib.assert(v59);
                    const v60 = stdlib.le(0, v51);
                    const v61 = stdlib.lt(v51, 3);
                    const v62 = v60 ? v61 : false;
                    stdlib.assert(v62);
                    const v63 = stdlib.le(0, v51);
                    const v64 = stdlib.lt(v51, 3);
                    const v65 = v63 ? v64 : false;
                    const v66 = stdlib.le(0, v43);
                    const v67 = stdlib.lt(v43, 3);
                    const v68 = v66 ? v67 : false;
                    const v69 = v65 ? v68 : false;
                    const v70 = stdlib.sub(4, v43);
                    const v71 = stdlib.add(v51, v70);
                    const v72 = stdlib.mod(v71, 3);
                    const v73 = v68 ? 0 : 1;
                    const v74 = v65 ? 2 : v73;
                    const v75 = v69 ? v72 : v74;
                    interact("outcome", (v114) => {
                      const v115 = stdlib.le(0, v75);
                      const v116 = stdlib.lt(v75, 3);
                      const v117 = v115 ? v116 : false;
                      stdlib.assert(v117);
                      const v118 = stdlib.eq(v75, 0);
                      const v119 = stdlib.eq(v75, 1);
                      const v120 = v119 ? "Draw" : "Alice wins";
                      const v121 = v118 ? "Bob wins" : v120;
                      kTop(v121); }); }); }); }); }); }); }); }); }); }); }

export function B(stdlib, ctc, txn, interact, kTop) {
  ctc.recv("B", "e0", (v2, v3, txn) => {
    console.log("B receivedAP 0");
    const v6 = txn.value;
    const v7 = stdlib.add(v2, v3);
    const v8 = stdlib.eq(v6, v7);
    stdlib.assert(v8);
    interact("accepts", (v9) => {
      ctc.sendrecv("B", "m1", [v2, v3], v2, "e1", (txn) => {
        console.log("B sent/recv 1");
        const v10 = txn.value;
        const v11 = stdlib.eq(v10, v2);
        stdlib.assert(v11);
        ctc.recv("B", "e2", (v28, txn) => {
          console.log("B receivedAP 2");
          const v30 = txn.value;
          const v31 = stdlib.eq(v30, 0);
          stdlib.assert(v31);
          interact("getHand", (v32) => {
            const v33 = stdlib.bytes_eq(v32, "ROCK");
            const v34 = stdlib.bytes_eq(v32, "PAPER");
            const v35 = stdlib.bytes_eq(v32, "SCISSORS");
            const v36 = v34 ? true : v35;
            const v37 = v33 ? true : v36;
            stdlib.assert(v37);
            const v38 = v34 ? 1 : 2;
            const v39 = v33 ? 0 : v38;
            const v43 = v39;
            interact("shows", (v44) => {
              ctc.sendrecv("B", "m3", [v2, v3, v28, v43], 0, "e3", (txn) => {
                console.log("B sent/recv 3");
                const v45 = txn.value;
                const v46 = stdlib.eq(v45, 0);
                stdlib.assert(v46);
                const v47 = stdlib.le(0, v43);
                const v48 = stdlib.lt(v43, 3);
                const v49 = v47 ? v48 : false;
                stdlib.assert(v49);
                ctc.recv("B", "e4", (v50, v51, txn) => {
                  console.log("B receivedAP 4");
                  const v53 = txn.value;
                  const v54 = stdlib.eq(v53, 0);
                  stdlib.assert(v54);
                  const v55 = stdlib.uint256_to_bytes(v50);
                  const v56 = stdlib.uint256_to_bytes(v51);
                  const v57 = stdlib.bytes_cat(v55, v56);
                  const v58 = stdlib.keccak256(v57);
                  const v59 = stdlib.eq(v28, v58);
                  stdlib.assert(v59);
                  const v60 = stdlib.le(0, v51);
                  const v61 = stdlib.lt(v51, 3);
                  const v62 = v60 ? v61 : false;
                  stdlib.assert(v62);
                  const v63 = stdlib.le(0, v51);
                  const v64 = stdlib.lt(v51, 3);
                  const v65 = v63 ? v64 : false;
                  const v66 = stdlib.le(0, v43);
                  const v67 = stdlib.lt(v43, 3);
                  const v68 = v66 ? v67 : false;
                  const v69 = v65 ? v68 : false;
                  const v70 = stdlib.sub(4, v43);
                  const v71 = stdlib.add(v51, v70);
                  const v72 = stdlib.mod(v71, 3);
                  const v73 = v68 ? 0 : 1;
                  const v74 = v65 ? 2 : v73;
                  const v75 = v69 ? v72 : v74;
                  interact("outcome", (v114) => {
                    const v115 = stdlib.le(0, v75);
                    const v116 = stdlib.lt(v75, 3);
                    const v117 = v115 ? v116 : false;
                    stdlib.assert(v117);
                    const v118 = stdlib.eq(v75, 0);
                    const v119 = stdlib.eq(v75, 1);
                    const v120 = v119 ? "Draw" : "Alice wins";
                    const v121 = v118 ? "Bob wins" : v120;
                    kTop(v121); }); }); }); }); }); }); }); }); }); }

export const ABI = [{"constant":false,"inputs":[{"name":"session","type":"bytes32"},{"name":"clock","type":"uint256"},{"name":"participants","type":"address[]"},{"name":"balancedState","type":"bytes32"},{"name":"newClock","type":"uint256"},{"name":"newBalancedState","type":"bytes32"},{"name":"signatures_v","type":"bytes"},{"name":"signatures_r","type":"bytes32[]"},{"name":"signatures_s","type":"bytes32[]"}],"name":"updateState","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"clock","type":"uint256"},{"name":"participants","type":"address[]"}],"name":"nextClock","outputs":[{"name":"","type":"uint256"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":false,"inputs":[{"name":"session","type":"bytes32"},{"name":"clock","type":"uint256"},{"name":"participants","type":"address[]"},{"name":"data","type":"bytes32"},{"name":"deposit","type":"uint256"},{"name":"withdrawals","type":"uint256[]"},{"name":"newState","type":"bytes32"},{"name":"signatures_v","type":"bytes"},{"name":"signatures_r","type":"bytes32[]"},{"name":"signatures_s","type":"bytes32[]"}],"name":"settle","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"_session","type":"bytes32"},{"name":"_clock","type":"uint256"},{"name":"_participants","type":"address[]"},{"name":"_processor","type":"address"},{"name":"_stateRoot","type":"bytes32"},{"name":"_balances","type":"bytes32"},{"name":"_message","type":"bytes"},{"name":"_evidence","type":"bytes"}],"name":"sendMessage","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"session","type":"bytes32"},{"name":"clock","type":"uint256"},{"name":"_participants","type":"address[]"},{"name":"processState","type":"bytes32"},{"name":"_owned","type":"uint256[]"},{"name":"_collaterals","type":"uint256[]"},{"name":"_failures","type":"uint256[]"},{"name":"_deadlines","type":"uint256[]"},{"name":"failedParticipant","type":"uint256"}],"name":"timeOut","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":true,"inputs":[],"name":"timeoutInBlocks","outputs":[{"name":"timeout","type":"uint256"}],"payable":false,"stateMutability":"pure","type":"function"},{"constant":false,"inputs":[{"name":"session","type":"bytes32"},{"name":"clock","type":"uint256"},{"name":"participants","type":"address[]"},{"name":"data","type":"bytes32"},{"name":"withdrawals","type":"uint256[]"},{"name":"beneficiary","type":"address"},{"name":"signatures_v","type":"bytes"},{"name":"signatures_r","type":"bytes32[]"},{"name":"signatures_s","type":"bytes32[]"}],"name":"close","outputs":[],"payable":true,"stateMutability":"payable","type":"function"},{"constant":false,"inputs":[{"name":"session","type":"bytes32"},{"name":"clock","type":"uint256"},{"name":"participants","type":"address[]"},{"name":"processState","type":"bytes32"},{"name":"owned","type":"uint256[]"},{"name":"collaterals","type":"uint256[]"},{"name":"failures","type":"uint256[]"},{"name":"_deadlines","type":"uint256[]"},{"name":"challengingParticipant","type":"uint256"},{"name":"challengedParticipant","type":"uint256"}],"name":"challenge","outputs":[],"payable":false,"stateMutability":"nonpayable","type":"function"},{"constant":true,"inputs":[{"name":"digest","type":"bytes32"},{"name":"participants","type":"address[]"},{"name":"signatures_v","type":"bytes"},{"name":"signatures_r","type":"bytes32[]"},{"name":"signatures_s","type":"bytes32[]"}],"name":"checkSignatures","outputs":[],"payable":false,"stateMutability":"pure","type":"function"},{"inputs":[{"name":"state","type":"bytes32"}],"payable":true,"stateMutability":"payable","type":"constructor"},{"anonymous":false,"inputs":[{"indexed":false,"name":"","type":"bytes32"}],"name":"Unanimously","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"challengedParticipant","type":"uint256"}],"name":"Challenge","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"clock","type":"uint256"},{"indexed":false,"name":"failedParticipant","type":"uint256"}],"name":"TimeOut","type":"event"},{"anonymous":false,"inputs":[{"indexed":false,"name":"clock","type":"uint256"},{"indexed":false,"name":"message","type":"bytes"}],"name":"Message","type":"event"}];

export const Bytecode = "0x";