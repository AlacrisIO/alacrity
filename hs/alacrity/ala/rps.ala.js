import * as stdlib from './alacrity-runtime.mjs';

/* XXX export const ABI = Copy the ABI from the solc output; */

/* XXX export const Bytecode = "0x Copy the bytecode from the solc output"; */

export function A(ctc, interact, v0, v1, v2, kTop) {
  const v4 = v2 == 0;
  const v5 = v2 == 1;
  const v6 = v2 == 2;
  const v7 = v5 ? true : v6;
  const v8 = v4 ? true : v7;
  stdlib.assert(v8);
  const v14 = stdlib.random_uint256();
  const v15 = stdlib.uint256_to_bytes(v14);
  const v16 = stdlib.uint256_to_bytes(v2);
  const v17 = stdlib.bytes_cat(v15, v16);
  const v18 = stdlib.keccak256(v17);
  const v19 = v0;
  const v20 = v1;
  const v21 = v18;
  const v22 = v19 + v20;
  ctc.send("m0", [v19, v20, v21], v22, () => {
    ctc.recv("e0", (p19, p20, p21, v23) => {
      stdlib.assert(stdlib.equal(v19, p19));
      stdlib.assert(stdlib.equal(v20, p20));
      stdlib.assert(stdlib.equal(v21, p21));
      const v24 = v19 + v20;
      const v25 = v23 == v24;
      stdlib.assert(v25);
      ctc.recv("e1", (v26, v27) => {
        const v28 = v27 == v19;
        stdlib.assert(v28);
        const v29 = v26 == 0;
        const v30 = v26 == 1;
        const v31 = v26 == 2;
        const v32 = v30 ? true : v31;
        const v33 = v29 ? true : v32;
        stdlib.assert(v33);
        const v34 = v14;
        const v35 = v2;
        ctc.send("m2", [v19, v20, v21, v26, v34, v35], 0, () => {
          ctc.recv("e2", (p34, p35, v36) => {
            stdlib.assert(stdlib.equal(v34, p34));
            stdlib.assert(stdlib.equal(v35, p35));
            const v37 = v36 == 0;
            stdlib.assert(v37);
            const v38 = stdlib.uint256_to_bytes(v34);
            const v39 = stdlib.uint256_to_bytes(v35);
            const v40 = stdlib.bytes_cat(v38, v39);
            const v41 = stdlib.keccak256(v40);
            const v42 = v21 == v41;
            stdlib.assert(v42);
            const v43 = v35 == 0;
            const v44 = v35 == 1;
            const v45 = v35 == 2;
            const v46 = v44 ? true : v45;
            const v47 = v43 ? true : v46;
            stdlib.assert(v47);
            const v48 = v35 == 0;
            const v49 = v35 == 1;
            const v50 = v35 == 2;
            const v51 = v49 ? true : v50;
            const v52 = v48 ? true : v51;
            const v53 = v26 == 0;
            const v54 = v26 == 1;
            const v55 = v26 == 2;
            const v56 = v54 ? true : v55;
            const v57 = v53 ? true : v56;
            const v58 = v52 ? v57 : false;
            const v59 = 4 - v26;
            const v60 = v35 + v59;
            const v61 = v60 % 3;
            const v62 = v57 ? 0 : 1;
            const v63 = v52 ? 2 : v62;
            const v64 = v58 ? v61 : v63;
            const v65 = v64 == 0;
            const v66 = v64 == 1;
            const v67 = v64 == 2;
            const v68 = v66 ? true : v67;
            const v69 = v65 ? true : v68;
            stdlib.assert(v69);
            const v70 = v64 == 2;
            const v71 = v35 == 0;
            const v72 = v35 == 1;
            const v73 = v35 == 2;
            const v74 = v72 ? true : v73;
            const v75 = v71 ? true : v74;
            const v76 = v70 ? false : true;
            const v77 = v76 ? true : v75;
            stdlib.assert(v77);
            const v78 = v64 == 0;
            const v79 = v26 == 0;
            const v80 = v26 == 1;
            const v81 = v26 == 2;
            const v82 = v80 ? true : v81;
            const v83 = v79 ? true : v82;
            const v84 = v78 ? false : true;
            const v85 = v84 ? true : v83;
            stdlib.assert(v85);
            const v86 = v64 == 2;
            const v87 = 2 * v19;
            const v88 = v87 + v20;
            const v89 = v64 == 0;
            const v90 = 2 * v19;
            const v91 = v19 + v20;
            const v92 = v89 ? v20 : v91;
            const v93 = v89 ? v90 : v19;
            const v94 = v86 ? v88 : v92;
            const v95 = v86 ? 0 : v93;
            const v96 = v64 == 2;
            const v97 = v64 == 0;
            kTop(v64); }) }) }) }) }) }

export function B(ctc, interact, v3, kTop) {
  const v9 = v3 == 0;
  const v10 = v3 == 1;
  const v11 = v3 == 2;
  const v12 = v10 ? true : v11;
  const v13 = v9 ? true : v12;
  stdlib.assert(v13);
  ctc.recv("e0", (v19, v20, v21, v23) => {
    const v24 = v19 + v20;
    const v25 = v23 == v24;
    stdlib.assert(v25);
    const v26 = v3;
    ctc.send("m1", [v19, v20, v21, v26], v19, () => {
      ctc.recv("e1", (p26, v27) => {
        stdlib.assert(stdlib.equal(v26, p26));
        const v28 = v27 == v19;
        stdlib.assert(v28);
        const v29 = v26 == 0;
        const v30 = v26 == 1;
        const v31 = v26 == 2;
        const v32 = v30 ? true : v31;
        const v33 = v29 ? true : v32;
        stdlib.assert(v33);
        ctc.recv("e2", (v34, v35, v36) => {
          const v37 = v36 == 0;
          stdlib.assert(v37);
          const v38 = stdlib.uint256_to_bytes(v34);
          const v39 = stdlib.uint256_to_bytes(v35);
          const v40 = stdlib.bytes_cat(v38, v39);
          const v41 = stdlib.keccak256(v40);
          const v42 = v21 == v41;
          stdlib.assert(v42);
          const v43 = v35 == 0;
          const v44 = v35 == 1;
          const v45 = v35 == 2;
          const v46 = v44 ? true : v45;
          const v47 = v43 ? true : v46;
          stdlib.assert(v47);
          const v48 = v35 == 0;
          const v49 = v35 == 1;
          const v50 = v35 == 2;
          const v51 = v49 ? true : v50;
          const v52 = v48 ? true : v51;
          const v53 = v26 == 0;
          const v54 = v26 == 1;
          const v55 = v26 == 2;
          const v56 = v54 ? true : v55;
          const v57 = v53 ? true : v56;
          const v58 = v52 ? v57 : false;
          const v59 = 4 - v26;
          const v60 = v35 + v59;
          const v61 = v60 % 3;
          const v62 = v57 ? 0 : 1;
          const v63 = v52 ? 2 : v62;
          const v64 = v58 ? v61 : v63;
          const v65 = v64 == 0;
          const v66 = v64 == 1;
          const v67 = v64 == 2;
          const v68 = v66 ? true : v67;
          const v69 = v65 ? true : v68;
          stdlib.assert(v69);
          const v70 = v64 == 2;
          const v71 = v35 == 0;
          const v72 = v35 == 1;
          const v73 = v35 == 2;
          const v74 = v72 ? true : v73;
          const v75 = v71 ? true : v74;
          const v76 = v70 ? false : true;
          const v77 = v76 ? true : v75;
          stdlib.assert(v77);
          const v78 = v64 == 0;
          const v79 = v26 == 0;
          const v80 = v26 == 1;
          const v81 = v26 == 2;
          const v82 = v80 ? true : v81;
          const v83 = v79 ? true : v82;
          const v84 = v78 ? false : true;
          const v85 = v84 ? true : v83;
          stdlib.assert(v85);
          const v86 = v64 == 2;
          const v87 = 2 * v19;
          const v88 = v87 + v20;
          const v89 = v64 == 0;
          const v90 = 2 * v19;
          const v91 = v19 + v20;
          const v92 = v89 ? v20 : v91;
          const v93 = v89 ? v90 : v19;
          const v94 = v86 ? v88 : v92;
          const v95 = v86 ? 0 : v93;
          const v96 = v64 == 2;
          const v97 = v64 == 0;
          kTop(v64); }) }) }) }) }