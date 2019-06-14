pragma solidity ^0.5.2;


contract ALAFactory {
  function make(address payable pA, address payable pB, uint256 v13, uint256 v14, uint256 v15) public payable returns (ALAContract _ctc) {
    ALAContract ctc = new ALAContract(pA, pB);
    ctc.msg0_m.value(msg.value)(pA, pB, v13, v14, v15);
    return ctc; } }

contract ALAContract {
  uint256 current_state;
  
  constructor(address payable pA, address payable pB) public payable {
    
    current_state = uint256(keccak256( abi.encode(0, pA, pB) )); }
  
  event msg0_evt(uint256 v13, uint256 v14, uint256 v15);
  function msg0_m(address payable pA, address payable pB, uint256 v13, uint256 v14, uint256 v15) external payable {
    require(current_state == uint256(keccak256( abi.encode(0, pA, pB) )));
    require(msg.sender == pA);
    emit msg0_evt(v13, v14, v15);
    current_state = uint256(keccak256( abi.encode(1, pA, pB, v13, v14, v15) )); }
  
  event msg1_evt(uint256 v23);
  function msg1_m(address payable pA, address payable pB, uint256 v13, uint256 v14, uint256 v15, uint256 v23) external payable {
    require(current_state == uint256(keccak256( abi.encode(1, pA, pB, v13, v14, v15) )));
    require(msg.sender == pB);
    bool v24 = true;
    bool v25 = v24 ? false : true;
    bool v26 = v23 == 0;
    bool v27 = v23 == 1;
    bool v28 = v23 == 2;
    bool v29 = v27 ? true : v28;
    bool v30 = v26 ? true : v29;
    bool v31 = v25 ? false : true;
    bool v32 = v31 ? true : v30;
    require(v32);
    emit msg1_evt(v23);
    current_state = uint256(keccak256( abi.encode(2, pA, pB, v13, v14, v15, v23) )); }
  
  event msg2_evt(uint256 v34, uint256 v35);
  function msg2_m(address payable pA, address payable pB, uint256 v13, uint256 v14, uint256 v15, uint256 v23, uint256 v34, uint256 v35) external payable {
    require(current_state == uint256(keccak256( abi.encode(2, pA, pB, v13, v14, v15, v23) )));
    require(msg.sender == pA);
    bytes memory v36 = abi.encode(v34, v35);
    uint256 v37 = uint256(keccak256( abi.encode(v36) ));
    bool v38 = v15 == v37;
    require(v38);
    bool v40 = true;
    bool v41 = v40 ? false : true;
    bool v42 = v35 == 0;
    bool v43 = v35 == 1;
    bool v44 = v35 == 2;
    bool v45 = v43 ? true : v44;
    bool v46 = v42 ? true : v45;
    bool v47 = v41 ? false : true;
    bool v48 = v47 ? true : v46;
    require(v48);
    bool v50 = v35 == 0;
    bool v51 = v35 == 1;
    bool v52 = v35 == 2;
    bool v53 = v51 ? true : v52;
    bool v54 = v50 ? true : v53;
    bool v55 = v23 == 0;
    bool v56 = v23 == 1;
    bool v57 = v23 == 2;
    bool v58 = v56 ? true : v57;
    bool v59 = v55 ? true : v58;
    bool v60 = v54 ? v59 : false;
    uint256 v61 = 4 - v23;
    uint256 v62 = v35 + v61;
    uint256 v63 = v62 % 3;
    uint256 v64 = v59 ? 0 : 1;
    uint256 v65 = v54 ? 2 : v64;
    uint256 v66 = v60 ? v63 : v65;
    bool v67 = v66 == 0;
    bool v68 = v66 == 1;
    bool v69 = v66 == 2;
    bool v70 = v68 ? true : v69;
    bool v71 = v67 ? true : v70;
    require(v71);
    bool v73 = v66 == 2;
    bool v74 = v35 == 0;
    bool v75 = v35 == 1;
    bool v76 = v35 == 2;
    bool v77 = v75 ? true : v76;
    bool v78 = v74 ? true : v77;
    bool v79 = v73 ? false : true;
    bool v80 = v79 ? true : v78;
    require(v80);
    bool v82 = v66 == 0;
    bool v83 = v23 == 0;
    bool v84 = v23 == 1;
    bool v85 = v23 == 2;
    bool v86 = v84 ? true : v85;
    bool v87 = v83 ? true : v86;
    bool v88 = v82 ? false : true;
    bool v89 = v88 ? true : v87;
    require(v89);
    bool v91 = v66 == 2;
    uint256 v92 = 2 * v13;
    uint256 v93 = v92 + v14;
    bool v94 = v66 == 0;
    uint256 v95 = 2 * v13;
    uint256 v96 = v13 + v14;
    uint256 v97 = v94 ? v14 : v96;
    uint256 v98 = v94 ? v95 : v13;
    uint256 v99 = v91 ? v93 : v97;
    uint256 v100 = v91 ? 0 : v98;
    pA.transfer(v99);
    pB.transfer(v100);
    emit msg2_evt(v34, v35);
    current_state = 0x0;
    selfdestruct(address(0x02B463784Bc1a49f1647B47a19452aC420DFC65A)); } }