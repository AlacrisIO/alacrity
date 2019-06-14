pragma solidity ^0.5.2;


contract ALAFactory {
  function make(address payable pA, address payable pB, uint256 v12, uint256 v13, uint256 v14) public payable returns (ALAContract _ctc) {
    ALAContract ctc = new ALAContract(pA, pB);
    ctc.msg0_m.value(msg.value)(pA, pB, v12, v13, v14);
    return ctc; } }

contract ALAContract {
  uint256 current_state;
  
  constructor(address payable pA, address payable pB) public payable {
    
    current_state = uint256(keccak256( abi.encode(0, pA, pB) )); }
  
  event msg0_evt(uint256 v12, uint256 v13, uint256 v14);
  function msg0_m(address payable pA, address payable pB, uint256 v12, uint256 v13, uint256 v14) external payable {
    require(current_state == uint256(keccak256( abi.encode(0, pA, pB) )));
    require(msg.sender == pA);
    emit msg0_evt(v12, v13, v14);
    current_state = uint256(keccak256( abi.encode(1, pA, pB, v12, v13, v14) )); }
  
  event msg1_evt(uint256 v21);
  function msg1_m(address payable pA, address payable pB, uint256 v12, uint256 v13, uint256 v14, uint256 v21) external payable {
    require(current_state == uint256(keccak256( abi.encode(1, pA, pB, v12, v13, v14) )));
    require(msg.sender == pB);
    bool v22 = true;
    bool v23 = v22 ? false : true;
    bool v24 = v21 == 0;
    bool v25 = v21 == 1;
    bool v26 = v21 == 2;
    bool v27 = v25 ? true : v26;
    bool v28 = v24 ? true : v27;
    bool v29 = v23 ? false : true;
    bool v30 = v29 ? true : v28;
    require(v30);
    emit msg1_evt(v21);
    current_state = uint256(keccak256( abi.encode(2, pA, pB, v12, v13, v14, v21) )); }
  
  event msg2_evt(uint256 v31, uint256 v32);
  function msg2_m(address payable pA, address payable pB, uint256 v12, uint256 v13, uint256 v14, uint256 v21, uint256 v31, uint256 v32) external payable {
    require(current_state == uint256(keccak256( abi.encode(2, pA, pB, v12, v13, v14, v21) )));
    require(msg.sender == pA);
    bytes memory v33 = abi.encode(v31, v32);
    uint256 v34 = uint256(keccak256( abi.encode(v33) ));
    bool v35 = v14 == v34;
    require(v35);
    bool v36 = true;
    bool v37 = v36 ? false : true;
    bool v38 = v32 == 0;
    bool v39 = v32 == 1;
    bool v40 = v32 == 2;
    bool v41 = v39 ? true : v40;
    bool v42 = v38 ? true : v41;
    bool v43 = v37 ? false : true;
    bool v44 = v43 ? true : v42;
    require(v44);
    bool v45 = v32 == 0;
    bool v46 = v32 == 1;
    bool v47 = v32 == 2;
    bool v48 = v46 ? true : v47;
    bool v49 = v45 ? true : v48;
    bool v50 = v21 == 0;
    bool v51 = v21 == 1;
    bool v52 = v21 == 2;
    bool v53 = v51 ? true : v52;
    bool v54 = v50 ? true : v53;
    bool v55 = v49 ? v54 : false;
    uint256 v56 = 4 - v21;
    uint256 v57 = v32 + v56;
    uint256 v58 = v57 % 3;
    uint256 v59 = v54 ? 0 : 1;
    uint256 v60 = v49 ? 2 : v59;
    uint256 v61 = v55 ? v58 : v60;
    bool v62 = v61 == 0;
    bool v63 = v61 == 1;
    bool v64 = v61 == 2;
    bool v65 = v63 ? true : v64;
    bool v66 = v62 ? true : v65;
    require(v66);
    bool v67 = v61 == 2;
    bool v68 = v32 == 0;
    bool v69 = v32 == 1;
    bool v70 = v32 == 2;
    bool v71 = v69 ? true : v70;
    bool v72 = v68 ? true : v71;
    bool v73 = v67 ? false : true;
    bool v74 = v73 ? true : v72;
    require(v74);
    bool v75 = v61 == 0;
    bool v76 = v21 == 0;
    bool v77 = v21 == 1;
    bool v78 = v21 == 2;
    bool v79 = v77 ? true : v78;
    bool v80 = v76 ? true : v79;
    bool v81 = v75 ? false : true;
    bool v82 = v81 ? true : v80;
    require(v82);
    bool v83 = v61 == 2;
    uint256 v84 = 2 * v12;
    uint256 v85 = v84 + v13;
    bool v86 = v61 == 0;
    uint256 v87 = 2 * v12;
    uint256 v88 = v12 + v13;
    uint256 v89 = v86 ? v13 : v88;
    uint256 v90 = v86 ? v87 : v12;
    uint256 v91 = v83 ? v85 : v89;
    uint256 v92 = v83 ? 0 : v90;
    pA.transfer(v91);
    pB.transfer(v92);
    emit msg2_evt(v31, v32);
    current_state = 0x0;
    selfdestruct(address(0x02B463784Bc1a49f1647B47a19452aC420DFC65A)); } }