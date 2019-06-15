pragma solidity ^0.5.2;

contract ALAFactory {
  function make(address payable pA, address payable pB, uint256 v12, uint256 v13, uint256 v14) public payable returns (ALAContract _ctc) {
    ALAContract ctc = new ALAContract(pA, pB);
    ctc.msg0_m.value(msg.value)(pA, pB, v12, v13, v14);
    return ctc; } }

contract ALAContract {
  uint256 current_state;
  
  constructor(address payable pA, address payable pB) public payable {
    current_state = uint256(keccak256(abi.encode(0, pA, pB))); }
  
  event msg0_evt(uint256 v12, uint256 v13, uint256 v14);
  function msg0_m(address payable pA, address payable pB, uint256 v12, uint256 v13, uint256 v14) external payable {
    require(current_state == uint256(keccak256(abi.encode(0, pA, pB))));
    require(msg.sender == pA);
    emit msg0_evt(v12, v13, v14);
    current_state = uint256(keccak256(abi.encode(1, pA, pB, v12, v13, v14))); }
  
  event msg1_evt(uint256 v21);
  function msg1_m(address payable pA, address payable pB, uint256 v12, uint256 v13, uint256 v14, uint256 v21) external payable {
    require(current_state == uint256(keccak256(abi.encode(1, pA, pB, v12, v13, v14))));
    require(msg.sender == pB);
    require(((((true) ? false : true) ? false : true) ? true : ((v21 == 0) ? true : ((v21 == 1) ? true : (v21 == 2)))));
    emit msg1_evt(v21);
    current_state = uint256(keccak256(abi.encode(2, pA, pB, v12, v13, v14, v21))); }
  
  event msg2_evt(uint256 v31, uint256 v32);
  function msg2_m(address payable pA, address payable pB, uint256 v12, uint256 v13, uint256 v14, uint256 v21, uint256 v31, uint256 v32) external payable {
    require(current_state == uint256(keccak256(abi.encode(2, pA, pB, v12, v13, v14, v21))));
    require(msg.sender == pA);
    require((v14 == (uint256(keccak256(abi.encode((abi.encode(v31, v32))))))));
    require(((((true) ? false : true) ? false : true) ? true : ((v32 == 0) ? true : ((v32 == 1) ? true : (v32 == 2)))));
    bool v49 = (v32 == 0) ? true : ((v32 == 1) ? true : (v32 == 2));
    bool v54 = (v21 == 0) ? true : ((v21 == 1) ? true : (v21 == 2));
    uint256 v61 = (v49 ? v54 : false) ? ((v32 + (4 - v21)) % 3) : (v49 ? 2 : (v54 ? 0 : 1));
    require(((v61 == 0) ? true : ((v61 == 1) ? true : (v61 == 2))));
    require((((v61 == 2) ? false : true) ? true : ((v32 == 0) ? true : ((v32 == 1) ? true : (v32 == 2)))));
    require((((v61 == 0) ? false : true) ? true : ((v21 == 0) ? true : ((v21 == 1) ? true : (v21 == 2)))));
    bool v83 = v61 == 2;
    bool v86 = v61 == 0;
    pA.transfer((v83 ? ((2 * v12) + v13) : (v86 ? v13 : (v12 + v13))));
    pB.transfer((v83 ? 0 : (v86 ? (2 * v12) : v12)));
    emit msg2_evt(v31, v32);
    current_state = 0x0;
    selfdestruct(address(0x02B463784Bc1a49f1647B47a19452aC420DFC65A)); } }