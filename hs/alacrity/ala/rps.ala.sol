pragma solidity ^0.5.2;

contract ALAFactory {
  function make(address payable pA, address payable pB, uint256 v14, uint256 v15, uint256 v16) public payable returns (ALAContract _ctc) {
    ALAContract ctc = new ALAContract(pA, pB);
    ctc.msg0_m.value(msg.value)(pA, pB, v14, v15, v16);
    return ctc; } }

contract ALAContract {
  uint256 current_state;
  
  constructor(address payable pA, address payable pB) public payable {
    current_state = uint256(keccak256(abi.encode(0, pA, pB))); }
  
  event e0(uint256 v14, uint256 v15, uint256 v16);
  function m0(address payable pA, address payable pB, uint256 v14, uint256 v15, uint256 v16) external payable {
    require(current_state == uint256(keccak256(abi.encode(0, pA, pB))));
    require(msg.sender == pA);
    emit e0(v14, v15, v16);
    current_state = uint256(keccak256(abi.encode(1, pA, pB, v14, v15, v16))); }
  
  event e1(uint256 v23);
  function m1(address payable pA, address payable pB, uint256 v14, uint256 v15, uint256 v16, uint256 v23) external payable {
    require(current_state == uint256(keccak256(abi.encode(1, pA, pB, v14, v15, v16))));
    require(msg.sender == pB);
    require(((((true) ? false : true) ? false : true) ? true : ((v23 == 0) ? true : ((v23 == 1) ? true : (v23 == 2)))));
    emit e1(v23);
    current_state = uint256(keccak256(abi.encode(2, pA, pB, v14, v15, v16, v23))); }
  
  event e2(uint256 v33, uint256 v34);
  function m2(address payable pA, address payable pB, uint256 v14, uint256 v15, uint256 v16, uint256 v23, uint256 v33, uint256 v34) external payable {
    require(current_state == uint256(keccak256(abi.encode(2, pA, pB, v14, v15, v16, v23))));
    require(msg.sender == pA);
    require((v16 == (uint256(keccak256(abi.encode((abi.encode((abi.encode(v33)), (abi.encode(v34))))))))));
    require(((((true) ? false : true) ? false : true) ? true : ((v34 == 0) ? true : ((v34 == 1) ? true : (v34 == 2)))));
    bool v53 = (v34 == 0) ? true : ((v34 == 1) ? true : (v34 == 2));
    bool v58 = (v23 == 0) ? true : ((v23 == 1) ? true : (v23 == 2));
    uint256 v65 = (v53 ? v58 : false) ? ((v34 + (4 - v23)) % 3) : (v53 ? 2 : (v58 ? 0 : 1));
    require(((v65 == 0) ? true : ((v65 == 1) ? true : (v65 == 2))));
    require((((v65 == 2) ? false : true) ? true : ((v34 == 0) ? true : ((v34 == 1) ? true : (v34 == 2)))));
    require((((v65 == 0) ? false : true) ? true : ((v23 == 0) ? true : ((v23 == 1) ? true : (v23 == 2)))));
    bool v87 = v65 == 2;
    bool v90 = v65 == 0;
    pA.transfer((v87 ? ((2 * v14) + v15) : (v90 ? v15 : (v14 + v15))));
    pB.transfer((v87 ? 0 : (v90 ? (2 * v14) : v14)));
    emit e2(v33, v34);
    current_state = 0x0;
    selfdestruct(address(0x02B463784Bc1a49f1647B47a19452aC420DFC65A)); } }