pragma solidity ^0.5.2;

contract ALAContract {
  uint256 current_state;
  
  constructor(address payable pA, address payable pB) public payable {
    current_state = uint256(keccak256(abi.encode(0, pA, pB))); }
  
  event e0(uint256 v19, uint256 v20, uint256 v21);
  function m0(address payable pA, address payable pB, uint256 v19, uint256 v20, uint256 v21) external payable {
    require(current_state == uint256(keccak256(abi.encode(0, pA, pB))));
    require(msg.sender == pA);
    require((msg.value == (v19 + v20)));
    emit e0(v19, v20, v21);
    current_state = uint256(keccak256(abi.encode(1, pA, pB, v19, v20, v21))); }
  
  event e1(uint256 v26);
  function m1(address payable pA, address payable pB, uint256 v19, uint256 v20, uint256 v21, uint256 v26) external payable {
    require(current_state == uint256(keccak256(abi.encode(1, pA, pB, v19, v20, v21))));
    require(msg.sender == pB);
    require((msg.value == v19));
    require(((v26 == 0) ? true : ((v26 == 1) ? true : (v26 == 2))));
    emit e1(v26);
    current_state = uint256(keccak256(abi.encode(2, pA, pB, v19, v20, v21, v26))); }
  
  event e2(uint256 v34, uint256 v35);
  function m2(address payable pA, address payable pB, uint256 v19, uint256 v20, uint256 v21, uint256 v26, uint256 v34, uint256 v35) external payable {
    require(current_state == uint256(keccak256(abi.encode(2, pA, pB, v19, v20, v21, v26))));
    require(msg.sender == pA);
    require((msg.value == 0));
    require((v21 == (uint256(keccak256(abi.encode((abi.encode((abi.encode(v34)), (abi.encode(v35))))))))));
    require(((v35 == 0) ? true : ((v35 == 1) ? true : (v35 == 2))));
    bool v52 = (v35 == 0) ? true : ((v35 == 1) ? true : (v35 == 2));
    bool v57 = (v26 == 0) ? true : ((v26 == 1) ? true : (v26 == 2));
    uint256 v64 = (v52 ? v57 : false) ? ((v35 + (4 - v26)) % 3) : (v52 ? 2 : (v57 ? 0 : 1));
    require(((v64 == 0) ? true : ((v64 == 1) ? true : (v64 == 2))));
    require((((v64 == 2) ? false : true) ? true : ((v35 == 0) ? true : ((v35 == 1) ? true : (v35 == 2)))));
    require((((v64 == 0) ? false : true) ? true : ((v26 == 0) ? true : ((v26 == 1) ? true : (v26 == 2)))));
    bool v86 = v64 == 2;
    bool v89 = v64 == 0;
    pA.transfer((v86 ? ((2 * v19) + v20) : (v89 ? v20 : (v19 + v20))));
    pB.transfer((v86 ? 0 : (v89 ? (2 * v19) : v19)));
    emit e2(v34, v35);
    current_state = 0x0;
    selfdestruct(address(0x02B463784Bc1a49f1647B47a19452aC420DFC65A)); } }