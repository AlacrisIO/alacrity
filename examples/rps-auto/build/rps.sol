pragma solidity ^0.5.2;

import "sol/stdlib.sol";

contract ALAContract is Stdlib {
  uint256 current_state;
  
  constructor(address payable pA, address payable pB) public payable {
    current_state = uint256(keccak256(abi.encodePacked(uint256(0), pA, pB))); }
  
  event e0(uint256 v15, uint256 v16, uint256 v14);
  function m0(address payable pA, address payable pB, uint256 v15, uint256 v16, uint256 v14) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), pA, pB))));
    require(msg.sender == pA);
    require((msg.value == (v15 + v16)));
    emit e0(v15, v16, v14);
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), pA, pB, v14, v15, v16))); }
  
  event e1(uint256 v21);
  function m1(address payable pA, address payable pB, uint256 v14, uint256 v15, uint256 v16, uint256 v21) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), pA, pB, v14, v15, v16))));
    require(msg.sender == pB);
    require((msg.value == v15));
    require(((uint256(0) <= v21) ? (v21 < uint256(3)) : false));
    emit e1(v21);
    current_state = uint256(keccak256(abi.encodePacked(uint256(2), pA, pB, v14, v15, v16, v21))); }
  
  event e2(uint256 v27, uint256 v28);
  function m2(address payable pA, address payable pB, uint256 v14, uint256 v15, uint256 v16, uint256 v21, uint256 v27, uint256 v28) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(2), pA, pB, v14, v15, v16, v21))));
    require(msg.sender == pA);
    require((msg.value == uint256(0)));
    require((v14 == (uint256(keccak256(abi.encodePacked((ALA_BCAT((abi.encodePacked(v27)), (abi.encodePacked(v28))))))))));
    require(((uint256(0) <= v28) ? (v28 < uint256(3)) : false));
    bool v41 = (uint256(0) <= v28) ? (v28 < uint256(3)) : false;
    bool v44 = (uint256(0) <= v21) ? (v21 < uint256(3)) : false;
    uint256 v51 = (v41 ? v44 : false) ? ((v28 + (uint256(4) - v21)) % uint256(3)) : (v41 ? uint256(2) : (v44 ? uint256(0) : uint256(1)));
    require(((uint256(0) <= v51) ? (v51 < uint256(3)) : false));
    require((((v51 == uint256(2)) ? false : true) ? true : ((uint256(0) <= v28) ? (v28 < uint256(3)) : false)));
    require((((v51 == uint256(0)) ? false : true) ? true : ((uint256(0) <= v21) ? (v21 < uint256(3)) : false)));
    bool v67 = v51 == uint256(2);
    bool v70 = v51 == uint256(0);
    pA.transfer((v67 ? ((uint256(2) * v15) + v16) : (v70 ? v16 : (v15 + v16))));
    pB.transfer((v67 ? uint256(0) : (v70 ? (uint256(2) * v15) : v15)));
    emit e2(v27, v28);
    current_state = 0x0;
    selfdestruct(address(0x02B463784Bc1a49f1647B47a19452aC420DFC65A)); } }