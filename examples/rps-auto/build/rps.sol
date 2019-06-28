pragma solidity ^0.5.2;

import "sol/stdlib.sol";

contract ALAContract is Stdlib {
  uint256 current_state;
  
  constructor(address payable pA, address payable pB) public payable {
    current_state = uint256(keccak256(abi.encodePacked(uint256(0), pA, pB))); }
  
  event e0(uint256 v15, uint256 v16, uint256 v17);
  function m0(address payable pA, address payable pB, uint256 v15, uint256 v16, uint256 v17) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), pA, pB))));
    require(msg.sender == pA);
    require((msg.value == (v15 + v16)));
    emit e0(v15, v16, v17);
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), pA, pB, v15, v16, v17))); }
  
  event e1(uint256 v22);
  function m1(address payable pA, address payable pB, uint256 v15, uint256 v16, uint256 v17, uint256 v22) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), pA, pB, v15, v16, v17))));
    require(msg.sender == pB);
    require((msg.value == v15));
    require(((uint256(0) <= v22) ? (v22 < uint256(3)) : false));
    emit e1(v22);
    current_state = uint256(keccak256(abi.encodePacked(uint256(2), pA, pB, v15, v16, v17, v22))); }
  
  event e2(uint256 v28, uint256 v29);
  function m2(address payable pA, address payable pB, uint256 v15, uint256 v16, uint256 v17, uint256 v22, uint256 v28, uint256 v29) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(2), pA, pB, v15, v16, v17, v22))));
    require(msg.sender == pA);
    require((msg.value == uint256(0)));
    require((v17 == (uint256(keccak256(abi.encodePacked((ALA_BCAT((abi.encodePacked(v28)), (abi.encodePacked(v29))))))))));
    require(((uint256(0) <= v29) ? (v29 < uint256(3)) : false));
    bool v42 = (uint256(0) <= v29) ? (v29 < uint256(3)) : false;
    bool v45 = (uint256(0) <= v22) ? (v22 < uint256(3)) : false;
    uint256 v52 = (v42 ? v45 : false) ? ((v29 + (uint256(4) - v22)) % uint256(3)) : (v42 ? uint256(2) : (v45 ? uint256(0) : uint256(1)));
    require(((uint256(0) <= v52) ? (v52 < uint256(3)) : false));
    require((((v52 == uint256(2)) ? false : true) ? true : ((uint256(0) <= v29) ? (v29 < uint256(3)) : false)));
    require((((v52 == uint256(0)) ? false : true) ? true : ((uint256(0) <= v22) ? (v22 < uint256(3)) : false)));
    bool v68 = v52 == uint256(2);
    bool v71 = v52 == uint256(0);
    pA.transfer((v68 ? ((uint256(2) * v15) + v16) : (v71 ? v16 : (v15 + v16))));
    pB.transfer((v68 ? uint256(0) : (v71 ? (uint256(2) * v15) : v15)));
    emit e2(v28, v29);
    current_state = 0x0;
    selfdestruct(address(0x02B463784Bc1a49f1647B47a19452aC420DFC65A)); } }