pragma solidity ^0.5.2;

contract ALAContract {
  uint256 current_state;
  
  constructor(address payable pA, address payable pB) public payable {
    current_state = uint256(keccak256(abi.encode(0, pA, pB))); }
  
  event e0(uint256 v19, uint256 v20, uint256 v21);
  function m0(address payable pA, address payable pB, uint256 v19, uint256 v20, uint256 v21) external payable {
    require(current_state == uint256(keccak256(abi.encode(0, pA, pB))));
    require(msg.sender == pA);
    emit e0(v19, v20, v21);
    current_state = uint256(keccak256(abi.encode(1, pA, pB, v19, v20, v21))); }
  
  event e1(uint256 v23);
  function m1(address payable pA, address payable pB, uint256 v19, uint256 v20, uint256 v21, uint256 v23) external payable {
    require(current_state == uint256(keccak256(abi.encode(1, pA, pB, v19, v20, v21))));
    require(msg.sender == pB);
    require(((v23 == 0) ? true : ((v23 == 1) ? true : (v23 == 2))));
    emit e1(v23);
    current_state = uint256(keccak256(abi.encode(2, pA, pB, v19, v20, v21, v23))); }
  
  event e2(uint256 v29, uint256 v30);
  function m2(address payable pA, address payable pB, uint256 v19, uint256 v20, uint256 v21, uint256 v23, uint256 v29, uint256 v30) external payable {
    require(current_state == uint256(keccak256(abi.encode(2, pA, pB, v19, v20, v21, v23))));
    require(msg.sender == pA);
    require((v21 == (uint256(keccak256(abi.encode((abi.encode((abi.encode(v29)), (abi.encode(v30))))))))));
    require(((v30 == 0) ? true : ((v30 == 1) ? true : (v30 == 2))));
    bool v45 = (v30 == 0) ? true : ((v30 == 1) ? true : (v30 == 2));
    bool v50 = (v23 == 0) ? true : ((v23 == 1) ? true : (v23 == 2));
    uint256 v57 = (v45 ? v50 : false) ? ((v30 + (4 - v23)) % 3) : (v45 ? 2 : (v50 ? 0 : 1));
    require(((v57 == 0) ? true : ((v57 == 1) ? true : (v57 == 2))));
    require((((v57 == 2) ? false : true) ? true : ((v30 == 0) ? true : ((v30 == 1) ? true : (v30 == 2)))));
    require((((v57 == 0) ? false : true) ? true : ((v23 == 0) ? true : ((v23 == 1) ? true : (v23 == 2)))));
    bool v79 = v57 == 2;
    bool v82 = v57 == 0;
    pA.transfer((v79 ? ((2 * v19) + v20) : (v82 ? v20 : (v19 + v20))));
    pB.transfer((v79 ? 0 : (v82 ? (2 * v19) : v19)));
    emit e2(v29, v30);
    current_state = 0x0;
    selfdestruct(address(0x02B463784Bc1a49f1647B47a19452aC420DFC65A)); } }