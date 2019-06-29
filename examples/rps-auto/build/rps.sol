pragma solidity ^0.5.2;

pragma solidity ^0.5.0;

contract Stdlib {

  function ALA_INT_TO_BYTES (uint256 x)
    internal pure returns (bytes memory) {
    return abi.encodePacked(x); }

  function ALA_BCAT (bytes memory l, bytes memory r)
    internal pure returns (bytes memory) {
    return abi.encodePacked(uint16(l.length), l, r); }

  function ALA_BCAT_LEFT_LENGTH (bytes memory c)
    internal pure returns (uint16) {
      require(c.length >= 2);
      uint16 len = uint16(uint8(c[0]))*256+uint16(uint8(c[1])); // TODO: improve using some library on the net
      require(c.length >= 2 + uint(len));
      return len;
  }

  // TODO: optimize using tricks from the Internet? But not before there are tests somehow.
  function ALA_BYTES_SLICE (bytes memory _in, uint16 _start, uint16 _len)
    internal pure returns (bytes memory) {
      require(_in.length >= uint(_start) + uint(_len));
      bytes memory out = new bytes(_len);
      uint16 end = _start + _len;
      for (uint16 i = 0; i < end; i++) {
              out[i] = _in[i];
      }
      return out;
  }

  function ALA_BCAT_LEFT (bytes memory c)
    internal pure returns (bytes memory) {
      return ALA_BYTES_SLICE(c, 2, ALA_BCAT_LEFT_LENGTH(c));
  }

  function ALA_BCAT_RIGHT (bytes memory c)
    internal pure returns (bytes memory) {
      uint16 start = 2 + ALA_BCAT_LEFT_LENGTH(c);
      uint16 len = uint16(c.length) - start;
      return ALA_BYTES_SLICE(c, start, len);
  }
}


contract ALAContract is Stdlib {
  uint256 current_state;
  
  constructor(address payable pA, address payable pB) public payable {
    current_state = uint256(keccak256(abi.encodePacked(uint256(0), pA, pB))); }
  
  event e0(uint256 v17, uint256 v18, uint256 v16);
  function m0(address payable pA, address payable pB, uint256 v17, uint256 v18, uint256 v16) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), pA, pB))));
    require(msg.sender == pA);
    require((msg.value == (v17 + v18)));
    emit e0(v17, v18, v16);
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), pA, pB, v16, v17, v18))); }
  
  event e1(uint256 v23);
  function m1(address payable pA, address payable pB, uint256 v16, uint256 v17, uint256 v18, uint256 v23) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), pA, pB, v16, v17, v18))));
    require(msg.sender == pB);
    require((msg.value == v17));
    require(((uint256(0) <= v23) ? (v23 < uint256(3)) : false));
    emit e1(v23);
    current_state = uint256(keccak256(abi.encodePacked(uint256(2), pA, pB, v16, v17, v18, v23))); }
  
  event e2(uint256 v29, uint256 v30);
  function m2(address payable pA, address payable pB, uint256 v16, uint256 v17, uint256 v18, uint256 v23, uint256 v29, uint256 v30) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(2), pA, pB, v16, v17, v18, v23))));
    require(msg.sender == pA);
    require((msg.value == uint256(0)));
    require((v16 == (uint256(keccak256(abi.encodePacked((ALA_BCAT((abi.encodePacked(v29)), (abi.encodePacked(v30))))))))));
    require(((uint256(0) <= v30) ? (v30 < uint256(3)) : false));
    bool v43 = (uint256(0) <= v30) ? (v30 < uint256(3)) : false;
    bool v46 = (uint256(0) <= v23) ? (v23 < uint256(3)) : false;
    uint256 v53 = (v43 ? v46 : false) ? ((v30 + (uint256(4) - v23)) % uint256(3)) : (v43 ? uint256(2) : (v46 ? uint256(0) : uint256(1)));
    
    
    
    bool v69 = v53 == uint256(2);
    bool v71 = v53 == uint256(0);
    pA.transfer((v18 + (v69 ? (uint256(2) * v17) : (v71 ? uint256(0) : v17))));
    pB.transfer((v69 ? uint256(0) : (v71 ? (uint256(2) * v17) : v17)));
    emit e2(v29, v30);
    current_state = 0x0;
    selfdestruct(address(0x02B463784Bc1a49f1647B47a19452aC420DFC65A)); } }