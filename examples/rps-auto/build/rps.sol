pragma solidity ^0.5.2;


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
  
  event e0(uint256 v0, uint256 v1);
  function m0(address payable pA, address payable pB, uint256 v0, uint256 v1) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(0), pA, pB))));
    require(msg.sender == pA);
    require(((msg.value) == (v0 + v1)));
    emit e0(v0, v1);
    current_state = uint256(keccak256(abi.encodePacked(uint256(1), pA, pB, v0, v1))); }
  
  event e1();
  function m1(address payable pA, address payable pB, uint256 v0, uint256 v1) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(1), pA, pB, v0, v1))));
    require(msg.sender == pB);
    require(((msg.value) == v0));
    emit e1();
    current_state = uint256(keccak256(abi.encodePacked(uint256(2), pA, pB, v0, v1))); }
  
  event e2(uint256 v25);
  function m2(address payable pA, address payable pB, uint256 v0, uint256 v1, uint256 v25) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(2), pA, pB, v0, v1))));
    require(msg.sender == pA);
    require(((msg.value) == uint256(0)));
    emit e2(v25);
    current_state = uint256(keccak256(abi.encodePacked(uint256(3), pA, pB, v0, v1, v25))); }
  
  event e3(uint256 v36);
  function m3(address payable pA, address payable pB, uint256 v0, uint256 v1, uint256 v25, uint256 v36) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(3), pA, pB, v0, v1, v25))));
    require(msg.sender == pB);
    require(((msg.value) == uint256(0)));
    require(((uint256(0) <= v36) ? (v36 < uint256(3)) : false));
    emit e3(v36);
    current_state = uint256(keccak256(abi.encodePacked(uint256(4), pA, pB, v0, v1, v25, v36))); }
  
  event e4(uint256 v21, uint256 v17);
  function m4(address payable pA, address payable pB, uint256 v0, uint256 v1, uint256 v25, uint256 v36, uint256 v21, uint256 v17) external payable {
    require(current_state == uint256(keccak256(abi.encodePacked(uint256(4), pA, pB, v0, v1, v25, v36))));
    require(msg.sender == pA);
    require(((msg.value) == uint256(0)));
    require((v25 == (uint256(keccak256(abi.encodePacked((ALA_BCAT((abi.encodePacked(v21)), (abi.encodePacked(v17))))))))));
    require(((uint256(0) <= v17) ? (v17 < uint256(3)) : false));
    bool v59 = (uint256(0) <= v17) ? (v17 < uint256(3)) : false;
    bool v62 = (uint256(0) <= v36) ? (v36 < uint256(3)) : false;
    uint256 v69 = (v59 ? v62 : false) ? ((v17 + (uint256(4) - v36)) % uint256(3)) : (v59 ? uint256(2) : (v62 ? uint256(0) : uint256(1)));
    bool v85 = v69 == uint256(2);
    bool v92 = v69 == uint256(0);
    bool v99 = v69 == uint256(2);
    bool v101 = v69 == uint256(0);
    pA.transfer((v1 + (v99 ? (uint256(2) * v0) : (v101 ? uint256(0) : v0))));
    pB.transfer((v99 ? uint256(0) : (v101 ? (uint256(2) * v0) : v0)));
    emit e4(v21, v17);
    current_state = 0x0;
    selfdestruct(address(0x02B463784Bc1a49f1647B47a19452aC420DFC65A)); } }