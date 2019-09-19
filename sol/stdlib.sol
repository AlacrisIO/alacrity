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

  function get_block_number() internal view returns(int) {
    int nbr;
    assembly {
      let y := number
      nbr := y
    }
    return nbr;
  }
}
