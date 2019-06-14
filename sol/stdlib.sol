pragma solidity ^0.5.0;

contract Stdlib {

  function ALA_INT_TO_BYTES (uint256 x)
    public pure returns (bytes memory) {
    return abi.encode(x); }

  function ALA_BCAT (bytes memory l, bytes memory r)
    public pure returns (bytes memory) {
    // XXX include l length
    return abi.encode(l, r); }

  function ALA_BCAT_LEFT (bytes memory c)
    public pure returns (bytes memory) {
    // XXX use l length
    (bytes memory l, bytes memory r ) = abi.decode( c, (bytes, bytes) );
    return l; }

  function ALA_BCAT_RIGHT (bytes memory c)
    public pure returns (bytes memory) {
    // XXX use l length
    (bytes memory l, bytes memory r ) = abi.decode( c, (bytes, bytes) );
    return r; }

}
