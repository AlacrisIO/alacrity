bytes ALA_INT_TO_BYTES ( int x ) { return abi.encode( x ); }

bytes ALA_BCAT ( bytes l, bytes r ) { return abi.encode( l, r ); }
bytes ALA_BCAT_LEFT ( bytes c ) {
  ( bytes l, bytes r ) = abi.decode( c, (bytes, bytes) );
  return l; }
bytes ALA_BCAT_RIGHT ( bytes c ) {
  ( bytes l, bytes r ) = abi.decode( c, (bytes, bytes) );
  return r; }
