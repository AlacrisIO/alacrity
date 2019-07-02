// vim: filetype=javascript

import * as RPS       from '../../build/rps.mjs';
import { stdlibNode } from '../alacrity-runtime.mjs';

const stdlib = stdlibNode(RPS.ABI, RPS.Bytecode);


describe('`BNtoHex` is a standard library primitive that', () => {
  it('correctly translates positive `BigNumber`s to hex', () => {
    expect(stdlib.BNtoHex(0 )).toBe('0000000000000000000000000000000000000000000000000000000000000000');
    expect(stdlib.BNtoHex(1 )).toBe('0000000000000000000000000000000000000000000000000000000000000001');
    expect(stdlib.BNtoHex(10)).toBe('000000000000000000000000000000000000000000000000000000000000000a');
    expect(stdlib.BNtoHex(25)).toBe('0000000000000000000000000000000000000000000000000000000000000019');
    expect(stdlib.BNtoHex(30)).toBe('000000000000000000000000000000000000000000000000000000000000001e');

    expect(stdlib.BNtoHex(5463728190))
      .toBe('0000000000000000000000000000000000000000000000000000000145a9e03e');
  });

  it('correctly translates negative `BigNumber`s to hex', () => {
    expect(stdlib.BNtoHex(-1 )).toBe('ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff');
    expect(stdlib.BNtoHex(-10)).toBe('fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff6');
    expect(stdlib.BNtoHex(-30)).toBe('ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe2');

    expect(stdlib.BNtoHex(-5463728190))
      .toBe('fffffffffffffffffffffffffffffffffffffffffffffffffffffffeba561fc2');
  });
});
