// vim: filetype=javascript

import * as RPS       from '../../build/rps.mjs';
import { stdlibNode } from '../alacrity-runtime.mjs';

const { bnToHex
      , toBN
      , isBigNumber
      , bytes_cat
      , eq
      , equal
      , add
      , sub
      , mod
      , mul
      , ge
      , gt
      , le
      , lt
      } = stdlibNode(RPS.ABI, RPS.Bytecode);


describe('`bnToHex` is a standard library function that', () => {
  it('correctly translates positive `BigNumber`s to hex', () => {
    expect(bnToHex(0 )).toBe('0000000000000000000000000000000000000000000000000000000000000000');
    expect(bnToHex(1 )).toBe('0000000000000000000000000000000000000000000000000000000000000001');
    expect(bnToHex(10)).toBe('000000000000000000000000000000000000000000000000000000000000000a');
    expect(bnToHex(25)).toBe('0000000000000000000000000000000000000000000000000000000000000019');
    expect(bnToHex(30)).toBe('000000000000000000000000000000000000000000000000000000000000001e');

    expect(bnToHex(5463728190))
      .toBe('0000000000000000000000000000000000000000000000000000000145a9e03e');
  });

  it('correctly translates negative `BigNumber`s to hex', () => {
    expect(bnToHex(-1 )).toBe('ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff');
    expect(bnToHex(-10)).toBe('fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff6');
    expect(bnToHex(-30)).toBe('ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe2');

    expect(bnToHex(-5463728190))
      .toBe('fffffffffffffffffffffffffffffffffffffffffffffffffffffffeba561fc2');
  });
});


describe('`toBN` is a standard library function that', () => {
  it('correctly translates integer inputs to their `BigNumber` equivalents', () =>
    expect(toBN(500).toString()).toBe('500'));

  it('correctly translates string inputs to their `BigNumber` equivalents', () =>
    expect(toBN('12345.67').toString()).toBe('12345.67'));
});


describe('`isBigNumber` is a standard library function that', () => {
  it('returns `true` for `BigNumber` arguments', () =>
    expect(isBigNumber(toBN('98765.4321'))).toBe(true));

  it('returns `false` for non-`BigNumber` arguments', () => {
    expect(isBigNumber(98765.4321)).toBe(false);
    expect(isBigNumber('98765.43')).toBe(false);
    expect(isBigNumber({        })).toBe(false);
  });
});


describe('The standard library exposes a `BigNumber` comparison function called', () => {
  describe('`eq` (a synonym of `equal`) that', () => {
    it('returns `true` when its arguments match', () =>
      expect(eq(toBN(5678.90), toBN(5678.90))).toBe(true));

    it('returns `false` when provided mismatched arguments', () =>
      expect(eq(toBN(1), toBN(2))).toBe(false));
  });

  describe('`equal` (a synonym of `eq`) that', () => {
    it('returns `true` when its arguments match', () =>
      expect(equal(toBN(5678.90), toBN(5678.90))).toBe(true));

    it('returns `false` when provided mismatched arguments', () =>
      expect(equal(toBN(1), toBN(2))).toBe(false));
  });

  describe('`ge` that', () => {
    it('returns `true` when its first argument is greater than or equal to its second', () => {
      expect(ge(toBN(5), toBN(5))).toBe(true);
      expect(ge(toBN(5), toBN(4))).toBe(true);
    });

    it('returns `false` when its first argument is less than its second', () =>
      expect(ge(toBN(5), toBN(6))).toBe(false));
  });

  describe('`gt` that', () => {
    it('returns `true` when its first argument is greater than its second', () =>
      expect(gt(toBN(5), toBN(4))).toBe(true));

    it('returns `false` when its first argument is equal to or less than its second', () => {
      expect(gt(toBN(5), toBN(5))).toBe(false);
      expect(gt(toBN(5), toBN(6))).toBe(false);
    });
  });

  describe('`le` that', () => {
    it('returns `true` when its first argument is lesser than or equal to its second', () => {
      expect(le(toBN(5), toBN(5))).toBe(true);
      expect(le(toBN(4), toBN(5))).toBe(true);
    });

    it('returns `false` when its first argument is greater than its second', () =>
      expect(le(toBN(6), toBN(5))).toBe(false));
  });

  describe('`lt` that', () => {
    it('returns `true` when its first argument is lesser than its second', () =>
      expect(lt(toBN(4), toBN(5))).toBe(true));

    it('returns `false` when its first argument is equal to or greater than its second', () => {
      expect(lt(toBN(5), toBN(5))).toBe(false);
      expect(lt(toBN(6), toBN(5))).toBe(false);
    });
  });
});


describe('The standard library exposes a `BigNumber` arithmetic function called', () => {
  it('`add` that sums its arguments', () =>
    expect(add(toBN(12), toBN(1)).eq(toBN(13))).toBe(true));

  it('`sub` that subtracts its second argument from its first', () =>
    expect(sub(toBN(12), toBN(1)).eq(toBN(11))).toBe(true));

  it('`mod` that returns the remainder of its first argument divided by its second', () =>
    expect(mod(toBN(10), toBN(4)).eq(toBN(2))).toBe(true));

  it('`mul` that returns the product of its arguments', () =>
    expect(mul(toBN(3), toBN(5)).eq(toBN(15))).toBe(true));
});


describe('The standard library exposes a `bytes_cat` function that', () => {
  it('concatenates its arguments in hex form and prefixes with length of the first', () => {
    const a = 'cddcbafdec2cfaf2c9b716fe3f4f09ef59874fc4201fffe3bf1382a5bbe8c010';
    const b = '0000000000000000000000000000000000000000000000000000000000000000';

    expect(bytes_cat(a, b)).toBe(`0020${a}${b}`);
  });
});
