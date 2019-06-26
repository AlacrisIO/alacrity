// vim: filetype=javascript

import { stdlib } from '../alacrity-runtime.mjs';

describe("BNtoHex", () => {
  it("BNtoHex positive", () => {
    expect(stdlib.BNtoHex(0)).toBe("0000000000000000000000000000000000000000000000000000000000000000");
    expect(stdlib.BNtoHex(1)).toBe("0000000000000000000000000000000000000000000000000000000000000001");
    expect(stdlib.BNtoHex(10)).toBe("000000000000000000000000000000000000000000000000000000000000000a");
    expect(stdlib.BNtoHex(25)).toBe("0000000000000000000000000000000000000000000000000000000000000019");
    expect(stdlib.BNtoHex(30)).toBe("000000000000000000000000000000000000000000000000000000000000001e");
    expect(stdlib.BNtoHex(5463728190))
      .toBe("0000000000000000000000000000000000000000000000000000000145a9e03e");
  });
  it("BNtoHex negative", () => {
    expect(stdlib.BNtoHex(-1)).toBe("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff");
    expect(stdlib.BNtoHex(-10)).toBe("fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff6");
    expect(stdlib.BNtoHex(-30)).toBe("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe2");
    expect(stdlib.BNtoHex(-5463728190))
      .toBe("fffffffffffffffffffffffffffffffffffffffffffffffffffffffeba561fc2");
  });
});
