// vim: filetype=javascript

// NB: the following imports are going away when we fix + finalize the `stdlib`
// parameterization
import Web3            from 'web3';
import * as crypto     from 'crypto';
import * as nodeAssert from 'assert';

const panic = e => { throw Error(e); };

const hexTo0x        = h => '0x' + h;
const byteToHex      = b => (b & 0xFF).toString(16).padStart(2, '0');
const byteArrayToHex = b => Array.from(b, byteToHex).join('');

const isBigInt = x =>
     typeof x           === 'object'
  && typeof x.isInteger === 'function'
  && x.isInteger();


const nat_to_fixed_size_hex = size => n => {
  const err = m => panic(`nat_to_fixed_size_hex: ${m}`);

	const notNat = !(Number.isInteger(n) && 0 <= n);
	const tooBig = !(Math.ceil(Math.log2(n + 1) / 8) <= size);

  return notNat ? err(`expected a nat`)
       : tooBig ? err(`expected a nat that fits into ${size} bytes`)
       : n.toString(16).padStart((2 * size), '0');
};

// Encodes a 16-bit unsigned integer as 2 hex bytes or 4 hex characters
const nat16_to_fixed_size_hex =
  nat_to_fixed_size_hex(2);


// Parameterized ////////////

const assert = a => d => a(d);

const toBN             = web3 =>      web3.toBigNumber;
const hexToBN          = web3 => h => toBN(web3)(hexTo0x(h));
const digestHex        = web3 => x => web3.sha3(x, { encoding: 'hex' });
const keccak256        = web3 => b => digestHex(web3)(hexOf(web3)(b));
const uint256_to_bytes = web3 => i => BNtoHex(web3)(i);

const BNtoHex = web3 => (u, size = 32) => {
  const n = toBN(web3)(u);
  // TODO: if/when we switch to web3 v1.0:
  //       return n.toTwos(8 * size).toString(16, 2 * size);

  if (n.isNegative()) {
    const top   = toBN(web3)(256).pow(size);
    const nTwos = n.modulo(top).add(top);
    return nTwos.toString(16).padStart(2 * size, 'f');
  } else {
    return n.toString(16).padStart(2 * size, '0');
  }
};

// Gets the hex bytes of a number or byte-string, without the 0x prefix
const hexOf = web3 => x =>
    typeof x === 'number'  ? BNtoHex(web3)(toBN(web3)(x))
  : isBigInt(x)            ? BNtoHex(web3)(x)
  : typeof x !== 'string'  ? panic(`Cannot convert to hex: ${x}`)
  : x.slice(0, 2) === '0x' ? x.slice(2)
  : x; // Assume `x` is already in hexadecimal form


const bytes_eq = web3 => (x, y) =>
  hexOf(web3)(x) === hexOf(web3)(y);

// TODO `Number.isInteger` probably isn't sufficient on its own here
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isInteger#Description
const bytes_len = web3 => b => {
  const bh = hexOf(web3)(b);
  const n  = bh.length / 2;

  return !Number.isInteger(n)
    ? panic(`Invalid byte string: ${bh}`)
    : n;
};

// ∀ a b, msg_left (msg_cat(a, b)) = a
// ∀ a b, msg_right(msg_cat(a, b)) = b
const bytes_cat = web3 => (a, b) => {
  const ah = hexOf(web3)(a);
  const bh = hexOf(web3)(b);
  const n  = nat16_to_fixed_size_hex(bytes_len(web3)(ah));

  return n + ah + bh;
};

const random_uint256 = (web3, random32Bytes) => () =>
  hexToBN(web3)(byteArrayToHex(random32Bytes()));


// TODO these should probably just assume BigNumber arguments
const equal = web3 => (a, b) => toBN(web3)(a).eq(toBN(web3)(b));
const add   = web3 => (a, b) => toBN(web3)(a).add(toBN(web3)(b));
const sub   = web3 => (a, b) => toBN(web3)(a).sub(toBN(web3)(b));
const mod   = web3 => (a, b) => toBN(web3)(a).mod(toBN(web3)(b));
const mul   = web3 => (a, b) => toBN(web3)(a).mul(toBN(web3)(b));
const ge    = web3 => (a, b) => toBN(web3)(a).gte(toBN(web3)(b));
const gt    = web3 => (a, b) => toBN(web3)(a).gt(toBN(web3)(b));
const le    = web3 => (a, b) => toBN(web3)(a).lte(toBN(web3)(b));
const lt    = web3 => (a, b) => toBN(web3)(a).lt(toBN(web3)(b));


const mkStdlib = (web3, random32Bytes, asserter) =>
 ({ hexTo0x
  , web3 // TODO remove me
  , random_uint256:   random_uint256(web3, random32Bytes)
  , uint256_to_bytes: uint256_to_bytes(web3)
  , bytes_cat:        bytes_cat(web3)
  , bytes_len:        bytes_len(web3)
  , bytes_eq:         bytes_eq(web3)
  , keccak256:        keccak256(web3)
  , BNtoHex:          BNtoHex(web3)
  , digestHex:        digestHex(web3)
  , assert:           assert(asserter)
  , toBN:             toBN(web3)
  , equal:            equal(web3)
  , eq:               equal(web3)
  , add:              add(web3)
  , sub:              sub(web3)
  , mod:              mod(web3)
  , mul:              mul(web3)
  , ge:               ge(web3)
  , gt:               gt(web3)
  , le:               le(web3)
  , lt:               lt(web3)
  });


const stdlibNode = () =>
  mkStdlib(new Web3(new Web3.providers.HttpProvider('http://localhost:8545'))
         , () => crypto.randomBytes(32)
         , nodeAssert.strict);

// TODO Improve parameterization over node/browser APIs, dependencies like
// `web3`, etc + revisit the ergonomics of how this gets plugged in at run time
export const stdlib = stdlibNode();
