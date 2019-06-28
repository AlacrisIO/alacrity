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

// TODO shouldn't this throw or abort somehow?
// Encodes an unsigned integer as size hex bytes or (2*size) hex characters
const nat_to_fixed_size_hex = size => n => {
  if (!(Number.isInteger(n) && 0 <= n)) {
    console.error('nat_to_fixed_size_hex: expected a nat');
  }
  if (!(Math.ceil(Math.log2(n + 1) / 8) <= size)) {
    console.error('nat_to_fixed_size_hex: expected a nat that can fit into ' + size.toString() + ' bytes');
  }
  // size bytes = (2*size) hex characters
  return n.toString(16).padStart((2*size), '0');
};
// Encodes a 16-bit unsigned integer as 2 hex bytes or 4 hex characters
const nat16_to_fixed_size_hex = nat_to_fixed_size_hex(2);
// Encodes a 256-bit unsigned integer as 32 hex bytes or 64 hex characters
const nat256_to_fixed_size_hex = nat_to_fixed_size_hex(32);

// Used by both msg_left and msg_right to see when the left stops and the right
// starts - 16 bits = 2 bytes = 4 hex characters
const bytes_left_length = m =>
  parseInt(m.substring(0, 4), 16);

// In our JS representation of byte strings, a byte
// is a pair of hex chars, so the "JS length" is twice
// the "logical length". So we multiply by 2 to convert
// logical length -> JS length
// eslint-disable-next-line no-unused-vars
const bytes_left  = m => m.substring(4, 4 + (2 * bytes_left_length(m)));
// eslint-disable-next-line no-unused-vars
const bytes_right = m => m.substring(4    + (2 * bytes_left_length(m)));


// Parameterized ////////////

const assert = a => d => a(d);

const toBN             = web3 =>      web3.toBigNumber;
const hexToBN          = web3 => h => toBN(web3)(hexTo0x(h));
const digestHex        = web3 => x => web3.sha3(x, { encoding: 'hex' });
const keccak256        = web3 => b => digestHex(web3)(hexOf(web3)(b));
const uint256_to_bytes = web3 => i => BNtoHex(web3)(i);

const BNtoHex = web3 => (u, size = 32) => {
  let n = toBN(web3)(u);
  // TODO: if/when we switch to web3 v1.0:
  //       return n.toTwos(8 * size).toString(16, 2 * size);
  if (n.isNegative()) {
    let top = toBN(web3)(256).pow(size);
    let nTwos = n.modulo(top).add(top);
    return nTwos.toString(16).padStart(2 * size, "f");
  } else {
    return n.toString(16).padStart(2 * size, "0");
  }
};

// Gets the hex bytes of a number or byte-string, without the 0x prefix
const hexOf = web3 => x =>
    typeof x === 'number'  ? BNtoHex(web3)(toBN(web3)(x))
  : isBigInt(x)            ? BNtoHex(web3)(x)
  : typeof x !== 'string'  ? panic(`Cannot convert to hex: ${x}`)
  : x.slice(0, 2) === '0x' ? x.slice(2)
  : x; // Assume `x` is already in hexadecimal form


// eslint-disable-next-line no-unused-vars
const bytes_eq = web3 => (x, y) =>
  hexOf(web3)(x) === hexOf(web3)(y);

// TODO `Number.isInteger` probably isn't sufficient on its own here
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

// --------------------------------------------------------

/** Minimal variants of web3.js's web3.eth.abi functions that only support:
     * uint256
     * bool
     * address
     * bytes
     * ["tuple", type ...]
    because we can only use web3 0.20.x at this time (which is what metamask provides).
    Documentation for encoding:
    https://solidity.readthedocs.io/en/v0.5.9/abi-spec.html?highlight=abi.encode#formal-specification-of-the-encoding
    NB: they are meant as the JS analogues to Solidity's abi.encode.
  */
function typeIsDynamic(type) {
  return (type === "bytes")
         || (Array.isArray(type)
             && (type[0] === "tuple")
             && (type.slice(1).some(typeIsDynamic)))
}
function typeHeadSize(type) {
  if (typeIsDynamic(type)) {
      return 32;
  } else if (type === "uint256" || type === "bool" || type === "address") {
      return 32;
  } else if (Array.isArray(type) && (type[0] === "tuple")) {
      return type.slice(1).map(typeHeadSize).reduce((a,b) => a + b, 0);
  } else {
      console.error("encode: unsupported type");
  }
}
export function encode(type, value) {
  if (type === "uint256") {
      return nat256_to_fixed_size_hex(value);
  } else if (type === "bool") {
      return nat256_to_fixed_size_hex(value ? 1 : 0);
  } else if (type === "address") {
      return nat256_to_fixed_size_hex(value);
  } else if (type === "bytes") {
      // js-length = 2 * logical-length
      let k = value.length / 2;
      let kpad = nextMultiple(k, 32);
      return nat256_to_fixed_size_hex(k) + value.padEnd(2 * kpad, "0");
  } else if (Array.isArray(type) && (type[0] === "tuple")) {
      let types = type.slice(1);
      let k = types.length;
      assert(k === value.length);
      // ptr and heads are mutable!
      let ptr = types.map(typeHeadSize).reduce((a,b) => a + b, 0);
      let heads = [];
      let tails = types.map((t,i) => {
          if (typeIsDynamic(t)) {
              heads[i] = nat256_to_fixed_size_hex(ptr);
              let tail = encode(t, value[i]);
              ptr += (tail.length / 2);
              return tail;
          } else {
              heads[i] = encode(t, value[i]);
              return "";
          }
      });
      return heads.concat(tails).join("");
  } else {
      console.error("encode: unsupported type");
  }
}
export const encodeParameters = (types, parameters) => {
  assert(types.length === parameters.length);
  return encode(["tuple"].concat(types), parameters);
}
export const parametrizedContractCode = (code, types, parameters) =>
  code + encodeParameters(types, parameters)

// --------------------------------------------------------


const mkStdlib = (web3, random32Bytes, asserter) =>
 ({ hexTo0x
  , web3 // TODO remove me
  , random_uint256:   random_uint256(web3, random32Bytes)
  , uint256_to_bytes: uint256_to_bytes(web3)
  , bytes_cat:        bytes_cat(web3)
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
