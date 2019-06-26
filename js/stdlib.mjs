import Web3            from 'web3';
import * as crypto     from 'crypto';

const web3 = new Web3(new Web3.providers.HttpProvider('http://localhost:8545'));
const toHex = web3.utils.toHex;
const toBN = web3.utils.toBN;
const random32Bytes = () => crypto.randomBytes(32);

/** : string => String0x */
const hexTo0x = hex => "0x" + hex;

/** : Uint8 => string */
const byteToHex = byte => (byte & 0xFF).toString(16).padStart(2, "0");

/** : Uint8Array => string */
const byteArrayToHex = byteArray => Array.from(byteArray, byteToHex).join('');

// BigNumbers
const isBigInt = x => typeof x === "object" && typeof x.isInteger === "function" && x.isInteger();
const hexToBN = hex => toBN(hexTo0x(hex));
const BNtoHex = (u, size = 32) => {
    let n = toBN(u);
    if (!(Math.ceil(n.bitLength() / 8) <= size)) {
        console.error("BNtoHex: expected a BN that can fit into " + size.toString() + "bytes");
    }
    return n.toTwos(8 * size).toString(16, 2 * size);
}
function nextMultiple(x, n) {
    return Math.ceil(x / n) * n;
}

// Digest
const digestHex = x => Web3.prototype.sha3(x, {encoding: "hex"});

// Gets the hex bytes of a number or byte-string, without the 0x prefix
export const hexOf = x => {
    if (typeof x === "number") {
        return BNtoHex(toBN(x));
    }
    if (isBigInt(x)) {
        return BNtoHex(x);
    }
    if (typeof x !== "string") {
        throw ["Cannot convert to hex:", x];
    }
    if (x.slice(0,2) === "0x") {
        return x.slice(2);
    }
    return x; // NB: Assume x is already hexadecimal.
}

// --------------------------------------------------------

export function equal(x, y) {
    return x == y;
}

export function assert(cond) {
    if (!cond) {
        console.error("assertion failed");
    }
}

export function uint256_to_bytes(i) {
    return BNtoHex(i);
}

export function keccak256(b) {
    return digestHex(hexOf(b));
}

export function bytes_eq(x, y) {
    return hexOf(x) == hexOf(y);
}

export function bytes_len(b) {
    let bh = hexOf(b);
    // In our JS representation of byte strings, a byte
    // is a pair of hex chars, so the "js length" is twice
    // the "logical length". So we divide by 2 to convert
    // js length -> logical length
    let n = bh.length / 2;
    if (!Number.isInteger(n)) {
        console.error("bytes_len: byte-string has an incomplete hex-digit pair");
    }
    return n;
}

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

// Used by both msg_left and msg_right to see when the left stops and the right starts
function bytes_left_length(msg) {
    // 16 bits = 2 bytes = 4 hex characters
    return parseInt(msg.substring(0,4), 16);
}

// ∀ a b, msg_left(msg_cat(a, b)) = a
// ∀ a b, msg_right(msg_cat(a, b)) = b
export function bytes_cat(a, b) {
    let ah = hexOf(a);
    let bh = hexOf(b);
    let n = nat16_to_fixed_size_hex(bytes_len(ah));
    return n + ah + bh;
}

export function bytes_left(msg) {
    let n = bytes_left_length(msg);
    // In our JS representation of byte strings, a byte
    // is a pair of hex chars, so the "js length" is twice
    // the "logical length". So we multiply by 2 to convert
    // logical length -> js length
    return msg.substring(4, 4 + (2 * n));
}
export function bytes_right(msg) {
    let n = bytes_left_length(msg);
    // In our JS representation of byte strings, a byte
    // is a pair of hex chars, so the "js length" is twice
    // the "logical length". So we multiply by 2 to convert
    // logical length -> js length
    return msg.substring(4 + (2 * n));
}


export function random_uint256() {
    return hexToBN(byteArrayToHex(random32Bytes()));
}

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
        return BNtoHex(value);
    } else if (type === "bool") {
        return BNtoHex(value ? 1 : 0);
    } else if (type === "address") {
        return BNtoHex(value);
    } else if (type === "bytes") {
        // js-length = 2 * logical-length
        let k = value.length / 2;
        let kpad = nextMultiple(k, 32);
        return BNtoHex(k) + value.padEnd(2 * kpad, "0");
    } else if (Array.isArray(type) && (type[0] === "tuple")) {
        let types = type.slice(1);
        let k = types.length;
        assert(k === value.length);
        // ptr and heads are mutable!
        let ptr = types.map(typeHeadSize).reduce((a,b) => a + b, 0);
        let heads = [];
        let tails = types.map((t,i) => {
            if (typeIsDynamic(t)) {
                heads[i] = BNtoHex(ptr);
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

export function decode(type, bytes) {
    if (type === "uint256") {
        return parseInt(bytes, 16);
    } else if (type === "bool") {
        return !(parseInt(bytes, 16) === 0);
    } else if (type === "address") {
        return parseInt(bytes, 16);
    } else if (type === "bytes") {
        // 256 bits = 32 bytes = 64 hex characters
        let k = parseInt(bytes.slice(0, 64), 16);
        // js-length = 2 * logical-length
        return bytes.slice(64, 64 + (2*k));
    } else if (Array.isArray(type) && (type[0] === "tuple")) {
        let types = type.slice(1);
        // i is mutable!
        let i = 0;
        return types.map(t => {
            let j = i + typeHeadSize(t);
            // js-length = 2 * logical-length
            let bs = bytes.slice(2*i, 2*j);
            i = j;
            if (typeIsDynamic(t)) {
                // follow the pointer
                let ptr = parseInt(bs, 16);
                return decode(t, bytes.slice(2*ptr))
            } else {
                // get the value directly
                return decode(t, bs);
            }
        });
    } else {
        console.error("encode: unsupported type");
    }
}
export const decodeParameters = (types, bytes) => {
    return decode(["tuple"].concat(types), bytes);
}
