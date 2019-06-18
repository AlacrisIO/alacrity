export function assert(cond) { XXX; }
export function int2bytes(i) { XXX; }
export function keccak256(b) { XXX; }

// Used by msg_cat to encode the left_length 16-bit unsigned integer
// as 2 hex bytes or 4 hex characters
function nat16_to_fixed_size_hex(n) {
    // 2^16 = 1 << 16
    if (!(Number.isInteger(n) && 0 <= n && n < (1 << 16))) {
        console.error("nat16_to_fixed_size_hex: expected a nat that can fit into 16 bits");
    }
    // 16 bits = 2 bytes = 4 hex characters
    return n.toString(16).padStart(4, "0");
}

// Used by both msg_left and msg_right to see when the left stops and the right starts
function msg_left_length(msg) {
    // 16 bits = 2 bytes = 4 hex characters
    return parseInt(msg.substring(0,4), 16);
}

// ∀ a b, msg_left(msg_cat(a, b)) = a
// ∀ a b, msg_right(msg_cat(a, b)) = b
export function msg_cat(a, b) {
    let ah = hexOf(a);
    let bh = hexOf(b);
    // In our JS representation of byte strings, a byte
    // is a pair of hex chars, so the "js length" is twice
    // the "logical length". So we divide by 2 to convert
    // js length -> logical length
    let n = nat16_to_fixed_size_hex(ah.length / 2);
    return n + ah + bh;
}

export function msg_left(msg) {
    let n = msg_left_length(msg);
    // In our JS representation of byte strings, a byte
    // is a pair of hex chars, so the "js length" is twice
    // the "logical length". So we multiply by 2 to convert
    // logical length -> js length
    return msg.substring(4, 4 + (2 * n));
}
export function msg_right(msg) {
    let n = msg_left_length(msg);
    // In our JS representation of byte strings, a byte
    // is a pair of hex chars, so the "js length" is twice
    // the "logical length". So we multiply by 2 to convert
    // logical length -> js length
    return msg.substring(4 + (2 * n));
}


export function random() { XXX; }
