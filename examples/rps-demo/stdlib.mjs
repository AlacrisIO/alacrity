import {throw_if_false, bytesTo0x, hexToInt, intToHex} from "./common-utils.mjs"
import {random32Bytes} from "./web3-prelude.mjs"
import {digestHex, hexOf, hexCat, toBN, BNtoHex} from "./common-runtime.mjs"

export const equal = (x, y) => toBN(x).equal(y)
export const bytes_eq = (x, y) => hexOf(x) === hexOf(y)
export const assert = x => throw_if_false(x)
export const uint256_to_bytes = x => BNtoHex(x, 32)
export const keccak256 = (...x) => digestHex(hexOf(...x))

export const bytes_len = hex => hex.length / 2;
export const bytes_cat = (a, b) => hexCat(intToHex(bytes_len(a), 2), a, b)

// Used by both msgCar and msgCdr to see when the left stops and the right starts
// 16 bits = 2 bytes = 4 hex characters
export const bytes_left_length = hex => hexToInt(hex.substring(0,4))

export const bytes_left = hex => hex.substring(4, 4 + 2*bytes_left_length(hex))
export const bytes_right = hex => hex.substring(4 + 2*bytes_left_length(hex))

export const random_uint256 = () => toBN(bytesTo0x(random32Bytes()));
