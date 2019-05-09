// General-Purpose Utilities
'use strict';

// Combinators for regular functions
/** : 'a => 'a */
const identity = (x) => x;

/** : 'a => ...'b => 'a */
const konstant = (x) => (...y) => x;

/** : (...'z => 'y => 'x) => (...'z => 'y) => ...'z => 'x */
const smelt = (x) => (y) => (...z) => x(...z)(y(...z));

/** : (...'z => ...'y => 'x) => ...'y => ...'z => 'x */
const flip = (x) => (...y) => (...z) => x(...z)(...y);

/** : ('y => 'z) => (...'x => 'y) => ...'x => 'z */
const kompose = (f) => (g) => (...x) => f(g(...x));

/** : (...'x => 'y) => ('y => 'z) => ...'x => 'z */
const seq = (f) => (g) => (...x) => g(f(...x));

const compose = (...fa) => {
    const l = fa.length;
    if (l == 0) { return identity; }
    else if (l == 1) { return fa[0]; }
    else if (l == 2) { return kompose(fa[0])(fa[1]); }
    else { const f = l.pop(); return kompose(compose(...fa))(f);};}

// UNTESTED! Combinators for CPS functions
// type Not(...'a) = (...'a) => 'r
// type Kont(...'a) = Not(Not(...'a))

/** call a direct function from CPS
    Kont.arrow
    : (...'a => 'b) => ...'a => Kont('b) */
const arrowK = (f) => (...x) => (k) => k(f(...x));

/** Kont.pure
    : ...'a => Kont(...'a) */
const identityK = (...x) => (k) => k(...x);

/** : ...'a => ...'b => Kont(...'a) */
const konstantK = (...x) => (...y) => (k) => k(...x);

/** : (...'z => 'y => Kont('x)) => (...'z => Kont('y)) => ...'z => Kont('x) */
const smeltK = (x) => (y) => (...z) => (k) => x(...z)((xz) => y(...z)(seq(xz)(k)));

/** : (...'z => ...'y => Kont('x)) => ...'y => ...'z => Kont('x) */
const flipK = (x) => (...y) => (...z) => (k) => x(...z)((xz) => xz(...y)(k));

/** : (...'y => Kont(...'z)) => (...'x => Kont(...'y)) => ...'x => Kont(...'z) */
const komposeK = (f) => (g) => (...x) => (k) => g(...x)((...y) => f(...y)(k));

/** : (...'x => Kont(...'y)) => (...'y => Kont(...'z)) => ...'x => Kont(...'z) */
const seqK = (f) => (g) => komposeK(g)(f);

const composeK = (...fa) => {
    const l = fa.length;
    if (l == 0) { return identityK; }
    else if (l == 1) { return fa[0]; }
    else if (l == 2) { return komposeK(fa[0])(fa[1]); }
    else { const f = l.pop(); return komposeK(composeK(...fa))(f);};}


/** : Uint8 => string */
const byteToHex = (byte) => ('0' + (byte & 0xFF).toString(16)).slice(-2);

/** : Uint8Array => string */
const bytesToHex = (bytes) => Array.from(bytes, byteToHex).join('');

/** : string => String0x */
const hexTo0x = (hex) => "0x" + hex;

/** : Uint8Array => String0x */
const bytesTo0x = (bytes) => hexTo0x(bytesToHex(bytes));

/** Strip the 0x prefix
    : String0x => string */
const un0x = (s) => {
    console.assert(s.slice(0,2) == "0x");
    return s.slice(2);}

/** Prepend "0x" to a hex string
    : string => String0x */
const hexToAddress = (hex) => hexTo0x(hex.slice(-40));

/** Convert a hex string to a BigNumber
    : string => BigNumber */

/** Return a random salt
    : () => String0x */
const randomSalt = () => {
    const array = new Uint8Array(32);
    window.crypto.getRandomValues(array);
    return bytesTo0x(array);}

/** : (Array('a), 'a) => Array('a) */
const snoc = (l, e) => [...l, e];

const logErrorK = (error) => (k) => {console.log("error: ", error); return k();}

const handlerK = (successK = identityK, errorK = logErrorK) => (error, result) =>
      error ? errorK(error) : successK(result);

const handlerThenK = (successK = identityK, errorK = logErrorK) => (k) =>
      handlerK(seqK(successK, k), seqK(errorK, k));

/** : ('a => Kont()) => Array('a) => Kont() */
const forEachK = (f) => (l) => (k) => {
    const loop = () => {
        if (l.length == 0) {
            return k();
        } else if (l.length == 1) {
            return f(l[0])(k);
        } else {
            return f(l.shift())(loop);
        };};
    return loop(); }

const compareFirst = (a, b) => a[0].localeCompare(b[0]);

const log = (result) => {console.log("logging: ", JSON.stringify(result)); return result;};

// Local Storage for the DApp
// TODO: use remote storage and implement distributed transactions, for redundancy.
const keyValuePair = (key, value) => { let o = {}; o[key] = value; return o; }
const getStorage = (key, default_ = null) => JSON.parse(window.localStorage.getItem(key)) || default_;
const putStorage = (key, value) => window.localStorage.setItem(key, JSON.stringify(value));
const updateStorage = (key, update) => putStorage(key, {...getStorage(key), ...update});

// For Apps with a "current user" that may change, making keys relative to a userID.
/** : string */
let userID;
const userKey = (key) => `${userID}.${key}`;
const getUserStorage = (key, default_ = null) => getStorage(userKey(key), default_);
const putUserStorage = (key, value) => putStorage(userKey(key), value);
const updateUserStorage = (key, update) => updateStorage(userKey(key), update);
const putUserStorageField = (key, field, value) => updateUserStorage(key, keyValuePair(field, value));


/** Debugging stuff */
let r;
const setr = (result) => {r = result; return log(r); };
const setrr = seq(Array.of)(setr);
const setrk = (result) => (k) => k(setr(result));
const setrrk = seq(Array.of)(setrk);
const srf = (func) => {r = undefined; return func(setr);}
const srrf = seqK(Array.of)(srf);
