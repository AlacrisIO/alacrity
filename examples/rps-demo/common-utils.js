/** General-Purpose Utilities for JavaScript programs, by Alacris */
'use strict';

// Combinators for regular functions
/** : 'a => 'a */
const identity = x => x;

/** : 'a => ...'b => 'a */
const konstant = x => (...y) => x;

/** : (...'z => 'y => 'x) => (...'z => 'y) => ...'z => 'x */
const smelt = x => y => (...z) => x(...z)(y(...z));

/** : (...'z => ...'y => 'x) => ...'y => ...'z => 'x */
const flip = x => (...y) => (...z) => x(...z)(...y);

/** : ('y => 'z) => (...'x => 'y) => ...'x => 'z */
const kompose = f => g => (...x) => f(g(...x));

/** : (...'x => 'y) => ('y => 'z) => ...'x => 'z */
const seq = f => g => (...x) => g(f(...x));

const compose = (...fa) => {
    const l = fa.length;
    if (l == 0) { return identity; }
    else if (l == 1) { return fa[0]; }
    else if (l == 2) { return kompose(fa[0])(fa[1]); }
    else { const f = l.pop(); return kompose(compose(...fa))(f);};}

// Combinators for CPS functions
// type Not(...'a) = forall('result) ...'a => 'result
// type Kont(...'a) = Not(Not(...'a))
/* See my relevant tweets at https://twitter.com/Ngnghm/status/1125831388996014080
If you understand Continuation Passing Style, you're not in Callback Hell anymore,
just in yet another Overly-Low-Level-Syntax Purgatorium
— with a bit more scorn towards metaprogramming deniers
— and your automatic indentation mode disabled.

OK, it *is* somewhat hellish when all the APIs in a programming language are
in CPS, but the implementers purposefully refuse to support proper tail calls.
Now you're going to leak stack space, and/or implement your own trampolining
system and adapters. Damn Continuation deniers!

Interestingly, to reliably bridge CPS functions into the trampoline,
you have the initial/final continuation register a trampoline function.
But continuations are not linear and may return more than once,
so your register is a deque and you get green threading for free.
*/

/** call a direct function from CPS
    Kont.arrow
    : (...'a => 'b) => ...'a => Kont('b) */
const arrowK = f => (...x) => k => k(f(...x));

/** Kont.pure
    : ...'a => Kont(...'a) */
const identityK = (...x) => k => k(...x);

/** : ...'a => ...'b => Kont(...'a) */
const konstantK = (...x) => (...y) => k => k(...x);

/** : (...'z => 'y => Kont('x)) => (...'z => Kont('y)) => ...'z => Kont('x) */
const smeltK = x => y => (...z) => k => x(...z)(xz => y(...z)(seq(xz)(k)));

/** : (...'z => ...'y => Kont('x)) => ...'y => ...'z => Kont('x) */
const flipK = x => (...y) => (...z) => k => x(...z)(xz => xz(...y)(k));

/** : (...'y => Kont(...'z)) => (...'x => Kont(...'y)) => ...'x => Kont(...'z) */
const komposeK = f => g => (...x) => k => g(...x)((...y) => f(...y)(k));

/** : (...'x => Kont(...'y)) => (...'y => Kont(...'z)) => ...'x => Kont(...'z) */
const seqK = f => g => komposeK(g)(f);

const composeK = (...fa) => {
    const l = fa.length;
    if (l == 0) { return identityK; }
    else if (l == 1) { return fa[0]; }
    else if (l == 2) { return komposeK(fa[0])(fa[1]); }
    else { const f = l.pop(); return komposeK(composeK(...fa))(f);};}

/** : Uint8 => string */
const byteToHex = byte => ('0' + (byte & 0xFF).toString(16)).slice(-2);

/** : Uint8Array => string */
const bytesToHex = bytes => Array.from(bytes, byteToHex).join('');

/** : string => String0x */
const hexTo0x = hex => "0x" + hex;

/** : Uint8Array => String0x */
const bytesTo0x = bytes => hexTo0x(bytesToHex(bytes));

/** Strip the 0x prefix
    : String0x => string */
const un0x = s => {
    console.assert(s.slice(0,2) == "0x");
    return s.slice(2);}

/** Prepend "0x" to a hex string
    : string => String0x */
const hexToAddress = hex => hexTo0x(hex.slice(-40));

/** Parse a decimal number */
const parseDecimal = x => parseInt(x, 10);
const stringToInt = parseDecimal

/** Any object to a string */
const anyToString = x => `${x}`
const intToString = anyToString

/** Return a random salt
    : () => String0x */
const randomSalt = () => {
    const array = new Uint8Array(32);
    window.crypto.getRandomValues(array);
    return bytesTo0x(array);}

/** Create an array containing the integers from start to start + length - 1 (included).
   : (int, int) => Array */
const range = (start, length) => Array.from({length}, (_, i) => start + i);

/** : (Array(...'a), 'b) => Array(...'a, 'b) */
const snoc = (l, e) => [...l, e];

/** : Object => bool */
const isEmpty = obj => {
    for(var key in obj) { return false; }
    return true;}
const popEntry = (table, key) => {
    const value = table[key];
    delete table[key];
    return value;}
const merge = o1 => o2 => ({...o2, ...o1});

/** : ...'a => ...'b => () */
const logging = (...prefix) => (...result) =>
      console.log(...prefix, ...result.map(JSON.stringify));

const loggedAlert = (...message) => {
    console.log(...message); alert(message.pop()); }

/** : ...'a => ...'b => Kont(...'b) */
const loggingK = (...prefix) => (...result) => k =>
      {logging(...prefix)(...result); return k(...result);}

/** : 'error => Kont() */
const logErrorK = (...error) => k => loggingK("error:", ...error)()(k);

/** : Not('result) */
const kLogResult = (...result) => loggingK("result:", ...result)()(identity);

/** : Not('error) */
const kLogError = (...error) => logErrorK(...error)(identity);

// type KontE('a) = (Not('a), Not('error)) => 'bottom
/** : (Not('result), Not('error)) => Not('error, 'result) */
const handlerK = (kSuccess = kLogResult, kError = kLogError) => (error, result) =>
    error ? kError(error) : kSuccess(result);

/** : ('result => Kont(...'a), 'success => Kont(...'a)) => Not(...'a) => Not('error, 'result) */
const handlerThenK = (successK = identityK, errorK = logErrorK) => k =>
      handlerK(result => successK(result)(k), error => errorK(error)(k));

/** Run a node-style CPS function with an "error-first" callback
    made from a success continuation and an error continuation.
    : (...'a => Not('error, 'success)) => ...'a => Not(Not('success), Not('error)) */
const errbacK = func => (...args) => (kSuccess = kLogResult, kError = kLogError) =>
      func(...args, handlerK(kSuccess, kError));

/** : ('a => Kont()) => Array('a) => Kont() */
const forEachK = f => l => k => {
    let ll = [...l]; // copy it
    const loop = () => {
        if (ll.length == 0) {
            return k();
        } else if (ll.length == 1) {
            return f(ll[0])(k);
        } else {
            return f(ll.shift())(loop);
        }}
    return loop(); }

/** : ((string, ...'a), (string, ...'b) ) => int */
const compareFirst = (a, b) => a[0].localeCompare(b[0]);

/** : StringTable(Not(...'a)) => ...'a => Kont() */
const runHooks = hooks => (...args) => k =>
    forEachK(entry => entry[1](...args))(Object.entries(hooks).sort(compareFirst))(k);

const inDependencyOrder = (act, dependencyOrder, activityName) => k => {
    const issued = {};
    const current = {};
    const handleFun = name => k => {
        if (current[name]) {
            throw ["Circular dependency during", activityName, Object.keys(current)];
        } else if (issued[name]) {
            return k();
        } else {
            issued[name] = true;
            current[name] = true;
            const rec = dependencyOrder[name];
            const kk =
                () => {logging(activityName, name)(); return act(name)(
                () => { delete current[name]; return k(); })};
            if (rec.dependsOn) { return handleFuns(rec.dependsOn)(kk) } else { return kk(); };}};
    const handleFuns = funs => forEachK(handleFun)(funs);
    return handleFuns(Object.keys(dependencyOrder))(k);}

// Initialization. TODO: maybe have a dependency graph instead?
var initFunctions = {}; // maps names to an object { dependsOn: [list of dependencies], fun: actual fun }
const registerInit = (init) => initFunctions = {...initFunctions, ...init};
const initialize = () => inDependencyOrder(name => initFunctions[name].fun, initFunctions, "init:")(identity);

// "places", the imperative alternative to lenses.
// type place('a) = { get: () => 'a, set: 'a => () }
const modifyPlace = (place, func) => place.set(func(place.get()));
const valPlace = x => ({ get: () => x, set: y => x = y })
const pathGet = (x, path) => { for (let i in path) { x = x[path[i]] } ; return x }
const pathDo = (x, path, func) =>
      (path.length == 0) ? func(x) :
      func(refPlace(pathGet(place.get(), path.slice(0, -1)), path[path.length-1]))
const pathSet = (place, path, x) => pathDo(place, path, p => p.set(x))
const pathModify = (place, path, func) => pathDo(place, path, p => modifyPlace(p, func))
const pathPlace = (x, path) => ({ get: () => pathGet(x.get(), path), set: v => pathSet(x, path, v) })

// type container('key, 'value) = { get: ('key, 'value) => 'value, set: ('key, 'value) => (), remove: 'key => (), modify: ('key, 'value => 'value) => (), includes: 'key =>  }
const container = c => ({
    includes: k => c.includes(k),
    get: (k, default_ = null) => c.includes(k) ? c[k] : default_,
    set: (k, v) => c[k] = v,
    remove: k => delete c[k],
    modify: (k, f) => c[k] = f(c[k])})

const keyValuePair = (key, value) => { let o = {}; o[key] = value; return o; }
const defaultIfNull = (value, default_) => value === null ? default_ : value

// Local Storage for the DApp
// TODO: use (encrypted) remote storage and implement distributed transactions, for redundancy.
const includesStorage = key => window.localStorage.getItem(key) != null;
const getStorage = (key, default_ = null) =>
      defaultIfNull(JSON.parse(window.localStorage.getItem(key)), default_);
const setStorage = (key, value) => window.localStorage.setItem(key, JSON.stringify(value));
const Storage = {
    includes: includesStorage,
    get: getStorage,
    set: setStorage,
    modify: (key, func) => setStorage(key, func(getStorage(key))),
    remove: key => window.localStorage.removeItem(key)};

const rekeyContainer = (container, rekey) => ({
    includes: key => container.includes(rekey(key)),
    get: (key, default_) => container.get(rekey(key), default_),
    set: (key, value) => container.set(rekey(key), value),
    modify: (key, func) => container.modify(rekey(key), func),
    remove: key => container.remove(rekey(key))})

/** For Apps with a "current user" that may change, making keys relative to a userId.
TODO: decide a good naming policy wrt delete and remove.
Also we need to have a convention for the contents of an interaction:
creation:

TODO: maybe use leveldb via web3.db to get some atomicity?
Although that seems useless unless and until we have them expose more of leveldb.
*/
/** : string */
let userId;
const userKey = key => `${userId}.${key}`;
const userStorage = rekeyContainer(Storage, userKey);
const updateUserStorage = (key, fields) => userStorage.modify(key, merge(fields));
const setUserStorageField = (key, field, value) => updateUserStorage(key, keyValuePair(field, value));
const deleteUserStorageField = (key, field) => {
    const record = userStorage.get(key);
    delete record[field];
    userStorage.set(key, record);
    return record;}

/** checkRequirement checks that some boolean is true, and if not throws an error
    based on the result of a thunk. It s thus kind of like require in solidity,
    but with the much more useful message-producing thunk. */
const checkRequirement = (bool, msg) => {
    if (!bool) { throw ["Requirement failed", msg()] }}

/**
   TODO: use web3.shh for messaging between players for simple state channels?
   */

/** Debugging stuff */
let r;
const setr = result => {r = result; logging("result:")(r); return r;};
const setrr = seq(Array.of)(setr);
const setrk = result => k => k(setr(result));
const setrrk = seq(Array.of)(setrk);
const srf = func => {r = undefined; return func(setr);}
const srrf = func => {r = undefined; return func(setrr);}
