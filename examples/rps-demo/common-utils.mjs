/** General-Purpose Utilities for JavaScript programs, by Alacris */
/* eslint-disable no-alert, no-console */

export const isInBrowser = typeof window === "object" && window !== null;
export let require;
export const globals = {}
export const registerGlobals = x => Object.assign(globals, x);

if (isInBrowser) {
    window.globals = globals;
    require = () => loggedAlert("Cannot use require in a browser");
} else {
    process.globals = globals;
    require = process.require; // cheat the module system; must be set by an outer loader.
}


// Combinators for regular functions
/** : 'a => 'a */
export const identity = x => x;

/** : 'a => ...'b => 'a */
export const konstant = x => () => x;

/** : (...'z => 'y => 'x) => (...'z => 'y) => ...'z => 'x */
export const smelt = x => y => (...z) => x(...z)(y(...z));

/** : (...'z => ...'y => 'x) => ...'y => ...'z => 'x */
export const flip = x => (...y) => (...z) => x(...z)(...y);

/** : ('y => 'z) => (...'x => 'y) => ...'x => 'z */
export const kompose = f => g => (...x) => f(g(...x));

/** : (...'x => 'y) => ('y => 'z) => ...'x => 'z */
export const seq = f => g => (...x) => g(f(...x));

export const compose = (...fa) => {
    const l = fa.length;
    if (l == 0) { return identity; }
    else if (l == 1) { return fa[0]; }
    else if (l == 2) { return kompose(fa[0])(fa[1]); }
    else { const f = l.pop(); return kompose(compose(...fa))(f)}}

export const assert = (bool, msg, doAlert) => { if (!bool) {
    const err = msg ? typeof msg == "function" ? msg () : msg : "assertion failed";
    // TODO: do something good on nodejs
    if (doAlert) { loggedAlert(err) } else { logging(err)() }
    throw(err)}}

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
export const arrowK = f => (...x) => k => k(f(...x));

/** Kont.pure
    : ...'a => Kont(...'a) */
export const identityK = (...x) => k => k(...x);

/** : ...'a => ...'b => Kont(...'a) */
export const konstantK = (...x) => () => k => k(...x);

/** : (...'z => 'y => Kont('x)) => (...'z => Kont('y)) => ...'z => Kont('x) */
export const smeltK = x => y => (...z) => k => x(...z)(xz => y(...z)(seq(xz)(k)));

/** : (...'z => ...'y => Kont('x)) => ...'y => ...'z => Kont('x) */
export const flipK = x => (...y) => (...z) => k => x(...z)(xz => xz(...y)(k));

/** : (...'y => Kont(...'z)) => (...'x => Kont(...'y)) => ...'x => Kont(...'z) */
export const komposeK = f => g => (...x) => k => g(...x)((...y) => f(...y)(k));

/** : (...'x => Kont(...'y)) => (...'y => Kont(...'z)) => ...'x => Kont(...'z) */
export const seqK = f => g => komposeK(g)(f);

export const composeK = (...fa) => {
    const l = fa.length;
    if (l == 0) { return identityK; }
    else if (l == 1) { return fa[0]; }
    else if (l == 2) { return komposeK(fa[0])(fa[1]); }
    else { const f = l.pop(); return komposeK(composeK(...fa))(f)}}

/** : Uint8 => string */
export const byteToHex = byte => ('0' + (byte & 0xFF).toString(16)).slice(-2);

/** : Uint8Array => string */
export const bytesToHex = bytes => Array.from(bytes, byteToHex).join('');

/** : string => String0x */
export const hexTo0x = hex => "0x" + hex;

/** : Uint8Array => String0x */
export const bytesTo0x = bytes => hexTo0x(bytesToHex(bytes));

/** Strip the 0x prefix
    : String0x => string */
export const un0x = s => {
    assert(s.slice(0,2) == "0x", () => ["string doesn't start with 0x", s]);
    return s.slice(2);}

/** Prepend "0x" to a hex string
    : string => String0x */
export const hexToAddress = hex => hexTo0x(hex.slice(-40));

/** Parse a decimal number */
export const parseDecimal = x => parseInt(x, 10);
export const stringToInt = parseDecimal

/** Any object to a string */
export const anyToString = x => `${x}`
export const intToString = anyToString

/** Return a random salt
    : () => String0x */
export const randomSalt = () => {
    const array = new Uint8Array(32);
    window.crypto.getRandomValues(array);
    return bytesTo0x(array);}

/** Create an array containing the integers from start to start + length - 1 (included).
   : (int, int) => Array */
export const range = (start, length) => Array.from({length}, (_, i) => start + i);

/** : (Array(...'a), 'b) => Array(...'a, 'b) */
export const snoc = (l, e) => [...l, e];

/** : Object => bool */
export const isEmpty = obj => {
    for(var key in obj) { return false; }
    return true;}
export const popEntry = (table, key) => {
    const value = table[key];
    delete table[key];
    return value;}
export const merge = o1 => o2 => ({...o2, ...o1});

/** : ...'a => ...'b => () */
export const logging = (...prefix) => (...result) => {
    const args = [...prefix, ...result.map(JSON.stringify)];
    if (isInBrowser) { console.log(...args) }
    else { console.log("%s", args.join(" ")); }}

export const loggedAlert = (...message) => {
    logging(...message)(); if (isInBrowser) { alert(message.pop()) }}

/** : ...'a => ...'b => Kont(...'b) */
export const loggingK = (...prefix) => (...result) => k =>
      {logging(...prefix)(...result); return k(...result);}

/** : 'error => Kont() */
export const logErrorK = (...error) => k => loggingK("error:", ...error)()(k);

/** : Not('result) */
export const kLogResult = (...result) => loggingK("result:", ...result)()(identity);

/** : Not('error) */
export const kLogError = (...error) => logErrorK(...error)(identity);

// type KontE('a) = (Not('a), Not('error)) => 'bottom
/** : (Not('result), Not('error)) => Not('error, 'result) */
export const handlerK = (kSuccess = kLogResult, kError = kLogError) => (error, result) =>
    error ? kError(error) : kSuccess(result);

/** : ('result => Kont(...'a), 'success => Kont(...'a)) => Not(...'a) => Not('error, 'result) */
export const handlerThenK = (successK = identityK, errorK = logErrorK) => k =>
      handlerK(result => successK(result)(k), error => errorK(error)(k));

/** Run a node-style CPS function with an "error-first" callback
    made from a success continuation and an error continuation.
    : (...'a => Not('error, 'success)) => ...'a => Not(Not('success), Not('error)) */
export const errbacK = func => (...args) => (kSuccess = kLogResult, kError = kLogError) =>
      func(...args, handlerK(kSuccess, kError));

/** : ('a => Kont()) => Array('a) => Kont() */
export const forEachK = f => l => k => {
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
export const compareFirst = (a, b) => a[0].localeCompare(b[0]);

/** : StringTable(Not(...'a)) => ...'a => Kont() */
export const runHooks = hooks => (...args) => k =>
    forEachK(entry => entry[1](...args))(Object.entries(hooks).sort(compareFirst))(k);

/** walk a data structure in dependency order, achieving a topologically sort */
export const inDependencyOrder = (act, dependencyOrder, activityName, issued = {}) => k => {
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
            if (rec.dependsOn) {return handleFuns(rec.dependsOn)(kk)} else {return kk()}}};
    const handleFuns = funs => forEachK(handleFun)(funs);
    return handleFuns(Object.keys(dependencyOrder))(k);}

// Initialization. TODO: maybe have a dependency graph instead?
const initFunctions = {}; // maps names to an object { dependsOn: [list of dependencies], fun: actual fun }
const initFunctionFunction = name => { const f = initFunctions[name]; return f.fun || f }
export const registerInit = init => Object.assign(initFunctions, init);
export const initialized = {}
export const initialize = () =>
    inDependencyOrder(initFunctionFunction, initFunctions, "init:", initialized)(identity);

// "places", the imperative alternative to lenses.
// type place('a) = { get: () => 'a, set: 'a => () }
export const modifyPlace = (place, func) => place.set(func(place.get()));
export const valPlace = x => ({ get: () => x, set: y => x = y })
export const refPlace = (x, y) => ({ get: () => x[y], set: z => x[y] = z })
export const pathGet = (x, path) => { for (let i in path) { x = x[path[i]] } return x }
export const pathDo = (place, path, func) =>
      (path.length == 0) ? func(place) :
      func(refPlace(pathGet(place.get(), path.slice(0, -1)), path[path.length-1]))
export const pathSet = (place, path, x) => pathDo(place, path, p => p.set(x))
export const pathModify = (place, path, func) => pathDo(place, path, p => modifyPlace(p, func))
export const pathPlace = (x, path) => ({ get: () => pathGet(x.get(), path), set: v => pathSet(x, path, v) })

// type container('key, 'value) = { get: ('key, 'value) => 'value, set: ('key, 'value) => (), remove: 'key => (), modify: ('key, 'value => 'value) => (), includes: 'key =>  }
export const container = c => ({
    includes: k => c.includes(k),
    get: (k, default_ = null) => c.includes(k) ? c[k] : default_,
    set: (k, v) => c[k] = v,
    remove: k => delete c[k],
    modify: (k, f) => c[k] = f(c[k])})

export const rekeyContainer = (container, rekey) => ({
    includes: key => container.includes(rekey(key)),
    get: (key, default_) => container.get(rekey(key), default_),
    set: (key, value) => container.set(rekey(key), value),
    modify: (key, func) => container.modify(rekey(key), func),
    remove: key => container.remove(rekey(key))})

export const keyValuePair = (key, value) => { let o = {}; o[key] = value; return o; }
export const defaultIfNull = (value, default_) => value === null ? default_ : value

/** checkRequirement checks that some boolean is true, and if not throws an error
    based on the result of a thunk. It s thus kind of like require in solidity,
    but with the much more useful message-producing thunk. */
export const checkRequirement = (bool, msg) => {
    if (!bool) { throw ["Requirement failed", msg()] }}

/**
   TODO: use web3.shh for messaging between players for simple state channels?
   */

/** Debugging stuff */
export let r;
export const rr = () => r;
export const setr = result => {r = result; logging("result:")(r); return r;};
export const setrr = seq(Array.of)(setr);
export const setrk = result => k => k(setr(result));
export const setrrk = seq(Array.of)(setrk);
export const srf = func => {r = undefined; return func(setr);}
export const srrf = func => {r = undefined; return func(setrr);}

// Local Variables:
// mode: JavaScript
// End:
