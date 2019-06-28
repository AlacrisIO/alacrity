/** Prelude for running the Alacrity runtime on nodejs

 Usage:
 * To use this prelude, define all your code using ES6 modules.
 * Your modules will transitively depend on our common-prelude via, e.g.:
       import {require} from "./common-prelude.mjs"
 * See the documentation in common-prelude.
 * You will have a toplevel ES6 module call say "mycode.mjs".
 * Your toplevel nodejs file will just do the following:
       require("node-prelude.js")
       const foo = import("./mycode.mjs");

*/

// Magically make `require` available through a global object on nodejs,
// so that ES6 modules that work on the browser and therefore
// cannot `import` anything that's not on the browser
// may still have access to a fallback to dynamically load dependencies
// when running on nodejs.
// This side-effect to a global object will be used by common-prelude.mjs
process.require = require;
