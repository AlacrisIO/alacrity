/** Compatibility layer to run on either browser or nodejs

   Usage:
   * Write all your code using ES6 modules so it will run as is on the browser
   * To define compatibility with nodejs, use in some base module
       import {isInBrowser, isInNode, require, loggedAlert, registerGlobals} from "./common-prelude.mjs"
   * To use on nodejs, see file node-prelude.js
   * Define conditional code with isInBrowser, isInNode, require.
   * For help with debugging on a browser, register your debuggable modules with registerGlobals.
*/

/* eslint-disable no-alert, no-console */

/** Is the code currently running in a browser ?

   : bool
*/
export const isInBrowser = typeof window === "object" && window !== null

/** Is the code currently running in nodejs ?

  : bool
*/
export const isInNode = !isInBrowser

/** Require a module if running on nodejs, log an error if running in the browser

  Usage:
    if (isInNode) { require("foo") }

  : String => Object
*/
export let require;

/** Log a message on the console. Additionally, issue an alert if on the browser.

  : (...Any) => ()
 */
export const loggedAlert = (...message) => {
    console.log(...message); if (isInBrowser) { alert(message.join(" ")) }}

/** A set of global modules, the contents of which can be made available at toplevel for debugging

  Usage: register new bindings with registerGlobals(), make them available with magic()

  : Object
*/
export const globals = {}

/** Register a module to be added to the globals.

  Usage:
     import * as foo from "./foo.mjs"
     import * as bar from "./bar.mjs"
     registerGlobals({foo, bar})

  : Object => ()
*/
export const registerGlobals = x => Object.assign(globals, x);


/** Create a string you can eval() at toplevel to access all registered global bindings

  Usage (at the F12 JavaScript console on the browser):
     eval(magic())

  Usage (at the nodejs console):
     eval(process.magic())

  : () => String
*/
export const magic = () =>
   `var globals = ${isInBrowser ? "window" : "process"}.globals;
   ${Object.keys(globals).map(m=>Object.keys(window.globals[m]).map(s=>`var ${s} = globals.${m}.${s}`).join(";")).join(";")}`

console.log(`To make all the program bindings available in the console, use:
eval(${isInBrowser ? "" : "process."}magic())`);

// const MAGIC = () => eval(magic()) // This does NOT work. You do have to call "eval" *at the toplevel*.

if (isInBrowser) {
    window.globals = globals;
    window.magic = magic;
    require = x => console.error("Cannot require(%s) in a browser", x);
} else {
    process.globals = globals;
    process.magic = magic;
    require = process.require; // cheat the module system; must be set by an outer loader.
}
