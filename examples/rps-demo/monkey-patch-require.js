"use strict";

// Magically make `require` available through a global object on nodejs,
// so that ES6 modules that work on the browser and therefore
// cannot `import` anything that's not on the browser
// may still have access to a fallback to dynamically load dependencies
// when running on nodejs.
process.require = require;
