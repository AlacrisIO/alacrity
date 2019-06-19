// TODO: localStorage is not robust, so use some transactional alternative, such as IndexedDB,
// or libraries that abstract over it (e.g. PouchDB):
// See https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API

import {require} from './common-prelude.mjs';
import {defaultIfNull} from './common-utils.mjs';

let _localStorage;
if (typeof localStorage === "object") {
  _localStorage = localStorage;
} else {
  var LocalStorage = require('node-localstorage').LocalStorage;
  _localStorage = new LocalStorage('./scratch');
}

// Local Storage for the DApp
// TODO: use (encrypted) remote storage and implement distributed transactions, for redundancy.
export const includesStorage = key => _localStorage.getItem(key) != null;
export const getStorage = (key, default_ = null) =>
      defaultIfNull(JSON.parse(_localStorage.getItem(key)), default_);
export const setStorage = (key, value) => _localStorage.setItem(key, JSON.stringify(value));
export const Storage = {
    includes: includesStorage,
    get: getStorage,
    set: setStorage,
    modify: (key, func) => setStorage(key, func(getStorage(key))),
    remove: key => _localStorage.removeItem(key)};
