// vim: filetype=javascript

import { runGameWith } from './demo.mjs';

const interact = (a, cb) =>
  console.log(a) || cb();

runGameWith(interact)
  .then(() => console.log('Done!'))
  .then(() => process.exit(0))
  .catch(e => console.error(e) || process.exit(1));
