"use strict";

import('./monkey-patch-require')
  .then(() => import("./rps-demo-test.mjs"));
