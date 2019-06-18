"use strict";

import('./monkey-patch-require')
  .then(() => import("./rps-test.mjs"));
