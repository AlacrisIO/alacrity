import {registerGlobals} from "./common-prelude.mjs";
import {init} from "./common-utils.mjs";

import * as runtime from "./alacrity-runtime.mjs";
import * as backend from "./backend-manual.mjs";
import * as frontend from "./rps-frontend.mjs";
import * as testing from "./testing.mjs";

registerGlobals({runtime, backend, frontend, testing})

init()()
